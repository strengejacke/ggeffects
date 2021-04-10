#' @importFrom stats confint na.omit
#' @importFrom insight find_response get_data model_info link_inverse
#' @rdname ggpredict
#' @export
ggemmeans <- function(model,
                      terms,
                      ci.lvl = .95,
                      type = "fe",
                      typical = "mean",
                      condition = NULL,
                      back.transform = TRUE,
                      interval = "confidence",
                      ...) {

  if (!requireNamespace("emmeans")) {
    stop("Package `emmeans` required to compute marginal effects with `ggemmeans()`.", call. = FALSE)
  }

  # check arguments
  type <- match.arg(type, choices = c("fe", "fixed", "count", "re", "random", "fe.zi", "zero_inflated", "re.zi", "zi_random", "zero_inflated_random", "zi.prob", "zi_prob"))
  interval <- match.arg(interval, choices = c("confidence", "prediction"))
  model_name <- deparse(substitute(model))

  type <- switch(
    type,
    "fixed" = ,
    "count" = "fe",
    "random" = "re",
    "zi" = ,
    "zero_inflated" = "fe.zi",
    "zi_random" = ,
    "zero_inflated_random" = "re.zi",
    "zi_prob" = "zi.prob",
    "survival" = "surv",
    "cumulative_hazard" = "cumhaz"    ,
    type
  )

  # check if terms are a formula
  if (!missing(terms) && !is.null(terms) && inherits(terms, "formula")) {
    terms <- all.vars(terms)
  }

  if (inherits(model, c("glmmTMB", "MixMod")) && type == "zi.prob") {
    stop(sprintf("This prediction-type is currently not available for models of class '%s'.", class(model)[1]), call. = FALSE)
  }

  # for gamm/gamm4 objects, we have a list with two items, mer and gam
  # extract just the mer-part then
  if (is.gamm(model) || is.gamm4(model)) model <- model$gam

  # check model family, do we have count model?
  model_info <- .get_model_info(model)

  # get model frame
  original_model_frame <- model_frame <- insight::get_data(model)

  # check terms argument
  terms <- .check_vars(terms, model)
  cleaned_terms <- .clean_terms(terms)

  data_grid <- .data_grid(
    model = model, model_frame = model_frame, terms = terms, value_adjustment = typical,
    condition = condition, emmeans.only = TRUE
  )


  # for zero-inflated mixed models, we need some extra handling

  if (model_info$is_zero_inflated && inherits(model, c("glmmTMB", "MixMod")) && type == "fe.zi") {

    preds <- .emmeans_mixed_zi(model, data_grid, cleaned_terms, ...)
    additional_dot_args <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)

    if ("nsim" %in% names(additional_dot_args))
      nsim <- eval(additional_dot_args[["nsim"]])
    else
      nsim <- 1000

    prediction_data <- .ggemmeans_zi_predictions(
      model = model,
      model_frame = model_frame,
      preds = preds,
      ci.lvl = ci.lvl,
      terms = terms,
      cleaned_terms = cleaned_terms,
      value_adjustment = typical,
      condition = condition,
      nsim = nsim,
      type = type
    )
    pmode <- "response"

  } else {

    # get prediction mode, i.e. at which scale predicted
    # values should be returned
    pmode <- .get_prediction_mode_argument(model, model_info, type)
    prediction_data <- .emmeans_prediction_data(model, data_grid, cleaned_terms, ci.lvl, pmode, type, model_info, interval = interval, ...)

    # fix gam here
    if (inherits(model, "gam") && model_info$is_zero_inflated) {
      prediction_data$predicted <- exp(prediction_data$predicted)
      prediction_data$conf.low <- exp(prediction_data$conf.low)
      prediction_data$conf.high <- exp(prediction_data$conf.high)
    }
  }

  # return NULL on error
  if (is.null(prediction_data)) return(NULL)

  attr(prediction_data, "continuous.group") <- attr(data_grid, "continuous.group")

  if (model_info$is_ordinal || model_info$is_categorical || model_info$is_multinomial) {
    if (colnames(prediction_data)[1] != "x") colnames(prediction_data)[1] <- "response.level"
  }

  result <- .post_processing_predictions(
    model = model,
    prediction_data = prediction_data,
    original_model_frame = original_model_frame,
    cleaned_terms = cleaned_terms
  )

  # apply link inverse function
  linv <- insight::link_inverse(model)
  if (!is.null(linv) && (inherits(model, c("lrm", "orm")) || pmode == "link" || (inherits(model, "MixMod") && type != "fe.zi"))) {
    result$predicted <- linv(result$predicted)
    result$conf.low <- linv(result$conf.low)
    result$conf.high <- linv(result$conf.high)
  }

  # check if outcome is log-transformed, and if so,
  # back-transform predicted values to response scale
  result <- .back_transform_response(model, result, back.transform)

  attr(result, "model.name") <- model_name

  # add raw data as well
  attr(result, "rawdata") <- .get_raw_data(model, original_model_frame, cleaned_terms)

  .post_processing_labels(
    model = model,
    result = result,
    original_model_frame = original_model_frame,
    data_grid = data_grid,
    cleaned_terms = cleaned_terms,
    original_terms = terms,
    model_info = model_info,
    type = type,
    prediction.interval = attr(prediction_data, "prediction.interval", exact = TRUE),
    at_list = .data_grid(
      model = model, model_frame = original_model_frame, terms = terms, value_adjustment = typical,
      condition = condition, show_pretty_message = FALSE, emmeans.only = TRUE
    ),
    ci.lvl = ci.lvl
  )
}


.get_prediction_mode_argument <- function(model, model_info, type) {
  if (inherits(model, "betareg"))
    "response"
  else if (inherits(model, c("polr", "clm", "clmm", "clm2", "rms", "lrm", "orm")))
    "prob"
  else if (inherits(model, "lmerMod"))
    "asymptotic"
  else if (inherits(model, "MixMod"))
    "fixed-effects"
  else if (inherits(model, c("gls", "lme")))
    "auto"
  else if (inherits(model, "MCMCglmm") && model_info$is_multinomial)
    "response"
  else if (model_info$is_ordinal || model_info$is_categorical || model_info$is_multinomial)
    "prob"
  else if (model_info$is_zero_inflated && type %in% c("fe", "re") && inherits(model, "glmmTMB"))
    "link"
  else if (model_info$is_zero_inflated && type %in% c("fe.zi", "re.zi"))
    "response"
  else if (model_info$is_zero_inflated && type %in% c("fe", "re"))
    "count"
  else if (model_info$is_zero_inflated && type %in% c("zi.prob"))
    "prob0"
  else
    "link"
}
