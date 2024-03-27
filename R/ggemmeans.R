#' @rdname ggpredict
#' @export
ggemmeans <- function(model,
                      terms,
                      ci_level = 0.95,
                      type = "fixed",
                      typical = "mean",
                      condition = NULL,
                      back_transform = TRUE,
                      interval = "confidence",
                      verbose = TRUE,
                      ci.lvl = ci_level,
                      back.transform = back_transform,
                      ...) {
  insight::check_if_installed("emmeans")
  # check arguments
  type <- match.arg(
    type,
    choices = c("fe", "fixed", "count", "re", "random", "fe.zi", "zero_inflated",
                "re.zi", "zi_random", "zero_inflated_random", "zi.prob", "zi_prob")
  )
  interval <- match.arg(interval, choices = c("confidence", "prediction"))
  model_name <- deparse(substitute(model))

  type <- switch(
    type,
    fixed = ,
    count = "fe",
    random = "re",
    zi = ,
    zero_inflated = "fe.zi",
    zi_random = ,
    zero_inflated_random = "re.zi",
    zi_prob = "zi.prob",
    survival = "surv",
    cumulative_hazard = "cumhaz",
    type
  )

  ## TODO: add warnings later

  # handle deprectated arguments
  if (!missing(ci.lvl)) {
    ci_level <- ci.lvl
  }
  if (!missing(back.transform)) {
    back_transform <- back.transform
  }

  # process "terms", so we have the default character format. Furthermore,
  # check terms argument, to make sure that terms were not misspelled and are
  # indeed existing in the data
  if (!missing(terms)) {
    terms <- .reconstruct_focal_terms(terms, model)
  }

  # tidymodels?
  if (inherits(model, "model_fit")) {
    model <- model$fit
  }

  if (inherits(model, "MixMod") && type == "zi.prob") {
    insight::format_error(sprintf(
      "This prediction-type is currently not available for models of class '%s'.", class(model)[1]
    ))
  }

  # for gamm/gamm4 objects, we have a list with two items, mer and gam
  # extract just the mer-part then
  if (is.gamm(model) || is.gamm4(model)) model <- model$gam

  # check model family, do we have count model?
  model_info <- .get_model_info(model)

  # get model frame
  model_frame <- .get_model_data(model)
  original_model_frame <- model_frame

  # clean "terms" from possible brackets
  cleaned_terms <- .clean_terms(terms)

  data_grid <- .data_grid(
    model = model, model_frame = model_frame, terms = terms, value_adjustment = typical,
    condition = condition, emmeans.only = TRUE, show_pretty_message = verbose
  )


  # for zero-inflated mixed models, we need some extra handling

  if (!is.null(model_info) && model_info$is_zero_inflated && inherits(model, c("glmmTMB", "MixMod")) && type == "fe.zi") { # nolint

    preds <- .emmeans_mixed_zi(model, data_grid, cleaned_terms, ...)
    additional_dot_args <- match.call(expand.dots = FALSE)[["..."]]

    if ("nsim" %in% names(additional_dot_args)) {
      nsim <- eval(additional_dot_args[["nsim"]])
    } else {
      nsim <- 1000
    }

    prediction_data <- .ggemmeans_zi_predictions(
      model = model,
      model_frame = model_frame,
      preds = preds,
      ci.lvl = ci_level,
      terms = terms,
      cleaned_terms = cleaned_terms,
      value_adjustment = typical,
      condition = condition,
      nsim = nsim,
      type = type
    )
    pmode <- "response"

  } else if (!is.null(model_info) && model_info$is_zero_inflated && inherits(model, "glmmTMB") && type == "zi.prob") { # nolint

    # .emmeans_mixed_zi() returns a list with two items, the first one is the
    # emmeans object for the conditional component, the second one is the
    # zero-inflated part
    preds <- .emmeans_mixed_zi(model, data_grid, cleaned_terms, ci.lvl, ...)
    prediction_data <- data.frame(
      predicted = stats::plogis(preds$x2$emmean),
      std.error = preds$x2$SE,
      conf.low = stats::plogis(preds$x2$asymp.LCL),
      conf.high = stats::plogis(preds$x2$asymp.UCL)
    )

  } else {

    # special handling for rqs
    if (inherits(model, "rqs") && !is.null(model$tau) && length(model$tau) > 1 && !"tau" %in% cleaned_terms) {
      cleaned_terms <- c(cleaned_terms, "tau")
    }

    # get prediction mode, i.e. at which scale predicted
    # values should be returned
    pmode <- .get_prediction_mode_argument(model, model_info, type)
    prediction_data <- .emmeans_prediction_data(
      model,
      data_grid,
      cleaned_terms,
      ci.lvl = ci_level,
      pmode,
      type,
      model_info,
      interval = interval,
      verbose = verbose,
      ...
    )

    # fix gam here
    if (inherits(model, "gam") && isTRUE(model_info$is_zero_inflated)) {
      prediction_data$predicted <- exp(prediction_data$predicted)
      prediction_data$conf.low <- exp(prediction_data$conf.low)
      prediction_data$conf.high <- exp(prediction_data$conf.high)
    }
  }

  # return NULL on error
  if (is.null(prediction_data)) return(NULL)

  attr(prediction_data, "continuous.group") <- attr(data_grid, "continuous.group")

  if (!is.null(model_info) &&
       (model_info$is_ordinal || model_info$is_categorical || model_info$is_multinomial) &&
       colnames(prediction_data)[1] != "x") {
    colnames(prediction_data)[1] <- "response.level"
  }

  result <- .post_processing_predictions(
    model = model,
    prediction_data = prediction_data,
    original_model_frame = original_model_frame,
    cleaned_terms = cleaned_terms
  )

  # apply link inverse function
  linv <- insight::link_inverse(model)
  if (!is.null(linv) && (inherits(model, c("lrm", "orm")) || pmode == "link" || (inherits(model, "MixMod") && type != "fe.zi"))) { # nolint
    result$predicted <- linv(result$predicted)
    result$conf.low <- linv(result$conf.low)
    result$conf.high <- linv(result$conf.high)
  }

  # check if outcome is log-transformed, and if so,
  # back-transform predicted values to response scale
  # but first, save original predicted values, to save as attribute
  if (back_transform) {
    untransformed.predictions <- result$predicted
    response.transform <- insight::find_terms(model)[["response"]]
  } else {
    untransformed.predictions <- response.transform <- NULL
  }
  result <- .back_transform_response(model, result, back_transform, verbose = verbose)

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
    at_list = data_grid,
    condition = condition,
    ci.lvl = ci_level,
    untransformed.predictions = untransformed.predictions,
    back.transform = back_transform,
    response.transform = response.transform,
    verbose = verbose
  )
}


.get_prediction_mode_argument <- function(model, model_info, type) {
  if (inherits(model, "betareg")) {
    "response"
  } else if (inherits(model, c("polr", "clm", "clmm", "clm2", "rms", "lrm", "orm"))) {
    "prob"
  } else if (inherits(model, "lmerMod")) {
    "asymptotic"
  } else if (inherits(model, "MixMod")) {
    "fixed-effects"
  } else if (inherits(model, c("gls", "lme"))) {
    "auto"
  } else if (inherits(model, "MCMCglmm") && isTRUE(model_info$is_multinomial)) {
    "response"
  } else if (!is.null(model_info) && (model_info$is_ordinal || model_info$is_categorical || model_info$is_multinomial)) { # nolint
    "prob"
  } else if (isTRUE(model_info$is_zero_inflated) && type %in% c("fe", "re") && inherits(model, "glmmTMB")) {
    "link"
  } else if (isTRUE(model_info$is_zero_inflated) && type %in% c("fe.zi", "re.zi")) {
    "response"
  } else if (isTRUE(model_info$is_zero_inflated) && type %in% c("fe", "re")) {
    "count"
  } else if (isTRUE(model_info$is_zero_inflated) && type == "zi.prob") {
    "prob0"
  } else {
    "link"
  }
}
