#' @rdname ggpredict
#' @export
ggemmeans <- function(model,
                      terms,
                      ci_level = 0.95,
                      type = "fixed",
                      typical = "mean",
                      condition = NULL,
                      back_transform = TRUE,
                      vcov = NULL,
                      vcov_args = NULL,
                      interval = "confidence",
                      bias_correction = FALSE,
                      verbose = TRUE,
                      ...) {
  insight::check_if_installed("emmeans")
  additional_dot_args <- list(...)

  ## TODO: remove deprecated later
  vcov <- .prepare_vcov_args(vcov, ...)

  # process vcov-arguments
  vcov <- .get_variance_covariance_matrix(
    model,
    vcov,
    vcov_args,
    skip_if_null = TRUE,
    verbose = verbose
  )

  # check arguments
  interval <- match.arg(interval, choices = c("confidence", "prediction"))
  model_name <- deparse(substitute(model))
  type <- .validate_type_argument(model, type, emmeans_call = TRUE)

  # check if bias-correction is appropriate
  bias_correction <- .check_bias_correction(
    model,
    type = "fixed",
    bias_correction = bias_correction,
    verbose = verbose
  )

  # check if sigma is provided when `bias_correction = TRUE`, else use default
  if (isTRUE(bias_correction) && is.null(additional_dot_args$sigma)) {
    residual_variance <- insight::get_variance_residual(model)
  } else {
    residual_variance <- NULL
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

  if (inherits(model, "MixMod") && type == "zi_prob") {
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
    condition = condition, emmeans.only = TRUE, show_pretty_message = verbose,
    verbose = verbose
  )


  # for zero-inflated mixed models, we need some extra handling

  if (!is.null(model_info) && model_info$is_zero_inflated && inherits(model, c("glmmTMB", "MixMod")) && type == "zero_inflated") { # nolint

    # here we go with simulating confidence intervals. ----------
    # point estimates are not simulated                ----------
    # -----------------------------------------------------------

    preds <- .emmeans_mixed_zi(
      model,
      data_grid,
      cleaned_terms,
      bias_correction = bias_correction,
      residual_variance = residual_variance,
      ...
    )

    if ("nsim" %in% names(additional_dot_args)) {
      nsim <- eval(additional_dot_args[["nsim"]])
    } else {
      nsim <- 1000
    }

    prediction_data <- .ggemmeans_zi_predictions(
      model = model,
      model_frame = model_frame,
      preds = preds,
      ci_level = ci_level,
      terms = terms,
      cleaned_terms = cleaned_terms,
      value_adjustment = typical,
      condition = condition,
      nsim = nsim,
      type = type
    )
    pmode <- "response"
  } else if (!is.null(model_info) && model_info$is_zero_inflated && inherits(model, "glmmTMB") && type == "zi_prob") { # nolint

    # here we go zero-inflation probabilities. ----------
    # ---------------------------------------------------

    # .emmeans_mixed_zi() returns a list with two items, the first one is the
    # emmeans object for the conditional component, the second one is the
    # zero-inflated part
    preds <- .emmeans_mixed_zi(
      model,
      data_grid,
      cleaned_terms,
      ci_level,
      bias_correction = bias_correction,
      residual_variance = residual_variance,
      ...
    )

    prediction_data <- data.frame(
      predicted = stats::plogis(preds$x2$emmean),
      std.error = preds$x2$SE,
      conf.low = stats::plogis(preds$x2$asymp.LCL),
      conf.high = stats::plogis(preds$x2$asymp.UCL)
    )
    term_pos <- which(colnames(preds$x2) == "emmean")
    prediction_data <- cbind(preds$x2[1:(term_pos - 1)], prediction_data)
    pmode <- .get_prediction_mode_argument(model, model_info, type)
  } else {
    # here we go with all other prediction-types. ----------
    # ------------------------------------------------------

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
      ci_level = ci_level,
      pmode,
      type,
      model_info,
      interval = interval,
      vcov_info = list(vcov = vcov, vcov_args = vcov_args),
      bias_correction = bias_correction,
      residual_variance = residual_variance,
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
  if (is.null(prediction_data)) {
    return(NULL)
  }

  attr(prediction_data, "continuous.group") <- attr(data_grid, "continuous.group")

  if (!is.null(model_info) &&
    (model_info$is_ordinal || model_info$is_categorical || model_info$is_multinomial) &&
    colnames(prediction_data)[1] != "x") {
    colnames(prediction_data)[1] <- "response.level"
  }

  # apply link inverse function
  linv <- .link_inverse(model, bias_correction = bias_correction, residual_variance = residual_variance, ...)
  if (!is.null(linv) && (inherits(model, c("lrm", "orm")) || pmode == "link" || (inherits(model, "MixMod") && type != "zero_inflated"))) { # nolint
    prediction_data$predicted <- linv(prediction_data$predicted)
    prediction_data$conf.low <- linv(prediction_data$conf.low)
    prediction_data$conf.high <- linv(prediction_data$conf.high)
  }

  result <- .post_processing_predictions(
    model = model,
    prediction_data = prediction_data,
    original_model_frame = original_model_frame,
    cleaned_terms = cleaned_terms
  )

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
  attr(result, "rawdata") <- .back_transform_data(
    model,
    mydf = .get_raw_data(model, original_model_frame, cleaned_terms),
    back_transform = back_transform
  )

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
    ci_level = ci_level,
    untransformed.predictions = untransformed.predictions,
    back_transform = back_transform,
    response.transform = response.transform,
    margin = "marginalmeans",
    vcov_args = vcov,
    bias_correction = bias_correction,
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
  } else if (isTRUE(model_info$is_zero_inflated) && type %in% c("fixed", "random") && inherits(model, "glmmTMB")) {
    "link"
  } else if (isTRUE(model_info$is_zero_inflated) && type %in% c("zero_inflated", "re.zi")) {
    "response"
  } else if (isTRUE(model_info$is_zero_inflated) && type %in% c("fixed", "random")) {
    "count"
  } else if (isTRUE(model_info$is_zero_inflated) && type == "zi_prob") {
    "prob0"
  } else {
    "link"
  }
}
