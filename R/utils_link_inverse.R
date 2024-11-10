# internal to return possibly bias correct link-function
.link_inverse <- function(model = NULL, bias_correction = FALSE, residual_variance = NULL, ...) {
  if (bias_correction) {
    dots <- list(...)
    if (!is.null(dots$sigma) && !is.na(dots$sigma)) {
      residual_variance <- dots$sigma^2
    }
    l <- .bias_correction(model, residual_variance)$linkinv
    if (is.null(l)) {
      l <- insight::link_inverse(model)
    }
  } else {
    l <- insight::link_inverse(model)
  }
  l
}


# apply bias-correction for back-transformation of predictions on the link-scale
# we want sigma^2 (residual_variance) here to calculate the correction
.bias_correction <- function(model = NULL, residual_variance = NULL) {
  # we need a model object
  if (is.null(model)) {
    return(NULL)
  }
  # extract residual variance, if not provided
  if (is.null(residual_variance)) {
    if (insight::is_mixed_model(model)) {
      residual_variance <- .safe(insight::get_variance_residual(model))
    } else {
      residual_variance <- .get_residual_variance(model) # returns sigma^2
    }
  }
  # we need residual variance
  if (is.null(residual_variance)) {
    return(NULL)
  }

  # extract current link function
  link <- .safe(insight::get_family(model))
  # we need a link function
  if (is.null(link)) {
    return(NULL)
  }

  link$inv <- link$linkinv
  link$der <- link$mu.eta
  link$residual_variance <- residual_variance / 2

  link$der2 <- function(eta) {
    with(link, 1000 * (der(eta + 5e-4) - der(eta - 5e-4)))
  }
  link$linkinv <- function(eta) {
    with(link, inv(eta) + residual_variance * der2(eta))
  }
  link$mu.eta <- function(eta) {
    with(link, der(eta) + 1000 * residual_variance * (der2(eta + 5e-4) - der2(eta - 5e-4)))
  }
  link
}


.check_bias_correction <- function(model, type, bias_correction, verbose = TRUE) {
  # exception: sdmTMB. return FALSE
  if (inherits(model, "sdmTMB")) {
    return(FALSE)
  }
  # sanity check - bias correction only for mixed models for now
  if (isTRUE(bias_correction) && !insight::is_mixed_model(model) && !inherits(model, c("gee", "geeglm"))) {
    bias_correction <- FALSE
    if (verbose) {
      insight::format_alert("Bias-correction is currently only supported for mixed or gee models. No bias-correction is applied.") # nolint
    }
  }
  info <- insight::model_info(model)
  # sanity check - multivariate models?
  if (insight::is_multivariate(model)) {
    info <- info[[1]]
  }
  # for GLMMs, when re.form is set to NA and bias_correction = FALSE, warn user
  if (isFALSE(bias_correction) &&
    verbose &&
    isTRUE(getOption("ggeffects_warning_bias_correction", TRUE)) &&
    insight::is_mixed_model(model) &&
    !info$is_linear &&
    !info$is_tweedie &&
    type %in% c("fixed", "zero_inflated")) {
    insight::format_alert(
      "You are calculating adjusted predictions on the population-level (i.e. `type = \"fixed\"`) for a *generalized* linear mixed model.",
      "This may produce biased estimates due to Jensen's inequality. Consider setting `bias_correction = TRUE` to correct for this bias.",
      "See also the documentation of the `bias_correction` argument."
    )
    options(ggeffects_warning_bias_correction = FALSE)
  }
  bias_correction
}
