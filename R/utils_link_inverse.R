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


.glmmTMB_logit_link_inverse <- function(model, model_info, type, sigma) {
  is_logit <- identical(model_info$link_function, "logit")
  if (!identical(model_info$family, "binomial") || !is_logit || isTRUE(model_info$is_zero_inflated)) {
    return(NULL)
  }

  random <- glmmTMB::VarCorr(model)$cond
  if (length(random) != 1L || !identical(rownames(random[[1L]]), "(Intercept)")) {
    return(NULL)
  }

  if (type %in% c("random", "zero_inflated_random")) {
    return(insight::link_inverse(model))
  }
  if (!type %in% c("fixed", "zero_inflated")) {
    return(NULL)
  }
  if (!is.null(sigma) && !is.na(sigma)) {
    return(.link_inverse(model, bias_correction = TRUE, sigma = sigma))
  }

  tau <- sqrt(random[[1L]][1L, 1L])
  log_tail <- function(eta, derivative = FALSE) {
    vapply(-abs(eta), .logit_normal_log_tail, numeric(1), tau = tau, derivative = derivative)
  }
  inverse <- function(eta) {
    result <- stats::plogis(eta)
    finite <- is.finite(eta) & eta != 0
    values <- eta[finite]
    lower <- exp(log_tail(values))
    result[finite] <- ifelse(values > 0, 1 - lower, lower)
    result
  }
  attr(inverse, "marginal_logit") <- function(eta) {
    finite <- is.finite(eta)
    lower <- log_tail(eta[finite])
    upper <- log1p(-exp(lower))
    slope <- rep(1, length(eta))
    slope[finite] <- exp(log_tail(eta[finite], derivative = TRUE) - lower - upper)
    estimate <- eta
    estimate[finite] <- sign(eta[finite]) * (upper - lower)
    list(estimate = estimate, slope = slope)
  }
  inverse
}


.logit_normal_log_tail <- function(eta, tau, derivative) {
  if (tau == 0) {
    if (derivative) return(stats::dlogis(eta, log = TRUE))
    return(stats::plogis(eta, log.p = TRUE))
  }

  # Split at the sigmoid midpoint and tilt the normal on exponential tails.
  # After the tilt, each integral averages a bounded sigmoid over a truncated normal.
  midpoint <- -eta / tau
  cutoff <- c(midpoint - tau, midpoint + if (derivative) tau else 0)
  log_mass <- c(
    stats::pnorm(cutoff[1L], log.p = TRUE),
    stats::pnorm(cutoff[2L], lower.tail = FALSE, log.p = TRUE)
  )
  log_weight <- log_mass + c(eta + tau^2 / 2, if (derivative) -eta + tau^2 / 2 else 0)
  log_scale <- max(log_weight)
  if (is.infinite(log_scale)) return(log_scale)

  mass <- vapply(which(is.finite(log_weight)), function(side) {
    left <- side == 1L
    weight <- stats::integrate(function(u) {
      z <- stats::qnorm(log(u) + log_mass[side], lower.tail = left, log.p = TRUE)
      distance <- abs(z - cutoff[side])
      stats::plogis(tau * distance)^(1L + derivative)
    }, 0, 1, rel.tol = 1e-10)$value
    weight * exp(log_weight[side] - log_scale)
  }, numeric(1))
  result <- log_scale + log(sum(mass))
  if (derivative) result else min(result, log(0.5))
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
