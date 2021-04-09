#' @importFrom stats qnorm predict
get_predictions_generic2 <- function(model, fitfram, ci.lvl, linv, type, model_class, value_adjustment, terms, vcov.fun, vcov.type, vcov.args, condition, interval, ...) {
  # get prediction type.
  pt <- switch(
    model_class,
    "betareg" = ,
    "vgam"    = ,
    "feglm"   = ,
    "glmx"    = ,
    "fixest"  = "link",
    "response"
  )

  se <- (!is.null(ci.lvl) && !is.na(ci.lvl)) || !is.null(vcov.fun)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  # get predictions
  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = pt,
      ...
    )

  fitfram$predicted <- as.vector(prdat)


  # get standard errors from variance-covariance matrix
  se.pred <-
    .standard_error_predictions(
      model = model,
      prediction_data = fitfram,
      value_adjustment = value_adjustment,
      type = type,
      terms = terms,
      model_class = model_class,
      vcov.fun = vcov.fun,
      vcov.type = vcov.type,
      vcov.args = vcov.args,
      condition = condition,
      interval = interval
    )


  if (!is.null(se.pred) && isTRUE(se) && length(se.pred) > 0) {
    se.fit <- se.pred$se.fit
    fitfram <- se.pred$prediction_data

    # CI
    fitfram$conf.low <- linv(fitfram$predicted - stats::qnorm(ci) * se.fit)
    fitfram$conf.high <- linv(fitfram$predicted + stats::qnorm(ci) * se.fit)

    # copy standard errors
    attr(fitfram, "std.error") <- se.fit
    attr(fitfram, "prediction.interval") <- attr(se.pred, "prediction_interval")
  } else {
    # CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram$predicted <- linv(fitfram$predicted)


  fitfram
}
