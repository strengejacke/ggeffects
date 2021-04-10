get_predictions_geeglm <- function(model, fitfram, ci.lvl, type, model_class, value_adjustment, terms, condition, ...) {
  se <- (!is.null(ci.lvl) && !is.na(ci.lvl))

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
      vcov.fun = NULL,
      vcov.type = NULL,
      vcov.args = NULL,
      condition = condition,
      interval = NULL
    )


  if (.check_returned_se(se.pred) && isTRUE(se)) {
    se.fit <- se.pred$se.fit
    fitfram <- se.pred$prediction_data

    # CI
    fitfram$conf.low <- fitfram$predicted - stats::qnorm(ci) * se.fit
    fitfram$conf.high <- fitfram$predicted + stats::qnorm(ci) * se.fit

    # copy standard errors
    attr(fitfram, "std.error") <- se.fit
    attr(fitfram, "prediction.interval") <- attr(se.pred, "prediction_interval")
  } else {
    # CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}
