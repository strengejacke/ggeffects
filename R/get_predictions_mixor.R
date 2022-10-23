get_predictions_mixor <- function(model, fitfram, ci.lvl, linv, value_adjustment, terms, model_class, condition, ...) {

  se <- (!is.null(ci.lvl) && !is.na(ci.lvl))

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- 0.975

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      ...
    )

  prdat <- as.data.frame(prdat$predicted)

  # bind predictions to model frame
  fitfram <- cbind(prdat, fitfram)

  # for proportional ordinal logistic regression (see MASS::polr),
  # we have predicted values for each response category. Hence,
  # gather columns

  fitfram <- .gather(fitfram, names_to = "response.level", values_to = "predicted", colnames(prdat))

  se.pred <-
    .standard_error_predictions(
      model = model,
      prediction_data = fitfram,
      value_adjustment = value_adjustment,
      terms = terms,
      model_class = model_class,
      condition = condition
    )

  if (.check_returned_se(se.pred) && isTRUE(se)) {
    se.fit <- se.pred$se.fit
    fitfram <- se.pred$prediction_data

    # CI
    fitfram$conf.low <- linv(stats::qlogis(fitfram$predicted) - stats::qnorm(ci) * se.fit)
    fitfram$conf.high <- linv(stats::qlogis(fitfram$predicted) + stats::qnorm(ci) * se.fit)

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
