get_predictions_polr <- function(model, fitfram, ci_level, linv, value_adjustment, terms, model_class, vcov_fun, vcov_type, vcov_args, condition, interval, ...) {

  se <- (!is.null(ci_level) && !is.na(ci_level)) || !is.null(vcov_fun)

  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level))
    ci <- (1 + ci_level) / 2
  else
    ci <- 0.975

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  prdat <- stats::predict(
    model,
    newdata = fitfram,
    type = "probs",
    ...
  )

  prdat <- as.data.frame(prdat)

  # usually, we have same numbers of rows for predictions and model frame.
  # this is, however. not true when calling the "emm()" function. in this
  # case. just return predictions
  if (nrow(prdat) > nrow(fitfram) && ncol(prdat) == 1) {
    colnames(prdat)[1] <- "predicted"
    return(.rownames_as_column(prdat, var = "response.level"))
  }

  # bind predictions to model frame
  fitfram <- cbind(prdat, fitfram)

  # for proportional ordinal logistic regression (see MASS::polr),
  # we have predicted values for each response category. Hence,
  # gather columns

  fitfram <- .gather(fitfram, names_to = "response.level", values_to = "predicted", colnames(prdat))

  se.pred <- .standard_error_predictions(
    model = model,
    prediction_data = fitfram,
    value_adjustment = value_adjustment,
    terms = terms,
    model_class = model_class,
    vcov_fun = vcov_fun,
    vcov_type = vcov_type,
    vcov_args = vcov_args,
    condition = condition,
    interval = interval
  )

  if (.check_returned_se(se.pred) && isTRUE(se)) {
    se.fit <- se.pred$se.fit
    fitfram <- se.pred$prediction_data

    # CI
    fitfram$conf.low <- linv(stats::qlogis(fitfram$predicted) - tcrit * se.fit)
    fitfram$conf.high <- linv(stats::qlogis(fitfram$predicted) + tcrit * se.fit)

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
