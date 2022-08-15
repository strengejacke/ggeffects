get_predictions_coxph <- function(model,
                                  fitfram,
                                  ci.lvl,
                                  model_class,
                                  value_adjustment,
                                  terms,
                                  vcov.fun,
                                  vcov.type,
                                  vcov.args,
                                  condition,
                                  interval,
                                  ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "lp",
      se.fit = se,
      ...
    )

  # did user request standard errors? if yes, compute CI
  if (!is.null(vcov.fun) || (!is.null(interval) && interval == "prediction")) {
    # copy predictions
    fitfram$predicted <- exp(prdat$fit)

    se.pred <-
      .standard_error_predictions(
        model = model,
        prediction_data = fitfram,
        value_adjustment = value_adjustment,
        terms = terms,
        model_class = model_class,
        vcov.fun = vcov.fun,
        vcov.type = vcov.type,
        vcov.args = vcov.args,
        condition = condition,
        interval = interval
      )

    if (.check_returned_se(se.pred)) {

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
  } else if (se) {
    # copy predictions
    fitfram$predicted <- exp(prdat$fit)

    # calculate CI
    fitfram$conf.low <- exp(prdat$fit - stats::qnorm(ci) * prdat$se.fit)
    fitfram$conf.high <- exp(prdat$fit + stats::qnorm(ci) * prdat$se.fit)

    # copy standard errors
    attr(fitfram, "std.error") <- prdat$se.fit

  } else {
    # copy predictions
    fitfram$predicted <- exp(as.vector(prdat))

    # no CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}
