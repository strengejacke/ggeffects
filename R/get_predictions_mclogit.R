get_predictions_mclogit <- function(model, fitfram, ci.lvl, model_class, value_adjustment, terms, vcov.fun, vcov.type, vcov.args, condition, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl) && is.null(vcov.fun)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  # add response to new data
  resp <- insight::find_response(model, combine = FALSE)
  cn <- c(colnames(fitfram), resp)
  for (r in resp) {
    fitfram <- cbind(fitfram, 1)
  }
  colnames(fitfram) <- cn

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "response",
      se.fit = se,
      ...
    )

  # did user request standard errors? if yes, compute CI
  if (!is.null(vcov.fun)) {

    # copy predictions
    if ("fit" %in% names(prdat))
      fitfram$predicted <- as.vector(prdat$fit)
    else
      fitfram$predicted <- as.vector(prdat)

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
        condition = condition
      )

    if (!is.null(se.pred)) {
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
    fitfram$predicted <- prdat$fit

    # calculate CI
    fitfram$conf.low <- prdat$fit - stats::qnorm(ci) * prdat$se.fit
    fitfram$conf.high <- prdat$fit + stats::qnorm(ci) * prdat$se.fit

    # copy standard errors
    attr(fitfram, "std.error") <- prdat$se.fit

  } else {
    # copy predictions
    fitfram$predicted <- as.vector(prdat)

    # no CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}
