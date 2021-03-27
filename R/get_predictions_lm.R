get_predictions_lm <- function(model, fitfram, ci.lvl, model_class, value_adjustment, terms, vcov.fun, vcov.type, vcov.args, condition, interval, type, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl) && is.null(vcov.fun)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "response",
      se.fit = se,
      ...
    )

  if (type == "sim") {

    # simulate predictions
    fitfram <- .do_simulate(model, terms, ci, ...)

  } else if (!is.null(vcov.fun) || (!is.null(interval) && interval == "prediction")) {

    # did user request standard errors? if yes, compute CI

    # copy predictions
    if ("fit" %in% names(prdat))
      fitfram$predicted <- as.vector(prdat$fit)
    else
      fitfram$predicted <- as.vector(prdat)

    se.pred <- .standard_error_predictions(
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
    # check if we have a multivariate response model
    pdim <- dim(prdat)
    if (!is.null(pdim) && pdim[2] > 1) {
      tmp <- cbind(fitfram, as.data.frame(prdat))
      gather.vars <- (ncol(fitfram) + 1):ncol(tmp)

      fitfram <- .gather(
        tmp,
        names_to = "response.level",
        values_to = "predicted",
        colnames(tmp)[gather.vars]
      )
    } else {
      # copy predictions
      fitfram$predicted <- as.vector(prdat)
    }

    # no CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}
