get_predictions_cgam <- function(model, fitfram, ci.lvl, linv, value_adjustment, model_class, terms, condition, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # does user want standard errors?
  if (se)
    interval <- "confidence"
  else
    interval <- "none"

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975


  prdat <- stats::predict(
    model,
    newData = fitfram,
    type = "link",
    interval = "none",
    ...
  )

  # copy predictions

  if (typeof(prdat) == "double")
    .predicted <- prdat
  else
    .predicted <- prdat$fit


  # get standard errors, if computed

  # get predicted values, on link-scale
  fitfram$predicted <- .predicted

  if (se) {
    se.pred <-
      .standard_error_predictions(
        model = model,
        prediction_data = fitfram,
        value_adjustment = value_adjustment,
        terms = terms,
        model_class = model_class,
        vcov.fun = NULL,
        vcov.type = NULL,
        vcov.args = NULL,
        condition = condition,
        interval = interval
      )

    if (!is.null(se.pred) && length(se.pred) > 0) {
      fitfram <- se.pred$prediction_data
      se.fit <- se.pred$se.fit
      se <- TRUE
    } else {
      se.fit <- NULL
      se <- FALSE
    }
  } else {
    se.pred <- NULL
  }

  if (se) {
    fitfram$conf.low <- linv(fitfram$predicted - stats::qnorm(ci) * se.fit)
    fitfram$conf.high <- linv(fitfram$predicted + stats::qnorm(ci) * se.fit)
    # copy standard errors
    attr(fitfram, "std.error") <- se.fit
    if (!is.null(se.pred) && length(se.pred) > 0)
      attr(fitfram, "prediction.interval") <- attr(se.pred, "prediction_interval")
  } else {
    # No CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  # transform predicted values
  fitfram$predicted <- linv(fitfram$predicted)

  fitfram
}
