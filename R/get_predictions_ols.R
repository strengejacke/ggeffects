get_predictions_ols <- function(model, fitfram, ci_level, ...) {
  # does user want standard errors?
  se <- !is.null(ci_level) && !is.na(ci_level)

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
    type = "lp",
    se.fit = se,
    ...
  )

  if (se) {
    # copy predictions
    fitfram$predicted <- prdat$linear.predictors

    # calculate CI
    fitfram$conf.low <- prdat$linear.predictors - tcrit * prdat$se.fit
    fitfram$conf.high <- prdat$linear.predictors + tcrit * prdat$se.fit

    # copy standard errors
    attr(fitfram, "std.error") <- prdat$se.fit

  } else {
    # copy predictions
    fitfram$predicted <- as.vector(prdat$linear.predictors)

    # no CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}
