get_predictions_tobit <- function(model, fitfram, ci_level, linv, ...) {
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

  # did user request standard errors? if yes, compute CI
  if (se) {
    # copy predictions
    fitfram$predicted <- linv(prdat$fit)

    # calculate CI
    fitfram$conf.low <- linv(prdat$fit - tcrit * prdat$se.fit)
    fitfram$conf.high <- linv(prdat$fit + tcrit * prdat$se.fit)

    # copy standard errors
    attr(fitfram, "std.error") <- prdat$se.fit

  } else {
    # copy predictions
    fitfram$predicted <- linv(as.vector(prdat))

    # no CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}
