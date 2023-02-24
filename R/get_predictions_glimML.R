get_predictions_glimML <- function(model, fitfram, ci.lvl, linv, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- 0.975

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  insight::check_if_installed("aod")

  prdat <- aod::predict(
    model,
    newdata = fitfram,
    type = "link",
    se.fit = se,
    ...
  )

  # copy predictions
  fitfram$predicted <- linv(prdat$fit)

  # did user request standard errors? if yes, compute CI
  if (se) {
    # calculate CI
    fitfram$conf.low <- linv(prdat$fit - tcrit * prdat$se.fit)
    fitfram$conf.high <- linv(prdat$fit + tcrit * prdat$se.fit)

    # copy standard errors
    attr(fitfram, "std.error") <- prdat$se.fit
  } else {
    # No CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}
