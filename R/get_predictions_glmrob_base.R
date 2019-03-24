get_predictions_glmrob_base <- function(model, fitfram, ci.lvl, linv, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  # for models from "robust"-pkg (glmRob) we need to
  # suppress warnings about fake models
  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "link",
      se.fit = se,
      ...
    )

  # get predicted values, on link-scale
  fitfram$predicted <- linv(prdat$fit)

  if (se) {
    fitfram$conf.low <- linv(prdat$fit - stats::qnorm(ci) * prdat$se.fit)
    fitfram$conf.high <- linv(prdat$fit + stats::qnorm(ci) * prdat$se.fit)
    # copy standard errors
    attr(fitfram, "std.error") <- prdat$se.fit
  } else {
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}
