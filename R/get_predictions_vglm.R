get_predictions_vglm <- function(model, fitfram, ci.lvl, linv, ...) {

  if (!requireNamespace("VGAM", quietly = TRUE)) {
    stop("Package `VGAM` needed to calculate marginal effects for a vector generalized linear model.", call. = FALSE)
  }

  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  prdat <- VGAM::predictvglm(
    model,
    newdata = fitfram,
    type = "link",
    se.fit = se,
    ...
  )

  # copy predictions
  prdat$fitted.values <- as.vector(prdat$fitted.values)
  fitfram$predicted <- suppressWarnings(linv(prdat$fitted.values))

  # did user request standard errors? if yes, compute CI
  if (se) {
    # calculate CI
    fitfram$conf.low <- suppressWarnings(linv(prdat$fitted.values - stats::qnorm(ci) * prdat$se.fit))
    fitfram$conf.high <- suppressWarnings(linv(prdat$fitted.values + stats::qnorm(ci) * prdat$se.fit))
  } else {
    # no CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}
