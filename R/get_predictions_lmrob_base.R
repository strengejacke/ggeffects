get_predictions_lmrob_base <- function(model, fitfram, ci.lvl, ...) {

  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- "confidence"
  else
    ci <- "none"

  prdat <- stats::predict(
    model,
    newdata = fitfram,
    type = "response",
    interval = ci,
    level = ci.lvl,
    ...
  )

  # get predicted values, on link-scale
  fitfram$predicted <- prdat[, "fit"]

  if (ci == "none") {
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  } else {
    fitfram$conf.low <- prdat[, "lwr"]
    fitfram$conf.high <- prdat[, "upr"]
  }

  fitfram
}
