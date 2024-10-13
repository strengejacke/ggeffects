get_predictions_lmrob_base <- function(model, fitfram, ci_level, ...) {

  if (!is.null(ci_level) && !is.na(ci_level))
    ci <- "confidence"
  else
    ci <- "none"

  prdat <- stats::predict(
    model,
    newdata = fitfram,
    type = "response",
    interval = ci,
    level = ci_level,
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
