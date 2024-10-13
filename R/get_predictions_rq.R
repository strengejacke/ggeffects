get_predictions_rq <- function(model, fitfram, ci_level, ...) {

  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- "confidence"
  } else {
    ci <- "none"
  }

  prdat <- stats::predict(
    model,
    newdata = fitfram,
    interval = ci,
    level = ci_level,
    ...
  )

  # get predicted values, on link-scale
  fitfram$predicted <- prdat[, 1]

  if (ci == "none") {
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  } else {
    fitfram$conf.low <- prdat[, 2]
    fitfram$conf.high <- prdat[, 3]
  }

  fitfram
}
