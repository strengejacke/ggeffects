get_predictions_rq <- function(model, fitfram, ci.lvl, ...) {

  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- "confidence"
  else
    ci <- "none"

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      interval = ci,
      level = ci.lvl,
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
