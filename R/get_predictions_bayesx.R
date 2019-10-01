get_predictions_bayesx <- function(model, fitfram, ...) {
  prdat <- suppressMessages(
    stats::predict(
      object = model,
      newdata = fitfram,
      type = "link"
    ))

  fitfram$predicted <- as.vector(prdat)
  fitfram$conf.low <- NA
  fitfram$conf.high <- NA

  fitfram
}
