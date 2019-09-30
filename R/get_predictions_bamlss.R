get_predictions_bamlss <- function(model, fitfram, linv, ...) {
  prdat <- suppressMessages(
    stats::predict(
      object = model,
      newdata = fitfram,
      model = "mu",
      type = "link"
    ))

  fitfram$predicted <- linv(as.vector(prdat))
  fitfram$conf.low <- NA
  fitfram$conf.high <- NA

  fitfram
}
