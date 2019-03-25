get_predictions_geeglm <- function(model, fitfram, ...) {
  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      ...
    )

  fitfram$predicted <- as.vector(prdat)
  fitfram$conf.low <- NA
  fitfram$conf.high <- NA

  fitfram
}
