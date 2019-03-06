get_predictions_gee <- function(model, fitfram, linv, ...) {
  prdat <-
    stats::predict(
      model,
      type = "response",
      ...
    )
  # copy predictions
  fitfram$predicted <- as.vector(prdat)

  # No CI
  fitfram$conf.low <- NA
  fitfram$conf.high <- NA

  fitfram
}
