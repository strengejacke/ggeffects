get_predictions_generic <- function(model, fitfram, linv, ...) {
  insight::check_if_installed("prediction", "to compute adjusted predictions.")

  prdat <-
    prediction::prediction(
      model,
      data = fitfram,
      type = "response",
      ...
    )

  # copy predictions
  fitfram$predicted <- prdat$fitted

  # No CI
  fitfram$conf.low <- NA
  fitfram$conf.high <- NA

  fitfram
}
