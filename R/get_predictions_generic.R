get_predictions_generic <- function(model, fitfram, linv, ...) {

  if (!requireNamespace("prediction", quietly = TRUE)) {
    stop("You need to install package `prediction` first to compute marginal effects.", call. = FALSE)
  }

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
