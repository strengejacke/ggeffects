get_predictions_vgam <- function(model, fitfram, ci_level, linv, ...) {
  prdat <- stats::predict(
    model,
    newdata = fitfram,
    type = "link",
    se.fit = FALSE
  )

  # copy predictions
  fitfram$predicted <- linv(as.vector(prdat))

  fitfram
}
