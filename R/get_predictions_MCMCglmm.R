get_predictions_MCMCglmm <- function(model, fitfram, ci.lvl, ...) {
  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "response",
      interval = "confidence",
      level = ci.lvl,
      ...
    )

  fitfram$predicted <- prdat[, 1]
  fitfram$conf.low <- prdat[, 2]
  fitfram$conf.high <- prdat[, 3]

  # copy standard errors
  attr(fitfram, "std.error") <- NULL

  fitfram
}
