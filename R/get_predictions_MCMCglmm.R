get_predictions_MCMCglmm <- function(model, fitfram, ci.lvl, interval, ...) {
  if (!(interval %in% c("confidence", "prediction"))) {
    interval <- "confidence"
  }

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "response",
      interval = interval,
      level = ci.lvl,
      ...
    )

  fitfram$predicted <- prdat[, 1]
  fitfram$conf.low <- prdat[, 2]
  fitfram$conf.high <- prdat[, 3]

  # copy standard errors
  attr(fitfram, "std.error") <- NULL
  attr(fitfram, "prediction.interval") <- interval == "prediction"

  fitfram
}
