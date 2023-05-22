get_predictions_nestedLogit <- function(model, fitfram, ci.lvl, ...) {
  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- 0.975


  prdat <- as.data.frame(stats::predict(
    model,
    newdata = fitfram,
    ...
  ))

  nc <- seq_len(ncol(prdat))
  tmp <- cbind(prdat, fitfram)
  fitfram <- .gather(tmp, names_to = "response.level", values_to = "predicted", colnames(tmp)[nc])

  # No CI
  fitfram$conf.low <- NA
  fitfram$conf.high <- NA

  fitfram
}
