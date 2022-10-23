get_predictions_mlogit <- function(model, fitfram, ...) {
  # bind IDX to new data
  dat <- insight::get_data(model)
  fitfram <- do.call(rbind, lapply(seq_along(levels(dat$idx$id2)), function(i) {
    fitfram$idx <- sprintf("%g:%s", i, levels(dat$idx$id2)[i])
    fitfram
  }))

  prdat <- stats::predict(
    model,
    newdata = fitfram,
    ...
  )

  # stack columns
  prdat <- utils::stack(as.data.frame(prdat))
  colnames(prdat) <- c("predicted", "response.level")
  cbind(fitfram, prdat)
}
