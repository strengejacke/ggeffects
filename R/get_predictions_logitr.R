get_predictions_logitr <- function(model, fitfram, ci.lvl, ...) {
  # bind obsID to new data
  obsID <- parse(text = insight::safe_deparse(insight::get_call(model)))[[1]]$obsID
  fitfram[[obsID]] <- model$data[[obsID]][1]

  prdat <- stats::predict(
    model,
    newdata = fitfram,
    obsID = obsID,
    ci = ci.lvl,
    ...
  )

  colnames(prdat) <- c("ID", "predicted", "conf.low", "conf.high")
  prdat$ID <- NULL
  cbind(fitfram, prdat)
}
