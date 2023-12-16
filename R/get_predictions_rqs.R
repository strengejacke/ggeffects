get_predictions_rqs <- function(model, fitfram, ci.lvl, ...) {
  # predictions, no SE
  prdat <- stats::predict(model, newdata = fitfram, ...)

  # numeric matrix to data frame
  prdat <- as.data.frame(prdat)

  # predictions in long format
  prediction_data <- .gather(prdat, names_to = "tau", values_to = "predicted")
  prediction_data <- cbind(fitfram, prediction_data)

  if (!is.na(ci.lvl) && !is.null(ci.lvl)) {
    # standard errors
    model_data <- insight::get_data(model)
    fitfram[setdiff(colnames(model_data), colnames(fitfram))] <- 0

    vcm <- suppressWarnings(insight::get_varcov(model))
    mm <- insight::get_modelmatrix(model, data = fitfram)
    standard_errors <- unlist(lapply(vcm, function(vmatrix) {
      colSums(t(mm %*% vmatrix) * t(mm))
    }))

    # ci-value
    ci <- (1 + ci.lvl) / 2
    # degrees of freedom
    dof <- .get_df(model)
    tcrit <- stats::qt(ci, df = dof)

    prediction_data$conf.low <- prediction_data$predicted - tcrit * standard_errors
    prediction_data$conf.high <- prediction_data$predicted + tcrit * standard_errors

    # copy standard errors
    attr(prediction_data, "std.error") <- standard_errors
  }

  prediction_data
}
