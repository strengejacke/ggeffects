#' @export
get_predictions.rqs <- function(model,
                                data_grid = NULL,
                                terms = NULL,
                                ci_level = 0.95,
                                type = NULL,
                                typical = NULL,
                                vcov = NULL,
                                vcov_args = NULL,
                                condition = NULL,
                                interval = "confidence",
                                bias_correction = FALSE,
                                link_inverse = insight::link_inverse(model),
                                model_info = NULL,
                                verbose = TRUE,
                                ...) {
  # predictions, no SE
  prdat <- stats::predict(model, newdata = data_grid, ...)

  # numeric matrix to data frame
  prdat <- as.data.frame(prdat)

  # predictions in long format
  prediction_data <- .gather(prdat, names_to = "tau", values_to = "predicted")
  prediction_data <- cbind(data_grid, prediction_data)

  # name cleanup
  prediction_data$tau <- gsub("tau= ", "", prediction_data$tau, fixed = TRUE)

  if (!is.na(ci_level) && !is.null(ci_level)) {
    # standard errors
    model_data <- insight::get_data(model, verbose = FALSE)
    data_grid[setdiff(colnames(model_data), colnames(data_grid))] <- 0

    vcm <- suppressWarnings(insight::get_varcov(model))
    mm <- insight::get_modelmatrix(model, data = data_grid)
    standard_errors <- unlist(lapply(vcm, function(vmatrix) {
      colSums(t(mm %*% vmatrix) * t(mm))
    }))

    # ci-value
    ci <- (1 + ci_level) / 2
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
