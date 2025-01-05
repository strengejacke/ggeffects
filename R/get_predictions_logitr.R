#' @export
get_predictions.logitr <- function(model,
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
  # bind obsID to new data
  obsID <- parse(text = insight::safe_deparse(insight::get_call(model)))[[1]]$obsID
  data_grid[[obsID]] <- model$data[[obsID]][1]

  prdat <- stats::predict(
    model,
    newdata = data_grid,
    obsID = obsID,
    level = ci_level,
    interval = "confidence",
    ...
  )

  colnames(prdat) <- c("ID", "predicted", "conf.low", "conf.high")
  prdat$ID <- NULL
  cbind(data_grid, prdat)
}
