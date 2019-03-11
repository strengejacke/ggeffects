#' @importFrom dplyr distinct
#' @importFrom insight get_data
get_predictions_gee <- function(model, terms, ...) {
  prdat <-
    stats::predict(
      model,
      type = "response",
      ...
    )

  mf <- insight::get_data(model)[, terms, drop = FALSE]

  # copy predictions
  mf$predicted <- as.vector(prdat)

  # No CI
  mf$conf.low <- NA
  mf$conf.high <- NA

  dplyr::distinct(mf)
}
