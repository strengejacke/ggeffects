#' @export
get_predictions.vgam <- function(model,
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
  prdat <- stats::predict(
    model,
    newdata = data_grid,
    type = "link",
    se.fit = FALSE
  )

  # copy predictions
  fitfram$predicted <- link_inverse(as.vector(prdat))

  fitfram
}
