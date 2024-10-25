#' @export
get_predictions.bamlss <- function(model,
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
                                   link_inverse = NULL,
                                   model_info = NULL,
                                   verbose = TRUE,
                                   ...) {
  prdat <- suppressMessages(stats::predict(
    object = model,
    newdata = data_grid,
    model = "mu",
    type = "link"
  ))

  data_grid$predicted <- link_inverse(as.vector(prdat))
  data_grid$conf.low <- NA
  data_grid$conf.high <- NA

  data_grid
}
