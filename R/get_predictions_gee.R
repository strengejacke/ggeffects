#' @export
get_predictions.gee <- function(model,
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
    type = "response",
    ...
  )

  mf <- insight::get_data(model, source = "frame", verbose = FALSE)[, terms, drop = FALSE]

  # copy predictions
  mf$predicted <- as.vector(prdat)

  # No CI
  mf$conf.low <- NA
  mf$conf.high <- NA

  unique(mf)
}
