#' @export
get_predictions.lmrob <- function(model,
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
  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- "confidence"
  } else {
    ci <- "none"
  }

  prdat <- stats::predict(
    model,
    newdata = data_grid,
    type = "response",
    interval = ci,
    level = ci_level,
    ...
  )

  # get predicted values, on link-scale
  data_grid$predicted <- prdat[, "fit"]

  if (ci == "none") {
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  } else {
    data_grid$conf.low <- prdat[, "lwr"]
    data_grid$conf.high <- prdat[, "upr"]
  }

  data_grid
}
