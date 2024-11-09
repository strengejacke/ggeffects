#' @export
get_predictions.rq <- function(model,
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
    interval = ci,
    level = ci_level,
    ...
  )

  # get predicted values, on link-scale
  data_grid$predicted <- prdat[, 1]

  if (ci == "none") {
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  } else {
    data_grid$conf.low <- prdat[, 2]
    data_grid$conf.high <- prdat[, 3]
  }

  data_grid
}

#' @export
get_predictions.rqss <- get_predictions.rq
