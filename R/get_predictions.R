#' @export
get_predictions <- function(model, ...) {
  UseMethod("get_predictions")
}

#' @export
get_predictions.default <- function(model,
                                    data_grid = NULL,
                                    terms = NULL,
                                    ci_level,
                                    type = NULL,
                                    typical = NULL,
                                    vcov = NULL,
                                    vcov_args = NULL,
                                    condition = NULL,
                                    interval = "confidence",
                                    bias_correction = FALSE,
                                    model_info = NULL,
                                    verbose = TRUE,
                                    ...) {
  prdat <- as.data.frame(insight::get_predicted(
    model,
    data = data_grid,
    predict = "expectation",
    ...
  ))

  # copy predictions
  data_grid$predicted <- prdat$Predicted

  if (!is.null(prdat$CI_low) && !is.null(prdat$CI_high)) {
    # No CI
    data_grid$conf.low <- prdat$CI_low
    data_grid$conf.high <- prdat$CI_high
  } else {
    # No CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  data_grid
}
