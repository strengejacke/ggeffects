get_predictions_generic <- function(model, data_grid, ci_level = NULL, linv, ...) {
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
