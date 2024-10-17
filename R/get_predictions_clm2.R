get_predictions_clm2 <- function(model, data_grid, ci_level, linv, ...) {
  # for predict.clm2, we need all unique levels of all included variables
  model_data <- insight::get_data(model, verbose = FALSE)
  prediction_data <- model_data[insight::find_variables(model, flatten = TRUE)]
  prediction_grid <- as.data.frame(expand.grid(lapply(prediction_data, unique)))

  # predictions, returned as vector
  prdat <- stats::predict(model, newdata = prediction_grid)

  # bind predictions to grid
  prediction_grid <- cbind(predicted = prdat, prediction_grid)

  # we now have predicted values for more observations than we need. We now
  # "match" the returned prediction grid with our initial data grid, but first
  # need to make sure that we remove columns in data_grid that do not exist in
  # prediction_grid
  data_grid <- data_grid[colnames(data_grid) %in% colnames(prediction_grid)]
  data_grid <- datawizard::data_match(prediction_grid, data_grid)

  colnames(data_grid)[colnames(data_grid) == insight::find_response(model)] <- "response.level"

  # No CI
  data_grid$conf.low <- NA
  data_grid$conf.high <- NA

  data_grid
}
