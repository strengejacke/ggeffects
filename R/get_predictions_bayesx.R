get_predictions_bayesx <- function(model, data_grid, ...) {
  prdat <- suppressMessages(
    stats::predict(
      object = model,
      newdata = data_grid,
      type = "link"
    ))

  data_grid$predicted <- as.vector(prdat)
  data_grid$conf.low <- NA
  data_grid$conf.high <- NA

  data_grid
}
