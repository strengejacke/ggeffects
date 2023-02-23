get_predictions_bamlss <- function(model, data_grid, linv, ...) {
  prdat <- suppressMessages(stats::predict(
    object = model,
    newdata = data_grid,
    model = "mu",
    type = "link"
  ))

  data_grid$predicted <- linv(as.vector(prdat))
  data_grid$conf.low <- NA
  data_grid$conf.high <- NA

  data_grid
}
