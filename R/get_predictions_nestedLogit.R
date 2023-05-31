get_predictions_nestedLogit <- function(model, data_grid, ci.lvl, linv, ...) {
  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- 0.975


  predictions <- as.data.frame(stats::predict(
    model,
    newdata = data_grid,
    ...
  ))

  # create ID for merging
  data_grid$.rowid <- seq_len(nrow(data_grid))
  predictions$.rowid <- data_grid$.rowid

  colnames(predictions)[colnames(predictions) == "response"] <- "response.level"
  colnames(predictions)[colnames(predictions) == "logit"] <- "predicted"

  # merge predictions to data grid
  data_grid <- merge(
    predictions[c("response.level", "predicted", "se.logit", ".rowid")],
    data_grid,
    by = ".rowid",
    sort = FALSE
  )

  # CI
  data_grid$conf.low <- linv(data_grid$predicted - stats::qnorm(ci) * data_grid$se.logit)
  data_grid$conf.high <- linv(data_grid$predicted + stats::qnorm(ci) * data_grid$se.logit)
  data_grid$predicted <- linv(data_grid$predicted)

  # remove SE
  data_grid$se.logit <- NULL
  data_grid$.rowid <- NULL

  data_grid
}
