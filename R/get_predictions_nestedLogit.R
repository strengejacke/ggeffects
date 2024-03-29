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
  ), newdata = data_grid)

  colnames(predictions)[colnames(predictions) == "response"] <- "response.level"
  colnames(predictions)[colnames(predictions) == "logit"] <- "predicted"

  # CI
  predictions$conf.low <- linv(predictions$predicted - stats::qnorm(ci) * predictions$se.logit)
  predictions$conf.high <- linv(predictions$predicted + stats::qnorm(ci) * predictions$se.logit)
  predictions$predicted <- linv(predictions$predicted)

  # remove SE
  predictions$se.logit <- NULL
  predictions$se.p <- NULL
  predictions[["p"]] <- NULL

  predictions
}
