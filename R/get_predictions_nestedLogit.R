get_predictions_nestedLogit <- function(model, fitfram, ci.lvl, ...) {
  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- 0.975


  predictions <- as.data.frame(stats::predict(
    model,
    newdata = fitfram,
    ...
  ))

  nr <- nrow(predictions)
  
  tmp <- cbind(prdat, fitfram)
  fitfram_pred <- .gather(tmp, names_to = "response.level", values_to = "predicted", colnames(tmp)[nc])

  # standard errors
  se <- predictions[["se.p"]]
  tmp <- cbind(se, fitfram)
  fitfram_se <- .gather(tmp, names_to = "response.level", values_to = "predicted", colnames(tmp)[nc])

  # CI
  fitfram$conf.low <- linv(stats::qlogis(fitfram$predicted) - stats::qnorm(ci) * se.fit)
  fitfram$conf.high <- linv(stats::qlogis(fitfram$predicted) + stats::qnorm(ci) * se.fit)

  fitfram
}
