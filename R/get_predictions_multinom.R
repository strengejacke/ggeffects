get_predictions_multinom <- function(model, fitfram, ci.lvl, linv, value_adjustment, terms, model_class, ...) {

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- 0.975


  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "probs",
      ...
    )

  if (is.data.frame(prdat) || is.matrix(prdat))
    nc <- 1:ncol(prdat)
  else
    nc <- 1

  # Matrix to vector
  tmp <- cbind(as.data.frame(prdat), fitfram)
  fitfram <- .gather(tmp, names_to = "response.level", values_to = "predicted", colnames(tmp)[nc])


  # se.pred <-
  #   .standard_error_predictions(
  #     model = model,
  #     prediction_data = fitfram,
  #     value_adjustment = value_adjustment,
  #     terms = terms,
  #     model_class = model_class
  #   )
  #
  # if (!is.null(se.pred)) {
  #   se.fit <- se.pred$se.fit
  #   fitfram <- se.pred$prediction_data
  #   # CI
  #   fitfram$conf.low <- linv(stats::qlogis(fitfram$predicted) - stats::qnorm(ci) * se.fit)
  #   fitfram$conf.high <- linv(stats::qlogis(fitfram$predicted) + stats::qnorm(ci) * se.fit)
  # } else {
  #   # No CI
  #   fitfram$conf.low <- NA
  #   fitfram$conf.high <- NA
  # }

  # No CI
  fitfram$conf.low <- NA
  fitfram$conf.high <- NA

  fitfram
}
