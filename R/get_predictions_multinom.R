get_predictions_multinom <- function(model, fitfram, ci_level, linv, value_adjustment, terms, model_class, ...) {

  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- (1 + ci_level) / 2
  } else {
    ci <- 0.975
  }

  if (inherits(model, c("multinom_weightit", "ordinal_weightit"))) {
    # we need the name of the response in the data
    fitfram[[insight::find_response(model)]] <- insight::get_response(model)[1]
  }

  prdat <- stats::predict(
    model,
    newdata = fitfram,
    type = "probs",
    ...
  )

  if (is.data.frame(prdat) || is.matrix(prdat))
    nc <- seq_len(ncol(prdat))
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
