get_predictions_svyglmnb <- function(model,
                                     fitfram,
                                     ci_level,
                                     linv,
                                     model_class,
                                     value_adjustment,
                                     terms,
                                     vcov,
                                     vcov_args,
                                     condition,
                                     interval,
                                     ...) {
  # does user want standard errors?
  se <- !is.null(ci_level) && !is.na(ci_level)

  prdat <- stats::predict(
    model,
    newdata = fitfram,
    type = "link",
    se.fit = se,
    ...
  )

  # copy predictions
  .generic_prediction_data(
    model,
    fitfram,
    linv,
    prdat,
    se,
    ci_level,
    model_class,
    value_adjustment,
    terms,
    vcov,
    vcov_args,
    condition,
    interval
  )
}
