get_predictions_glmRob <- function(model,
                                   data_grid,
                                   ci_level,
                                   linv,
                                   value_adjustment,
                                   model_class,
                                   terms,
                                   vcov,
                                   vcov_args,
                                   condition,
                                   interval,
                                   ...) {
  # does user want standard errors?
  se <- !is.null(ci_level) && !is.na(ci_level) && is.null(vcov)

  # for models from "robust"-pkg (glmRob) we need to
  # suppress warnings about fake models
  prdat <- suppressWarnings(stats::predict(
    model,
    newdata = data_grid,
    type = "link",
    se.fit = se,
    ...
  ))

  # copy predictions
  .generic_prediction_data(
    model,
    data_grid,
    linv,
    prediction_data = prdat,
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
