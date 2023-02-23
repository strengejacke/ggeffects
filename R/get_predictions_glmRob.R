get_predictions_glmRob <- function(model, data_grid, ci.lvl, linv, value_adjustment, model_class, terms, vcov.fun, vcov.type, vcov.args, condition, interval, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl) && is.null(vcov.fun)

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
    prdat,
    se,
    ci.lvl,
    model_class,
    value_adjustment,
    terms,
    vcov.fun,
    vcov.type,
    vcov.args,
    condition,
    interval
  )
}
