get_predictions_glm <- function(model,
                                data_grid,
                                ci.lvl,
                                linv,
                                value_adjustment,
                                model_class,
                                terms,
                                vcov.fun,
                                vcov.type,
                                vcov.args,
                                condition,
                                interval,
                                type,
                                ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl) && is.null(vcov.fun)

  if (type == "simulate") {
    # compute ci, two-ways
    if (!is.null(ci.lvl) && !is.na(ci.lvl)) {
      ci <- (1 + ci.lvl) / 2
    } else {
      ci <- 0.975
    }
    # simulate predictions
    .do_simulate(model, terms, ci, ...)
  } else {
    if (inherits(model, "bayesglm")) {
      prdat <- stats::predict(
        model,
        newdata = data_grid,
        type = "link",
        se.fit = se,
        ...
      )
    } else {
      # for models from "robust"-pkg (glmRob) we need to
      # suppress warnings about fake models
      prdat <- suppressWarnings(stats::predict.glm(
        model,
        newdata = data_grid,
        type = "link",
        se.fit = se,
        ...
      ))
    }

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
}
