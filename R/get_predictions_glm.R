get_predictions_glm <- function(model,
                                data_grid,
                                ci_level,
                                linv,
                                value_adjustment,
                                model_class,
                                terms,
                                vcov_fun,
                                vcov_type,
                                vcov_args,
                                condition,
                                interval,
                                type,
                                ...) {
  # does user want standard errors?
  se <- !is.null(ci_level) && !is.na(ci_level) && is.null(vcov_fun)

  if (type == "simulate") {
    # compute ci, two-ways
    if (!is.null(ci_level) && !is.na(ci_level)) {
      ci <- (1 + ci_level) / 2
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
      ci_level,
      model_class,
      value_adjustment,
      terms,
      vcov_fun,
      vcov_type,
      vcov_args,
      condition,
      interval
    )
  }
}
