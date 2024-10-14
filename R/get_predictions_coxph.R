get_predictions_coxph <- function(model,
                                  data_grid,
                                  ci_level,
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

  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- (1 + ci_level) / 2
  } else {
    ci <- 0.975
  }

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  prdat <- stats::predict(
    model,
    newdata = data_grid,
    type = "lp",
    se.fit = se,
    ...
  )

  # did user request standard errors? if yes, compute CI
  if (!is.null(vcov_fun) || (!is.null(interval) && interval == "prediction")) {
    # copy predictions
    data_grid$predicted <- exp(prdat$fit)

    se.pred <- .standard_error_predictions(
      model = model,
      prediction_data = data_grid,
      value_adjustment = value_adjustment,
      terms = terms,
      model_class = model_class,
      vcov = vcov,
      vcov_args = vcov_args,
      condition = condition,
      interval = interval
    )

    if (.check_returned_se(se.pred)) {
      se.fit <- se.pred$se.fit
      data_grid <- se.pred$prediction_data

      # CI
      data_grid$conf.low <- data_grid$predicted - tcrit * se.fit
      data_grid$conf.high <- data_grid$predicted + tcrit * se.fit

      # copy standard errors
      attr(data_grid, "std.error") <- se.fit
      attr(data_grid, "prediction.interval") <- attr(se.pred, "prediction_interval")
    } else {
      # CI
      data_grid$conf.low <- NA
      data_grid$conf.high <- NA
    }
  } else if (se) {
    # copy predictions
    data_grid$predicted <- exp(prdat$fit)

    # calculate CI
    data_grid$conf.low <- exp(prdat$fit - tcrit * prdat$se.fit)
    data_grid$conf.high <- exp(prdat$fit + tcrit * prdat$se.fit)

    # copy standard errors
    attr(data_grid, "std.error") <- prdat$se.fit
  } else {
    # copy predictions
    data_grid$predicted <- exp(as.vector(prdat))

    # no CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  data_grid
}
