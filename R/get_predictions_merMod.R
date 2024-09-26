get_predictions_merMod <- function(model,
                                   data_grid,
                                   ci.lvl,
                                   linv,
                                   type,
                                   terms,
                                   value_adjustment,
                                   condition,
                                   interval = NULL,
                                   bias_correction = FALSE,
                                   ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl)) {
    ci <- (1 + ci.lvl) / 2
  } else {
    ci <- 0.975
  }

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  # check whether predictions should be conditioned
  # on random effects (grouping level) or not.
  if (type == "fixed") {
    ref <- NA
    se_fit <- !identical(interval, "prediction")
  } else {
    # se.fit = TRUE currently doesn't work when re.form = NULL
    ref <- NULL
    se_fit <- FALSE
  }

  if (type %in% c("simulate", "simulate_random")) {
    # simulate predictions
    data_grid <- .do_simulate(model, terms, ci, type, ...)
  } else {
    # regular predictions
    lme4_predictions <- suppressWarnings(stats::predict(
      model,
      newdata = data_grid,
      type = "response",
      re.form = ref,
      allow.new.levels = TRUE,
      se.fit = se_fit,
      ...
    ))
    # do we have standard errors returned by 'predict()'?
    if (is.list(lme4_predictions)) {
      # if yes, copy predictions and standard errors
      data_grid$predicted <- as.vector(lme4_predictions$fit)
      standard_errors <- as.vector(lme4_predictions$se.fit)
    } else {
      # else, set standard_errors to NULL - we need to compute them from vcov
      data_grid$predicted <- as.vector(lme4_predictions)
      standard_errors <- NULL
    }

    # user wants standard errors?
    if (se) {
      # do we have regular standard errors, returned by 'predict()'?
      if (is.null(standard_errors)) {
        # if not, get standard errors from variance-covariance matrix
        vcov_predictions <- .standard_error_predictions(
          model = model,
          prediction_data = data_grid,
          value_adjustment = value_adjustment,
          terms = terms,
          type = type,
          condition = condition,
          interval = interval
        )
        # make sure own computed SE match length of predictions
        if (.check_returned_se(vcov_predictions)) {
          standard_errors <- vcov_predictions$se.fit
          data_grid <- vcov_predictions$prediction_data
        }
      } else {
        vcov_predictions <- NULL
      }
      # if we successfully retrieved standard errors, calculate CI
      if (!is.null(standard_errors)) {
        if (is.null(linv)) {
          # calculate CI for linear mixed models
          data_grid$conf.low <- data_grid$predicted - tcrit * standard_errors
          data_grid$conf.high <- data_grid$predicted + tcrit * standard_errors
        } else {
          # get link-function and back-transform fitted values
          # to original scale, so we compute proper CI
          lf <- insight::link_function(model)
          # if we have bias-correction, we must also adjust the predictions
          # this has not been done before, since we return predictions on
          # the response scale directly, without any adjustment
          if (isTRUE(bias_correction)) {
            data_grid$predicted <- linv(lf(data_grid$predicted))
          }
          # calculate CI for glmm
          data_grid$conf.low <- linv(lf(data_grid$predicted) - tcrit * standard_errors)
          data_grid$conf.high <- linv(lf(data_grid$predicted) + tcrit * standard_errors)
        }
        # copy standard errors
        attr(data_grid, "std.error") <- standard_errors
        if (!is.null(vcov_predictions)) {
          attr(data_grid, "prediction.interval") <- attr(vcov_predictions, "prediction_interval")
        }
      } else {
        data_grid$conf.low <- NA
        data_grid$conf.high <- NA
      }
    } else {
      data_grid$conf.low <- NA
      data_grid$conf.high <- NA
    }
  }

  data_grid
}
