get_predictions_lme <- function(model,
                                fitfram,
                                ci_level,
                                linv,
                                type,
                                terms,
                                value_adjustment,
                                model_class,
                                vcov,
                                vcov_args,
                                condition,
                                interval,
                                ...) {
  # does user want standard errors?
  se <- (!is.null(ci_level) && !is.na(ci_level)) || !is.null(vcov)

  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- (1 + ci_level) / 2
  } else {
    ci <- 0.975
  }

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  if (inherits(model, "glmmPQL")) {
    pr.type <- "link"
  } else {
    pr.type <- "response"
  }

  prdat <- stats::predict(
    model,
    newdata = fitfram,
    type = pr.type,
    level = 0, # always population level, see #267
    ...
  )

  # copy predictions
  fitfram$predicted <- as.vector(prdat)

  # did user request standard errors? if yes, compute CI
  if (se) {
    se.pred <- .standard_error_predictions(
      model = model,
      prediction_data = fitfram,
      value_adjustment = value_adjustment,
      terms = terms,
      model_class = model_class,
      type = type,
      vcov = vcov,
      vcov_args = vcov_args,
      condition = condition,
      interval = interval
    )

    if (.check_returned_se(se.pred)) {
      se.fit <- se.pred$se.fit
      fitfram <- se.pred$prediction_data

      # calculate CI
      fitfram$conf.low <- fitfram$predicted - tcrit * se.fit
      fitfram$conf.high <- fitfram$predicted + tcrit * se.fit

      # copy standard errors
      attr(fitfram, "std.error") <- se.fit
      attr(fitfram, "prediction.interval") <- attr(se.pred, "prediction_interval")
    } else {
      # No CI
      fitfram$conf.low <- NA
      fitfram$conf.high <- NA
    }
  } else {
    # No CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  # for glmmPQL, we need to back-transform using link-inverse

  if (inherits(model, "glmmPQL")) {
    fitfram$predicted <- linv(fitfram$predicted)
    fitfram$conf.low <- linv(fitfram$conf.low)
    fitfram$conf.high <- linv(fitfram$conf.high)
  }

  fitfram
}
