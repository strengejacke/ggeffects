get_predictions_mclogit <- function(model,
                                    fitfram,
                                    ci_level,
                                    model_class,
                                    value_adjustment,
                                    terms,
                                    vcov,
                                    vcov_args,
                                    condition,
                                    ...) {
  # does user want standard errors?
  se <- !is.null(ci_level) && !is.na(ci_level) && is.null(vcov)

  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- (1 + ci_level) / 2
  } else {
    ci <- 0.975
  }

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  # add response to new data
  resp <- insight::find_response(model, combine = FALSE)
  cn <- c(colnames(fitfram), resp)
  for (r in resp) {
    fitfram <- cbind(fitfram, 1)
  }
  colnames(fitfram) <- cn

  prdat <- stats::predict(
    model,
    newdata = fitfram,
    type = "response",
    se.fit = se,
    ...
  )

  # did user request standard errors? if yes, compute CI
  if (!is.null(vcov)) {
    # copy predictions
    if ("fit" %in% names(prdat)) {
      fitfram$predicted <- as.vector(prdat$fit)
    } else {
      fitfram$predicted <- as.vector(prdat)
    }

    se.pred <- .standard_error_predictions(
      model = model,
      prediction_data = fitfram,
      value_adjustment = value_adjustment,
      terms = terms,
      model_class = model_class,
      vcov = vcov,
      vcov_args = vcov_args,
      condition = condition
    )

    if (.check_returned_se(se.pred)) {
      se.fit <- se.pred$se.fit
      fitfram <- se.pred$prediction_data

      # CI
      fitfram$conf.low <- fitfram$predicted - tcrit * se.fit
      fitfram$conf.high <- fitfram$predicted + tcrit * se.fit

      # copy standard errors
      attr(fitfram, "std.error") <- se.fit
      attr(fitfram, "prediction.interval") <- attr(se.pred, "prediction_interval")
    } else {
      # CI
      fitfram$conf.low <- NA
      fitfram$conf.high <- NA
    }
  } else if (se) {
    # copy predictions
    fitfram$predicted <- prdat$fit

    # calculate CI
    fitfram$conf.low <- prdat$fit - tcrit * prdat$se.fit
    fitfram$conf.high <- prdat$fit + tcrit * prdat$se.fit

    # copy standard errors
    attr(fitfram, "std.error") <- prdat$se.fit
  } else {
    # copy predictions
    fitfram$predicted <- as.vector(prdat)

    # no CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}
