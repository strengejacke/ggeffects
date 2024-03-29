get_predictions_lrm <- function(model, fitfram, ci.lvl, linv, ...) {
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

  # for ordinal models, we need special handling
  if (isTRUE(insight::model_info(model)$is_ordinal)) {
    prdat <- stats::predict(
      model,
      newdata = fitfram,
      type = "fitted.ind",
      se.fit = FALSE,
      ...
    )

    # bind predictions to model frame
    fitfram <- cbind(prdat, fitfram)

    # reshape
    fitfram <- .gather(
      fitfram,
      names_to = "response.level",
      values_to = "predicted",
      colnames(prdat)
    )

    # No CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA

  } else {
    prdat <- stats::predict(
      model,
      newdata = fitfram,
      type = "lp",
      se.fit = se,
      ...
    )

    # copy predictions
    fitfram$predicted <- stats::plogis(prdat$linear.predictors)

    # did user request standard errors? if yes, compute CI
    if (se) {
      # calculate CI
      fitfram$conf.low <- stats::plogis(prdat$linear.predictors - tcrit * prdat$se.fit)
      fitfram$conf.high <- stats::plogis(prdat$linear.predictors + tcrit * prdat$se.fit)

      # copy standard errors
      attr(fitfram, "std.error") <- prdat$se.fit
    } else {
      # No CI
      fitfram$conf.low <- NA
      fitfram$conf.high <- NA
    }
  }

  fitfram
}
