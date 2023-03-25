get_predictions_mblogit <- function(model, fitfram, ci.lvl, linv, ...) {

  se <- (!is.null(ci.lvl) && !is.na(ci.lvl)) || !is.null(vcov.fun)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- 0.975

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  pr <- stats::predict(
    model,
    newdata = fitfram,
    type = "response",
    se.fit = se,
    ...
  )

  if (se) {
    prdat <- as.data.frame(pr$fit)
    sedat <- as.data.frame(pr$se.fit)
  } else {
    prdat <- as.data.frame(pr$fit)
    sedat <- NULL
  }

  # bind predictions to model frame
  fitfram <- cbind(prdat, fitfram)

  # for proportional ordinal logistic regression (see MASS::polr),
  # we have predicted values for each response category. Hence,
  # gather columns

  fitfram <- .gather(fitfram, names_to = "response.level", values_to = "predicted", colnames(prdat))

  if (se && !is.null(sedat)) {
    sefram <- .gather(sedat, names_to = "response.level", values_to = "se", colnames(sedat))
    lf <- insight::link_function(model)
    # CI
    fitfram$conf.low <- linv(lf(fitfram$predicted) - tcrit * lf(sefram$se))
    fitfram$conf.high <- linv(lf(fitfram$predicted) + tcrit * lf(sefram$se))
  } else {
    # CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }
  fitfram
}
