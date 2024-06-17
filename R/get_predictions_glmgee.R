get_predictions_glmgee <- function(model,
                                   fitfram,
                                   ci.lvl,
                                   linv,
                                   vcov = c("robust", "df-adjusted", "model", "bias-corrected"),
                                   ...) {
  if (is.null(vcov)) {
    vcov <- "robust"
  }
  vcov <- match.arg(vcov)
  se <- (!is.null(ci.lvl) && !is.na(ci.lvl))

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- 0.975

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  # get predictions
  prdat <- as.data.frame(stats::predict(
    model,
    newdata = fitfram,
    se.fit = TRUE,
    type = "link",
    varest = vcov,
    ...
  ))

  fitfram$predicted <- prdat$fit

  if (isTRUE(se)) {
    # CI
    fitfram$conf.low <- linv(fitfram$predicted - tcrit * prdat$se.fit)
    fitfram$conf.high <- linv(fitfram$predicted + tcrit * prdat$se.fit)

    # copy standard errors
    attr(fitfram, "std.error") <- prdat$se.fit
    attr(fitfram, "prediction.interval") <- FALSE
  } else {
    # CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram$predicted <- linv(fitfram$predicted)
  fitfram
}
