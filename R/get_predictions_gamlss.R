get_predictions_gamlss <- function(model, fitfram, ci.lvl, linv, terms, model.class, typical, condition, ...) {
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  prdat <- suppressMessages(
    stats::predict(
      model,
      newdata = fitfram,
      type = "link",
      se.fit = FALSE,
      ...
    ))

  fitfram$predicted <- as.vector(prdat)

  # did user request standard errors? if yes, compute CI
  se.pred <-
    .get_se_from_vcov(
      model = model,
      fitfram = fitfram,
      typical = typical,
      terms = terms,
      model.class = model.class,
      condition = condition
    )

  if (se && !is.null(se.pred)) {

    se.fit <- se.pred$se.fit
    fitfram <- se.pred$fitfram

    # CI
    fitfram$conf.low <- linv(fitfram$predicted - stats::qnorm(ci) * se.fit)
    fitfram$conf.high <- linv(fitfram$predicted + stats::qnorm(ci) * se.fit)

    # copy standard errors
    attr(fitfram, "std.error") <- se.fit

  } else {
    # CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram$predicted <- linv(fitfram$predicted)

  fitfram
}
