#' @importFrom stats model.matrix formula vcov
#' @importFrom purrr map
get_predictions_lme <- function(model, fitfram, ci.lvl, linv, type, terms, typical, model_class, vcov.fun, vcov.type, vcov.args, condition, ...) {
  # does user want standard errors?
  se <- (!is.null(ci.lvl) && !is.na(ci.lvl)) || !is.null(vcov.fun)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975


  if (inherits(model, "glmmPQL"))
    pr.type <- "link"
  else
    pr.type <- "response"

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = pr.type,
      level = 0,
      ...
    )

  # copy predictions
  fitfram$predicted <- as.vector(prdat)

  # did user request standard errors? if yes, compute CI
  if (se) {
    se.pred <-
      .get_se_from_vcov(
        model = model,
        fitfram = fitfram,
        typical = typical,
        terms = terms,
        model_class = model_class,
        type = type,
        vcov.fun = vcov.fun,
        vcov.type = vcov.type,
        vcov.args = vcov.args,
        condition = condition
      )

    if (!is.null(se.pred)) {

      se.fit <- se.pred$se.fit
      fitfram <- se.pred$fitfram

      # calculate CI
      fitfram$conf.low <- fitfram$predicted - stats::qnorm(ci) * se.fit
      fitfram$conf.high <- fitfram$predicted + stats::qnorm(ci) * se.fit

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
