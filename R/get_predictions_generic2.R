#' @importFrom stats qlogis
get_predictions_generic2 <- function(model, fitfram, ci.lvl, linv, type, fun, typical, terms, vcov.fun, vcov.type, vcov.args, condition, ...) {
  # get prediction type.
  pt <- dplyr::case_when(
    fun == "betareg" ~ "link",
    fun == "vgam" ~ "link",
    TRUE ~ "response"
  )

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  # get predictions
  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = pt,
      ...
    )

  fitfram$predicted <- as.vector(prdat)


  # get standard errors from variance-covariance matrix
  se.pred <-
    get_se_from_vcov(
      model = model,
      fitfram = fitfram,
      typical = typical,
      type = type,
      terms = terms,
      fun = fun,
      vcov.fun = vcov.fun,
      vcov.type = vcov.type,
      vcov.args = vcov.args,
      condition = condition
    )


  if (!is.null(se.pred)) {

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
