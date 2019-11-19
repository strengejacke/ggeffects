#' @importFrom insight link_inverse
get_predictions_gamlss <- function(model, fitfram, ci.lvl, terms, model_class, typical, condition, ...) {
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

  # check whether prediction are requested for specific distribution parameter
  # and if so, use correct link-inverse function.

  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

  if ("what" %in% names(add.args))
    what <- eval(add.args[["what"]])
  else
    what <- "mu"

  linv <- insight::link_inverse(model, what = what)


  # did user request standard errors? if yes, compute CI
  se.pred <-
    .standard_error_predictions(
      model = model,
      fitfram = fitfram,
      typical = typical,
      terms = terms,
      model_class = model_class,
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
