get_predictions_gamlss <- function(model,
                                   fitfram,
                                   ci_level,
                                   terms,
                                   model_class,
                                   value_adjustment,
                                   condition,
                                   verbose = TRUE,
                                   ...) {
  se <- !is.null(ci_level) && !is.na(ci_level)

  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level))
    ci <- (1 + ci_level) / 2
  else
    ci <- 0.975

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  prdat <- suppressMessages(stats::predict(
    model,
    newdata = fitfram,
    type = "link",
    se.fit = FALSE,
    ...
  ))

  fitfram$predicted <- as.vector(prdat)

  # check whether prediction are requested for specific distribution parameter
  # and if so, use correct link-inverse function.

  add.args <- match.call(expand.dots = FALSE)[["..."]]

  if ("what" %in% names(add.args)) {
    what <- eval(add.args[["what"]])
  } else {
    what <- "mu"
  }

  linv <- insight::link_inverse(model, what = what)


  # did user request standard errors? if yes, compute CI
  se.pred <- .standard_error_predictions(
    model = model,
    prediction_data = fitfram,
    value_adjustment = value_adjustment,
    terms = terms,
    model_class = model_class,
    condition = condition,
    verbose = verbose
  )

  if (se && .check_returned_se(se.pred)) {

    se.fit <- se.pred$se.fit
    fitfram <- se.pred$prediction_data

    # CI
    fitfram$conf.low <- linv(fitfram$predicted - tcrit * se.fit)
    fitfram$conf.high <- linv(fitfram$predicted + tcrit * se.fit)

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
