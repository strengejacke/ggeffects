get_predictions_Gam <- function(model, fitfram, ci.lvl, linv, value_adjustment, terms, model_class, condition, ...) {
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "link",
      ## TODO currently not supported
      se.fit = FALSE
    )

  # copy predictions
  fitfram$predicted <- linv(as.vector(prdat))

  # did user request standard errors? if yes, compute CI
  if (se) {
    se.pred <-
      .standard_error_predictions(
        model = model,
        prediction_data = fitfram,
        value_adjustment = value_adjustment,
        terms = terms,
        model_class = model_class,
        condition = condition
      )

    if (.check_returned_se(se.pred)) {
      se.fit <- se.pred$se.fit
      fitfram <- se.pred$prediction_data

      # calculate CI
      fitfram$conf.low <- linv(as.vector(prdat) - stats::qnorm(ci) * se.fit)
      fitfram$conf.high <- linv(as.vector(prdat) + stats::qnorm(ci) * se.fit)

      # copy standard errors
      attr(fitfram, "std.error") <- se.fit
    } else {
      # no CI
      fitfram$conf.low <- NA
      fitfram$conf.high <- NA
    }
  } else {
    # no CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}
