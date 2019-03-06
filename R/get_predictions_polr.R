#' @importFrom dplyr bind_cols bind_rows
#' @importFrom rlang .data
get_predictions_polr <- function(model, fitfram, ci.lvl, linv, typical, terms, fun, condition, ...) {

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "probs",
      ...
    )

  prdat <- as.data.frame(prdat)

  # usually, we have same numbers of rows for predictions and model frame.
  # this is, however. not true when calling the "emm()" function. in this
  # case. just return predictions
  if (nrow(prdat) > nrow(fitfram) && ncol(prdat) == 1) {
    colnames(prdat)[1] <- "predicted"
    return(rownames_as_column(prdat, var = "response.level"))
  }

  # bind predictions to model frame
  fitfram <- dplyr::bind_cols(prdat, fitfram)

  # for proportional ordinal logistic regression (see MASS::polr),
  # we have predicted values for each response category. Hence,
  # gather columns

  fitfram <- .gather(fitfram, "response.level", "predicted", colnames(prdat))

  se.pred <-
    get_se_from_vcov(
      model = model,
      fitfram = fitfram,
      typical = typical,
      terms = terms,
      fun = fun,
      condition = condition
    )

  if (!is.null(se.pred)) {

    se.fit <- se.pred$se.fit
    fitfram <- se.pred$fitfram

    # CI
    fitfram$conf.low <- linv(stats::qlogis(fitfram$predicted) - stats::qnorm(ci) * se.fit)
    fitfram$conf.high <- linv(stats::qlogis(fitfram$predicted) + stats::qnorm(ci) * se.fit)

    # copy standard errors
    attr(fitfram, "std.error") <- se.fit

  } else {
    # CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}
