#' @importFrom sjmisc to_long add_variables
#' @importFrom insight get_response find_response
get_predictions_clm2 <- function(model, fitfram, ci.lvl, linv, ...) {

  stop("`ggpredict()` does currently not support clm2-models.", call. = FALSE)

  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  fitfram <- sjmisc::add_variables(fitfram, as.factor(insight::get_response(model)), .before = 1)
  colnames(fitfram)[1] <- insight::find_response(model)

  # prediction, with CI
  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "prob",
      interval = se,
      level = ci,
      ...
    )

  # convert to data frame.
  prdat <- as.data.frame(prdat)

  # bind predictions to model frame
  fitfram <- cbind(prdat, fitfram)

  # get levels of response
  lv <- levels(insight::get_response(model))

  # for proportional ordinal logistic regression (see ordinal::clm),
  # we have predicted values for each response category. Hence,
  # gather columns. Since we also have conf. int. for each response
  # category, we need to gather multiple columns at once

  if (isTRUE(se)) {

    # length of each variable block
    l <- seq_len(ncol(prdat) / 3)
    colnames(fitfram)[l] <- lv

    fitfram <- sjmisc::to_long(
      fitfram,
      keys = "response.level",
      values = c("predicted", "conf.low", "conf.high"),
      l,
      l + length(l),
      l + 2 * length(l)
    )

  } else {
    fitfram <- .gather(fitfram, "response.level", "predicted", colnames(prdat))
    # No CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}
