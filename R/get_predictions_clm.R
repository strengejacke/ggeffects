#' @importFrom sjmisc to_long
get_predictions_clm <- function(model, fitfram, ci.lvl, linv, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

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
  fitfram <- dplyr::bind_cols(prdat, fitfram)

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
