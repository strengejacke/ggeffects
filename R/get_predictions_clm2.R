get_predictions_clm2 <- function(model, data_grid, ci.lvl, linv, ...) {

  ## TODO: check of clm2 works meanwhile
  insight::format_error("`ggpredict()` does currently not support clm2-models.")

  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- 0.975

  data_grid <- cbind(data.frame(as.factor(insight::get_response(model))), data_grid)
  colnames(data_grid)[1] <- insight::find_response(model)

  # prediction, with CI
  prdat <- stats::predict(
    model,
    newdata = data_grid,
    type = "prob",
    interval = se,
    level = ci,
    ...
  )

  # convert to data frame.
  prdat <- as.data.frame(prdat)

  # bind predictions to model frame
  data_grid <- cbind(prdat, data_grid)

  # get levels of response
  lv <- levels(insight::get_response(model))

  # for proportional ordinal logistic regression (see ordinal::clm),
  # we have predicted values for each response category. Hence,
  # gather columns. Since we also have conf. int. for each response
  # category, we need to gather multiple columns at once

  if (isTRUE(se)) {

    # length of each variable block
    l <- seq_len(ncol(prdat) / 3)
    colnames(data_grid)[l] <- lv

    data_grid <- .multiple_gather(
      data_grid,
      names_to = "response.level",
      values_to = c("predicted", "conf.low", "conf.high"),
      columns = list(l, l + length(l), l + 2 * length(l))
    )

  } else {
    data_grid <- .gather(data_grid, names_to = "response.level", values_to = "predicted", colnames(prdat))
    # No CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  data_grid
}
