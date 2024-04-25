get_predictions_MCMCglmm <- function(model, fitfram, ci.lvl, interval, terms, value_adjustment, condition, ...) {
  if (!(interval %in% c("confidence", "prediction"))) {
    interval <- "confidence"
  }

  new_grid <- .data_grid(
    model,
    model_frame = insight::get_data(model, source = "frame", verbose = FALSE),
    terms = terms,
    value_adjustment = value_adjustment,
    factor_adjustment = FALSE,
    show_pretty_message = FALSE,
    condition = condition,
    emmeans.only = FALSE
  )

  prdat <- stats::predict(
    model,
    newdata = new_grid,
    type = "response",
    interval = interval,
    level = ci.lvl,
    ...
  )

  new_grid$predicted <- prdat[, 1]
  new_grid$conf.low <- prdat[, 2]
  new_grid$conf.high <- prdat[, 3]
  fitfram <- merge(new_grid, fitfram, sort = FALSE)

  # copy standard errors
  attr(fitfram, "std.error") <- NULL
  attr(fitfram, "prediction.interval") <- interval == "prediction"

  fitfram
}
