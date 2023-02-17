get_predictions_cgam <- function(model, data_grid, ci.lvl, linv, value_adjustment, model_class, terms, condition, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # does user want standard errors?
  if (se)
    interval <- "confidence"
  else
    interval <- "none"

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- 0.975


  prdat <- stats::predict(
    model,
    newData = data_grid,
    type = "link",
    interval = "none",
    ...
  )
v
  # copy predictions

  if (typeof(prdat) == "double")
    .predicted <- prdat
  else
    .predicted <- prdat$fit


  # get standard errors, if computed

  # get predicted values, on link-scale
  data_grid$predicted <- .predicted

  if (se) {
    se.pred <-
      .standard_error_predictions(
        model = model,
        prediction_data = data_grid,
        value_adjustment = value_adjustment,
        terms = terms,
        model_class = model_class,
        vcov.fun = NULL,
        vcov.type = NULL,
        vcov.args = NULL,
        condition = condition,
        interval = interval
      )

    if (.check_returned_se(se.pred)) {
      data_grid <- se.pred$prediction_data
      se.fit <- se.pred$se.fit
      se <- TRUE
    } else {
      se.fit <- NULL
      se <- FALSE
    }
  } else {
    se.pred <- NULL
  }

  if (se) {
    data_grid$conf.low <- linv(data_grid$predicted - stats::qnorm(ci) * se.fit)
    data_grid$conf.high <- linv(data_grid$predicted + stats::qnorm(ci) * se.fit)
    # copy standard errors
    attr(data_grid, "std.error") <- se.fit
    if (!is.null(se.pred) && length(se.pred) > 0)
      attr(data_grid, "prediction.interval") <- attr(se.pred, "prediction_interval")
  } else {
    # No CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  # transform predicted values
  data_grid$predicted <- linv(data_grid$predicted)

  data_grid
}
