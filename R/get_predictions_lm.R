get_predictions_lm <- function(model, data_grid, ci.lvl, model_class, value_adjustment, terms, vcov.fun, vcov.type, vcov.args, condition, interval, type, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl) && is.null(vcov.fun)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- 0.975

  prdat <-
    stats::predict(
      model,
      newdata = data_grid,
      type = "response",
      se.fit = se,
      ...
    )

  if (type == "sim") {

    # simulate predictions
    data_grid <- .do_simulate(model, terms, ci, ...)

  } else if (!is.null(vcov.fun) || (!is.null(interval) && interval == "prediction")) {

    # did user request standard errors? if yes, compute CI

    # copy predictions
    if ("fit" %in% names(prdat))
      data_grid$predicted <- as.vector(prdat$fit)
    else
      data_grid$predicted <- as.vector(prdat)

    se.pred <- .standard_error_predictions(
      model = model,
      prediction_data = data_grid,
      value_adjustment = value_adjustment,
      terms = terms,
      model_class = model_class,
      vcov.fun = vcov.fun,
      vcov.type = vcov.type,
      vcov.args = vcov.args,
      condition = condition,
      interval = interval
    )

    if (.check_returned_se(se.pred)) {
      se.fit <- se.pred$se.fit
      data_grid <- se.pred$prediction_data

      # CI
      data_grid$conf.low <- data_grid$predicted - stats::qnorm(ci) * se.fit
      data_grid$conf.high <- data_grid$predicted + stats::qnorm(ci) * se.fit

      # copy standard errors
      attr(data_grid, "std.error") <- se.fit
      attr(data_grid, "prediction.interval") <- attr(se.pred, "prediction_interval")
    } else {
      # CI
      data_grid$conf.low <- NA
      data_grid$conf.high <- NA
    }
  } else if (se) {
    # copy predictions
    data_grid$predicted <- prdat$fit

    # calculate CI
    data_grid$conf.low <- prdat$fit - stats::qnorm(ci) * prdat$se.fit
    data_grid$conf.high <- prdat$fit + stats::qnorm(ci) * prdat$se.fit

    # copy standard errors
    attr(data_grid, "std.error") <- prdat$se.fit

  } else {
    # check if we have a multivariate response model
    pdim <- dim(prdat)
    if (!is.null(pdim) && pdim[2] > 1) {
      tmp <- cbind(data_grid, as.data.frame(prdat))
      gather.vars <- (ncol(data_grid) + 1):ncol(tmp)

      data_grid <- .gather(
        tmp,
        names_to = "response.level",
        values_to = "predicted",
        colnames(tmp)[gather.vars]
      )
    } else {
      # copy predictions
      data_grid$predicted <- as.vector(prdat)
    }

    # no CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  data_grid
}
