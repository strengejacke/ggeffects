get_predictions_merMod <- function(model, data_grid, ci.lvl, linv, type, terms, value_adjustment, condition, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- 0.975

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  # check whether predictions should be conditioned
  # on random effects (grouping level) or not.
  if (type == "fe")
    ref <- NA
  else
    ref <- NULL

  if (type %in% c("sim", "sim_re")) {

    # simulate predictions
    data_grid <- .do_simulate(model, terms, ci, type, ...)

  } else {

    data_grid$predicted <- suppressWarnings(stats::predict(
      model,
      newdata = data_grid,
      type = "response",
      re.form = ref,
      allow.new.levels = TRUE,
      ...
    ))

    if (se) {
      # get standard errors from variance-covariance matrix
      se.pred <- .standard_error_predictions(
        model = model,
        prediction_data = data_grid,
        value_adjustment = value_adjustment,
        terms = terms,
        type = type,
        condition = condition
      )

      if (.check_returned_se(se.pred)) {
        se.fit <- se.pred$se.fit
        data_grid <- se.pred$prediction_data

        if (is.null(linv)) {
          # calculate CI for linear mixed models
          data_grid$conf.low <- data_grid$predicted - tcrit * se.fit
          data_grid$conf.high <- data_grid$predicted + tcrit * se.fit
        } else {
          # get link-function and back-transform fitted values
          # to original scale, so we compute proper CI
          lf <- insight::link_function(model)

          # calculate CI for glmm
          data_grid$conf.low <- linv(lf(data_grid$predicted) - tcrit * se.fit)
          data_grid$conf.high <- linv(lf(data_grid$predicted) + tcrit * se.fit)
        }

        # copy standard errors
        attr(data_grid, "std.error") <- se.fit
        attr(data_grid, "prediction.interval") <- attr(se.pred, "prediction_interval")
      } else {
        data_grid$conf.low <- NA
        data_grid$conf.high <- NA
      }

    } else {
      data_grid$conf.low <- NA
      data_grid$conf.high <- NA
    }

  }

  data_grid
}
