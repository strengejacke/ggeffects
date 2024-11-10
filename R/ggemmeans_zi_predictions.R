.ggemmeans_zi_predictions <- function(model,
                                      model_frame,
                                      preds,
                                      ci_level,
                                      interval,
                                      terms,
                                      cleaned_terms,
                                      typical,
                                      condition,
                                      nsim = 1000,
                                      type = "fixed") {
  prdat <- exp(preds$x1$emmean) * (1 - stats::plogis(preds$x2$emmean))

  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- (1 + ci_level) / 2
  } else {
    ci <- 0.975
  }

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  # data grid
  newdata <- .data_grid(
    model = model,
    model_frame = model_frame,
    terms = terms,
    typical = typical,
    factor_adjustment = FALSE,
    show_pretty_message = FALSE,
    condition = condition
  )

  # 2nd data grid, reasons see below
  data_grid <- .data_grid(
    model = model,
    model_frame = model_frame,
    terms = terms,
    typical = typical,
    show_pretty_message = FALSE,
    condition = condition,
    emmeans_only = FALSE,
    verbose = FALSE
  )

  # Since the zero inflation and the conditional model are working in "opposite
  # directions", confidence intervals can not be derived directly  from the
  # "predict()"-function. Thus, confidence intervals for type = "zero_inflated" are
  # based on quantiles of simulated draws from a multivariate normal distribution
  # (see also _Brooks et al. 2017, pp.391-392_ for details).

  prdat.sim <- .simulate_zi_predictions(model, newdata, nsim, terms, typical, condition)

  if (is.null(prdat.sim)) {
    insight::format_error(
      "Predicted values could not be computed. Try reducing number of simulation, using argument `nsim` (e.g. `nsim = 100`)" # nolint
    )
  }

  # we need two data grids here: one for all combination of levels from the
  # model predictors ("newdata"), and one with the current combinations only
  # for the terms in question ("data_grid"). "sims" has always the same
  # number of rows as "newdata", but "data_grid" might be shorter. So we
  # merge "data_grid" and "newdata", add mean and quantiles from "sims"
  # as new variables, and then later only keep the original observations
  # from "data_grid" - by this, we avoid unequal row-lengths.

  sims <- exp(prdat.sim$cond) * (1 - stats::plogis(prdat.sim$zi))
  prediction_data <- .join_simulations(data_grid, newdata, prdat, sims, ci, cleaned_terms)

  if (identical(interval, "prediction")) {
    revar <- .get_residual_variance(model)
    # get link-function and back-transform fitted values
    # to original scale, so we compute proper CI
    if (!is.null(revar)) {
      lf <- insight::link_function(model)
      prediction_data$conf.low <- exp(lf(prediction_data$conf.low) - tcrit * sqrt(revar))
      prediction_data$conf.high <- exp(lf(prediction_data$conf.high) + tcrit * sqrt(revar))
      prediction_data$std.error <- sqrt(prediction_data$std.error^2 + revar)
    }
  }

  prediction_data
}
