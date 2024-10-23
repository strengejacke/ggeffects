get_predictions_zeroinfl <- function(model,
                                     data_grid,
                                     ci_level,
                                     linv,
                                     type,
                                     model_class,
                                     value_adjustment,
                                     terms,
                                     vcov,
                                     vcov_args,
                                     condition,
                                     interval = NULL,
                                     verbose = TRUE,
                                     ...) {
  # get prediction type.
  if (type == "fixed") {
    pred_type <- "count"
  } else if (type == "zi_prob") {
    pred_type <- "zero"
  } else {
    pred_type <- "response"
  }

  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- (1 + ci_level) / 2
  } else {
    ci <- 0.975
  }

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  # copy object
  predicted_data <- data_grid

  # check dot-arguments
  add.args <- match.call(expand.dots = FALSE)[["..."]]
  if ("nsim" %in% names(add.args)) {
    nsim <- eval(add.args[["nsim"]])
  } else {
    nsim <- 1000
  }

  # get predictions
  prdat <- stats::predict(
    model,
    newdata = data_grid,
    type = pred_type,
    ...
  )

  if (type == "zi_prob") {
    linv <- stats::plogis
    # need back-transformation
    predicted_data$predicted <- stats::qlogis(as.vector(prdat))
  } else {
    # need back-transformation
    predicted_data$predicted <- log(as.vector(prdat))
  }


  if (type == "zero_inflated") {
    model_frame <- insight::get_data(model, source = "frame", verbose = FALSE)
    clean_terms <- .clean_terms(terms)

    newdata <- .data_grid(
      model,
      model_frame,
      terms,
      value_adjustment = value_adjustment,
      factor_adjustment = FALSE,
      show_pretty_message = FALSE,
      condition = condition
    )

    # Since the zero inflation and the conditional model are working in "opposite
    # directions", confidence intervals can not be derived directly  from the
    # "predict()"-function. Thus, confidence intervals for type = "zero_inflated" are
    # based on quantiles of simulated draws from a multivariate normal distribution
    # (see also _Brooks et al. 2017, pp.391-392_ for details).

    prdat.sim <- .simulate_zi_predictions(model, newdata, nsim, terms, value_adjustment, condition)

    if (is.null(prdat.sim) || inherits(prdat.sim, c("error", "simpleError"))) {
      insight::print_color("Error: Confidence intervals could not be computed.\n", "red")
      cat("Possibly a polynomial term is held constant (and does not appear in the `terms`-argument). Or try reducing number of simulation, using argument `nsim` (e.g. `nsim = 100`).\n")

      predicted_data$predicted <- as.vector(prdat)
      predicted_data$conf.low <- NA
      predicted_data$conf.high <- NA
    } else {
      # we need two data grids here: one for all combination of levels from the
      # model predictors ("newdata"), and one with the current combinations only
      # for the terms in question ("data_grid"). "sims" has always the same
      # number of rows as "newdata", but "data_grid" might be shorter. So we
      # merge "data_grid" and "newdata", add mean and quantiles from "sims"
      # as new variables, and then later only keep the original observations
      # from "data_grid" - by this, we avoid unequal row-lengths.

      sims <- exp(prdat.sim$cond) * (1 - stats::plogis(prdat.sim$zi))
      predicted_data <- .join_simulations(data_grid, newdata, as.vector(prdat), sims, ci, clean_terms)

      if (.obj_has_name(predicted_data, "std.error")) {
        # copy standard errors
        attr(predicted_data, "std.error") <- predicted_data$std.error
        predicted_data <- .remove_column(predicted_data, "std.error")
      }
    }
  } else {
    # get standard errors from variance-covariance matrix
    se.pred <- .standard_error_predictions(
      model = model,
      prediction_data = predicted_data,
      value_adjustment = value_adjustment,
      type = type,
      terms = terms,
      model_class = model_class,
      vcov = vcov,
      vcov_args = vcov_args,
      condition = condition,
      interval = interval,
      verbose = verbose
    )

    if (.check_returned_se(se.pred)) {
      se.fit <- se.pred$se.fit
      predicted_data <- se.pred$prediction_data

      # CI
      predicted_data$conf.low <- linv(predicted_data$predicted - tcrit * se.fit)
      predicted_data$conf.high <- linv(predicted_data$predicted + tcrit * se.fit)

      # copy standard errors and attributes
      attr(predicted_data, "std.error") <- se.fit
      attr(predicted_data, "prediction.interval") <- attr(se.pred, "prediction_interval")
    } else {
      # CI
      predicted_data$conf.low <- NA
      predicted_data$conf.high <- NA
    }

    predicted_data$predicted <- linv(predicted_data$predicted)
  }

  predicted_data
}
