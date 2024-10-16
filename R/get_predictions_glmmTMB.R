get_predictions_glmmTMB <- function(model,
                                    data_grid,
                                    ci_level,
                                    linv,
                                    type,
                                    terms,
                                    value_adjustment,
                                    condition,
                                    interval = NULL,
                                    bias_correction = FALSE,
                                    verbose = TRUE,
                                    ...) {
  # does user want standard errors?
  se <- !is.null(ci_level) && !is.na(ci_level)
  pred.interval <- FALSE

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

  model_info <- insight::model_info(model)
  clean_terms <- .clean_terms(terms)

  # check if we have zero-inflated model part
  if (!model_info$is_zero_inflated && type %in% c("zero_inflated", "zero_inflated_random", "zi_prob")) {
    if (type == "zi_prob") {
      insight::format_error("Model has no zero-inflation part.")
    } else if (type == "zero_inflated") {
      type <- "fixed"
    } else {
      type <- "random"
    }
    # tell user if he wanted ZI component, but model has no ZI part
    insight::format_alert(sprintf("Model has no zero-inflation part. Changing prediction-type to \"%s\".", type)) # nolint
  }

  # check whether predictions should be conditioned
  # on random effects (grouping level) or not.
  if (type %in% c("fixed", "zero_inflated")) {
    ref <- NA
  } else {
    ref <- NULL
  }

  # check additional arguments, like number of simulations
  additional_dot_args <- list(...)

  if ("nsim" %in% names(additional_dot_args)) {
    nsim <- eval(additional_dot_args[["nsim"]])
  } else {
    nsim <- 500
  }

  # predictions conditioned on zero-inflation component

  if (type %in% c("zero_inflated", "zero_inflated_random")) {

    # prepare argument list
    pr_args <- list(
      model,
      newdata = data_grid,
      type = "response",
      se.fit = FALSE,
      re.form = ref,
      allow.new.levels = TRUE
    )
    # add `...` arguments
    pr_args <- c(pr_args, additional_dot_args)
    # remove arguments not supported by `predict()`
    pr_args$sigma <- NULL

    # call predict with argument list
    prdat <- as.vector(do.call(stats::predict, pr_args))

    # link function, for later use
    lf <- insight::link_function(model)

    # if we have bias-correction, we must also adjust the predictions
    # this has not been done before, since we return predictions on
    # the response scale directly, without any adjustment
    if (isTRUE(bias_correction)) {
      prdat <- linv(lf(prdat))
    }

    if (se) {

      model_frame <- insight::get_data(model, source = "frame", verbose = FALSE)

      # we need a data grid with combination from *all* levels for
      # all model predictors, so the data grid has the same number
      # of rows as our simulated data from ".simulate_zi_predictions"

      newdata <- .data_grid(
        model = model,
        model_frame = model_frame,
        terms = terms,
        value_adjustment = value_adjustment,
        factor_adjustment = FALSE,
        show_pretty_message = FALSE,
        condition = condition,
        verbose = verbose
      )

      # Since the zero inflation and the conditional model are working in "opposite
      # directions", confidence intervals can not be derived directly  from the
      # "predict()"-function. Thus, confidence intervals for type = "zero_inflated" are
      # based on quantiles of simulated draws from a multivariate normal distribution
      # (see also _Brooks et al. 2017, pp.391-392_ for details).

      prdat.sim <- .simulate_zi_predictions(model, newdata, nsim, terms, value_adjustment, condition)

      if (any(vapply(prdat.sim, nrow, numeric(1)) == 0)) {
        insight::format_error(
          "Could not simulate predictions. Maybe you have used 'scale()' in the formula? If so, please standardize your data before fitting the model." # nolint
        )
      }

      if (is.null(prdat.sim) || inherits(prdat.sim, c("error", "simpleError"))) {

        if (verbose) {
          insight::print_color("Error: Confidence intervals could not be computed.\n", "red")
          if (inherits(prdat.sim, c("error", "simpleError"))) {
            cat(sprintf("* Reason: %s\n", insight::safe_deparse(prdat.sim[[1]])))
            cat(sprintf("* Source: %s\n", insight::safe_deparse(prdat.sim[[2]])))
          }
        }

        predicted_data$predicted <- prdat
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
        predicted_data <- .join_simulations(data_grid, newdata, prdat, sims, ci, clean_terms)

        # if we want to condition on random effects, we need to add residual
        # variance to the confidence intervals
        if (type == "zero_inflated_random") {
          revar <- .get_residual_variance(model)
          # get link-function and back-transform fitted values
          # to original scale, so we compute proper CI
          if (!is.null(revar)) {
            predicted_data$conf.low <- exp(lf(predicted_data$conf.low) - tcrit * sqrt(revar))
            predicted_data$conf.high <- exp(lf(predicted_data$conf.high) + tcrit * sqrt(revar))
            predicted_data$std.error <- sqrt(predicted_data$std.error^2 + revar)
          }
        }
      }
    } else {

      predicted_data$predicted <- prdat
      predicted_data$conf.low <- NA
      predicted_data$conf.high <- NA

    }

  } else if (type == "simulate") {

    # predictions conditioned on zero-inflation component and random
    # effects, based on simulations
    predicted_data <- simulate_predictions(model, nsim, clean_terms, ci, type, interval)

  } else {

    # check if model is fit with REML=TRUE. If so, results only match when
    # `type = "random"` and `interval = "confidence"`.
    if (isTRUE(model$modelInfo$REML) && !type %in% c("random", "zero_inflated_random") && !identical(interval, "confidence") && verbose) {
      insight::format_alert(
        "Model was fit with `REML = TRUE`. Don't be surprised when standard errors and confidence intervals from `predict()` are different.", # nolint
        "To match results from `predict_response()` with those from `predict()`, use `type = \"random\"` and `interval = \"confidence\"`, or refit the model with `REML = FALSE`." # nolint
      )
    }

    # predictions conditioned on count or zi-component only
    if (type == "zi_prob") {
      ptype <- "zlink"
      linv <- stats::plogis
    } else {
      ptype <- "link"
    }

    # prepare argument list
    pr_args <- list(
      model,
      newdata = data_grid,
      type = ptype,
      se.fit = se,
      re.form = ref,
      allow.new.levels = TRUE
    )
    # add `...` arguments
    pr_args <- c(pr_args, additional_dot_args)
    # remove arguments not supported by `predict()`
    pr_args$sigma <- NULL

    # call predict with argument list
    prdat <- do.call(stats::predict, pr_args)

    # did user request standard errors? if yes, compute CI
    if (se) {
      predicted_data$predicted <- linv(prdat$fit)

      # add random effect uncertainty to s.e.
      if (type %in% c("random", "zero_inflated_random") && (is.null(interval) || identical(interval, "prediction"))) {
        pvar <- prdat$se.fit^2
        prdat$se.fit <- sqrt(pvar + .get_residual_variance(model))
        pred.interval <- TRUE
      }

      # calculate CI
      predicted_data$conf.low <- linv(prdat$fit - tcrit * prdat$se.fit)
      predicted_data$conf.high <- linv(prdat$fit + tcrit * prdat$se.fit)
      predicted_data$std.error <- prdat$se.fit
    } else {
      # copy predictions
      predicted_data$predicted <- linv(as.vector(prdat))

      # no CI
      predicted_data$conf.low <- NA
      predicted_data$conf.high <- NA
    }
  }

  if (.obj_has_name(predicted_data, "std.error")) {
    # copy standard errors
    attr(predicted_data, "std.error") <- predicted_data$std.error
    predicted_data <- .remove_column(predicted_data, "std.error")
  }

  attr(predicted_data, "prediction.interval") <- pred.interval

  predicted_data
}
