get_predictions_MixMod <- function(model, data_grid, ci_level, linv, type, terms, value_adjustment, condition, bias_correction = FALSE, ...) {
  # does user want standard errors?
  se <- !is.null(ci_level) && !is.na(ci_level)

  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- (1 + ci_level) / 2
  } else {
    ci <- 0.975
  }

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  # get info about model
  model_info <- insight::model_info(model)

  # copy object
  predicted_data <- data_grid

  if (!model_info$is_zero_inflated && type %in% c("zero_inflated", "zero_inflated_random", "zi_prob")) {
    if (type == "zi_prob") {
      insight::format_error("Model has no zero-inflation part.")
    } else if (type == "zero_inflated") {
      type <- "fixed"
    } else {
      type <- "random"
    }
    insight::format_alert(sprintf("Model has no zero-inflation part. Changing prediction-type to \"%s\".", type)) # nolint
  }

  if (model_info$is_zero_inflated && type %in% c("fixed", "random")) {
    if (type == "fixed") {
      type <- "zero_inflated"
    } else {
      type <- "zero_inflated_random"
    }
    insight::format_alert(sprintf("Model has zero-inflation part, predicted values can only be conditioned on zero-inflation part. Changing prediction-type to \"%s\".", type)) # nolint
  }

  if (type == "simulate") {
    predicted_data <- .do_simulate(model, terms, ci, ...)
  } else {
    response_name <- insight::find_response(model)
    if (is.null(condition) || !(response_name %in% names(condition))) {
      insight::format_alert(sprintf(
        "Results for MixMod-objects may vary depending on which value the response is conditioned on. Make sure to choose a sensible value for '%s' using the `condition` argument.", # nolint
        response_name
      ))
    }

    prtype <- switch(type,
      fe = ,
      zero_inflated = "mean_subject",
      re = ,
      zero_inflated_random = "subject_specific",
      zi_prob = "zero_part",
      "mean_subject"
    )

    prdat <- stats::predict(
      model,
      newdata = data_grid,
      type = prtype,
      type_pred = "response",
      se.fit = se,
      level = ci_level,
      ...
    )

    if (!is.list(prdat)) {
      prdat <- list(pred = prdat)
    }

    predicted_data$predicted <- prdat$pred


    if (model_info$is_zero_inflated && prtype == "mean_subject") {
      add.args <- match.call(expand.dots = FALSE)[["..."]]

      if ("nsim" %in% names(add.args)) {
        nsim <- eval(add.args[["nsim"]])
      } else {
        nsim <- 500
      }

      model_frame <- insight::get_data(model, source = "frame", verbose = FALSE)
      clean_terms <- .clean_terms(terms)

      newdata <- .data_grid(
        model = model,
        model_frame = model_frame,
        terms = terms,
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
        if (inherits(prdat.sim, c("error", "simpleError"))) {
          cat(sprintf("* Reason: %s\n", insight::safe_deparse(prdat.sim[[1]])))
          cat(sprintf("* Source: %s\n", insight::safe_deparse(prdat.sim[[2]])))
        }

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
      }
    } else if (.obj_has_name(prdat, "upp")) {
      predicted_data$conf.low <- prdat$low
      predicted_data$conf.high <- prdat$upp
    } else if (!is.null(prdat$se.fit)) {
      if (type == "zi_prob") {
        lf <- stats::qlogis
        linv <- stats::plogis
      } else {
        lf <- insight::link_function(model)
        if (is.null(lf)) lf <- function(x) x
      }
      # if we have bias-correction, we must also adjust the predictions
      # this has not been done before, since we return predictions on
      # the response scale directly, without any adjustment
      if (isTRUE(bias_correction)) {
        data_grid$predicted <- linv(lf(data_grid$predicted))
      }
      predicted_data$conf.low <- linv(lf(predicted_data$predicted) - tcrit * prdat$se.fit)
      predicted_data$conf.high <- linv(lf(predicted_data$predicted) + tcrit * prdat$se.fit)
    } else {
      predicted_data$conf.low <- NA
      predicted_data$conf.high <- NA
    }

    # copy standard errors
    attr(predicted_data, "std.error") <- prdat$se.fit
  }


  attr(predicted_data, "prediction.interval") <- type %in% c("random", "zero_inflated_random", "simulate")
  predicted_data
}
