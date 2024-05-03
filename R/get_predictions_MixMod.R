get_predictions_MixMod <- function(model, data_grid, ci.lvl, linv, type, terms, value_adjustment, condition, ...) {
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

  # get info about model
  model_info <- insight::model_info(model)

  # copy object
  predicted_data <- data_grid

  if (!model_info$is_zero_inflated && type %in% c("fe.zi", "re.zi", "zi.prob")) {
    if (type == "zi.prob") {
      insight::format_error("Model has no zero-inflation part.")
    } else if (type == "fe.zi") {
      type <- "fe"
    } else {
      type <- "re"
    }

    insight::format_alert(sprintf(
      "Model has no zero-inflation part. Changing prediction-type to \"%s\".",
      switch(type,
        fe = "fixed",
        re = "random"
      )
    ))
  }

  if (model_info$is_zero_inflated && type %in% c("fe", "re")) {
    if (type == "fe") {
      type <- "fe.zi"
    } else {
      type <- "re.zi"
    }

    insight::format_alert(sprintf(
      "Model has zero-inflation part, predicted values can only be conditioned on zero-inflation part. Changing prediction-type to \"%s\".", # nolint
      switch(type,
        fe.zi = "zero_inflated",
        re.zi = "zi_random"
      )
    ))
  }

  if (type == "sim") {

    predicted_data <- .do_simulate(model, terms, ci, ...)

  } else {

    response_name <- insight::find_response(model)
    if (is.null(condition) || !(response_name %in% names(condition))) {
      insight::format_warning(sprintf(
        "Results for MixMod-objects may vary depending on which value the response is conditioned on. Make sure to choose a sensible value for '%s' using the `condition` argument.", # nolint
        response_name
      ))
    }

    prtype <- switch(
      type,
      fe = ,
      fe.zi = "mean_subject",
      re = ,
      re.zi = "subject_specific",
      zi.prob = "zero_part",
      "mean_subject"
    )

    prdat <- stats::predict(
      model,
      newdata = data_grid,
      type = prtype,
      type_pred = "response",
      se.fit = se,
      level = ci.lvl,
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
        nsim <- 1000
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
      # "predict()"-function. Thus, confidence intervals for type = "fe.zi" are
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
      if (type == "zi.prob") {
        lf <- stats::qlogis
        linv <- stats::plogis
      } else {
        lf <- insight::link_function(model)
        if (is.null(lf)) lf <- function(x) x
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


  attr(predicted_data, "prediction.interval") <- type %in% c("re", "re.zi", "sim")
  predicted_data
}
