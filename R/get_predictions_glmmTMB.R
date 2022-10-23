get_predictions_glmmTMB <- function(model, data_grid, ci.lvl, linv, type, terms, value_adjustment, condition, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  # copy object
  predicted_data <- data_grid

  model_info <- insight::model_info(model)
  clean_terms <- .clean_terms(terms)

  # check if we have zero-inflated model part
  if (!model_info$is_zero_inflated && type %in% c("fe.zi", "re.zi", "zi.prob")) {
    if (type == "zi.prob")
      stop("Model has no zero-inflation part.", call. = FALSE)
    else if (type == "fe.zi")
      type <- "fe"
    else
      type <- "re"

    insight::format_alert(sprintf("Model has no zero-inflation part. Changing prediction-type to \"%s\".", type))
  }


  # check whether predictions should be conditioned
  # on random effects (grouping level) or not.

  if (type %in% c("fe", "fe.zi"))
    ref <- NA
  else
    ref <- NULL


  additional_dot_args <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)

  if ("nsim" %in% names(additional_dot_args))
    nsim <- eval(additional_dot_args[["nsim"]])
  else
    nsim <- 1000


  # predictions conditioned on zero-inflation component

  if (type %in% c("fe.zi", "re.zi")) {

    prdat <- as.vector(stats::predict(
      model,
      newdata = data_grid,
      type = "response",
      se.fit = FALSE,
      re.form = ref,
      ...
    ))

    if (!se) {

      predicted_data$predicted <- prdat
      predicted_data$conf.low <- NA
      predicted_data$conf.high <- NA

    } else {

      model_frame <- insight::get_data(model)

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
        condition = condition
      )

      # Since the zero inflation and the conditional model are working in "opposite
      # directions", confidence intervals can not be derived directly  from the
      # "predict()"-function. Thus, confidence intervals for type = "fe.zi" are
      # based on quantiles of simulated draws from a multivariate normal distribution
      # (see also _Brooks et al. 2017, pp.391-392_ for details).

      prdat.sim <- .simulate_zi_predictions(model, newdata, nsim, terms, value_adjustment, condition)

      if (any(sapply(prdat.sim, nrow) == 0)) {
        insight::format_error(
          "Could not simulate predictions. Maybe you have used 'scale()' in the formula? If so, please standardize your data before fitting the model."
        )
      }

      if (is.null(prdat.sim) || inherits(prdat.sim, c("error", "simpleError"))) {

        insight::print_color("Error: Confidence intervals could not be computed.\n", "red")
        if (inherits(prdat.sim, c("error", "simpleError"))) {
          cat(sprintf("* Reason: %s\n", insight::safe_deparse(prdat.sim[[1]])))
          cat(sprintf("* Source: %s\n", insight::safe_deparse(prdat.sim[[2]])))
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

        if (type == "re.zi") {
          revar <- .get_residual_variance(model)
          # get link-function and back-transform fitted values
          # to original scale, so we compute proper CI
          if (!is.null(revar)) {
            lf <- insight::link_function(model)
            predicted_data$conf.low <- exp(lf(predicted_data$conf.low) - stats::qnorm(ci) * sqrt(revar))
            predicted_data$conf.high <- exp(lf(predicted_data$conf.high) + stats::qnorm(ci) * sqrt(revar))
            predicted_data$std.error <- sqrt(predicted_data$std.error^2 + revar)
          }
        }
      }
    }

  } else if (type == "sim") {

    # predictions conditioned on zero-inflation component and random
    # effects, based on simulations
    predicted_data <- simulate_predictions(model, nsim, clean_terms, ci, type)

  } else {

    # predictions conditioned on count or zi-component only

    if (type == "zi.prob") {
      prdat <- stats::predict(
        model,
        newdata = data_grid,
        type = "zlink",
        se.fit = se,
        re.form = ref,
        ...
      )
      linv <- stats::plogis
    } else {
      prdat <- stats::predict(
        model,
        newdata = data_grid,
        type = "link",
        se.fit = se,
        re.form = ref,
        ...
      )
    }

    # did user request standard errors? if yes, compute CI
    if (se) {
      predicted_data$predicted <- linv(prdat$fit)

      # add random effect uncertainty to s.e.
      if (type %in% c("re", "re.zi")) {
        pvar <- prdat$se.fit^2
        prdat$se.fit <- sqrt(pvar + .get_residual_variance(model))
      }

      # calculate CI
      predicted_data$conf.low <- linv(prdat$fit - stats::qnorm(ci) * prdat$se.fit)
      predicted_data$conf.high <- linv(prdat$fit + stats::qnorm(ci) * prdat$se.fit)
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

  attr(predicted_data, "prediction.interval") <- type %in% c("re", "re.zi")

  predicted_data
}
