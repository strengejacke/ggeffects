#' @importFrom stats qlogis predict qnorm
get_predictions_zeroinfl <- function(model, data_grid, ci.lvl, linv, type, model_class, value_adjustment, terms, vcov.fun, vcov.type, vcov.args, condition, interval = NULL, ...) {
  # get prediction type.
  pt <- if (model_class == "zeroinfl" && type == "fe")
    "count"
  else if (model_class == "zeroinfl" && type == "fe.zi")
    "response"
  else if (model_class == "zeroinfl" && type == "zi.prob")
    "zero"
  else if (model_class == "zerotrunc" && type == "fe")
    "count"
  else if (model_class == "zerotrunc" && type == "fe.zi")
    "response"
  else if (model_class == "zerotrunc" && type == "zi.prob")
    "zero"
  else if (model_class == "hurdle" && type == "fe")
    "count"
  else if (model_class == "hurdle" && type == "fe.zi")
    "response"
  else if (model_class == "hurdle" && type == "zi.prob")
    "zero"
  else
    "response"

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  # copy object
  predicted_data <- data_grid

  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

  if ("nsim" %in% names(add.args))
    nsim <- eval(add.args[["nsim"]])
  else
    nsim <- 1000


  # get predictions
  prdat <-
    stats::predict(
      model,
      newdata = data_grid,
      type = pt,
      ...
    )

  if (type == "zi.prob") {
    linv <- stats::plogis
    # need back-transformation
    predicted_data$predicted <- stats::qlogis(as.vector(prdat))
  } else {
    # need back-transformation
    predicted_data$predicted <- log(as.vector(prdat))
  }


  if (type == "fe.zi") {

    model_frame <- insight::get_data(model)
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
    # "predict()"-function. Thus, confidence intervals for type = "fe.zi" are
    # based on quantiles of simulated draws from a multivariate normal distribution
    # (see also _Brooks et al. 2017, pp.391-392_ for details).

    prdat.sim <- .simulate_predictions(model, newdata, nsim, terms, value_adjustment, condition)

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
    se.pred <-
      .standard_error_predictions(
        model = model,
        prediction_data = predicted_data,
        value_adjustment = value_adjustment,
        type = type,
        terms = terms,
        model_class = model_class,
        vcov.fun = vcov.fun,
        vcov.type = vcov.type,
        vcov.args = vcov.args,
        condition = condition,
        interval = interval
      )


    if (!is.null(se.pred)) {
      se.fit <- se.pred$se.fit
      predicted_data <- se.pred$prediction_data

      # CI
      predicted_data$conf.low <- linv(predicted_data$predicted - stats::qnorm(ci) * se.fit)
      predicted_data$conf.high <- linv(predicted_data$predicted + stats::qnorm(ci) * se.fit)

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
