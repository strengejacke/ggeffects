.ggemmeans_zi_predictions <- function(model, model_frame, preds, ci.lvl, terms, cleaned_terms, value_adjustment, condition, nsim = 1000, type = "fe") {
  prdat <- exp(preds$x1$emmean) * (1 - stats::plogis(preds$x2$emmean))
  mf <- insight::get_data(model)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  newdata <- .data_grid(
    model = model,
    model_frame = mf,
    terms = terms,
    value_adjustment = value_adjustment,
    factor_adjustment = FALSE,
    show_pretty_message = FALSE,
    condition = condition
  )

  data_grid <- .data_grid(
    model = model, model_frame = model_frame, terms = terms, value_adjustment = value_adjustment,
    show_pretty_message = FALSE, condition = condition, emmeans.only = FALSE
  )

  if (inherits(model, "MixMod")) {
    prdat.sim <- get_MixMod_predictions(model, newdata, nsim, terms, value_adjustment, condition)
  } else {
    prdat.sim <- get_glmmTMB_predictions(model, newdata, nsim, terms, value_adjustment, condition)
  }

  if (is.null(prdat.sim))
    stop("Predicted values could not be computed. Try reducing number of simulation, using argument `nsim` (e.g. `nsim = 100`)", call. = FALSE)

  sims <- exp(prdat.sim$cond) * (1 - stats::plogis(prdat.sim$zi))
  prediction_data <- .zeroinflated_prediction_data(data_grid, newdata, prdat, sims, ci, cleaned_terms)

  if (type == "re.zi") {
    revar <- .get_random_effect_variance(model)
    # get link-function and back-transform fitted values
    # to original scale, so we compute proper CI
    lf <- insight::link_function(model)
    prediction_data$conf.low <- exp(lf(prediction_data$conf.low) - stats::qnorm(ci) * sqrt(revar))
    prediction_data$conf.high <- exp(lf(prediction_data$conf.high) + stats::qnorm(ci) * sqrt(revar))
    prediction_data$std.error <- sqrt(prediction_data$std.error^2 + revar)
  }

  prediction_data
}
