.ggemmeans_zi_predictions <- function(model, fitfram, preds, ci.lvl, terms, cleaned.terms, typical, condition, nsim = 1000, type = "fe") {
  prdat <- exp(preds$x1$emmean) * (1 - stats::plogis(preds$x2$emmean))
  mf <- insight::get_data(model)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  newdata <- .get_data_grid(
    model = model,
    mf = mf,
    terms = terms,
    typ.fun = typical,
    fac.typical = FALSE,
    pretty.message = FALSE,
    condition = condition
  )

  fitfram <- .get_data_grid(
    model = model, mf = fitfram, terms = terms, typ.fun = typical,
    pretty.message = FALSE, condition = condition, emmeans.only = FALSE
  )

  if (inherits(model, "MixMod")) {
    prdat.sim <- get_MixMod_predictions(model, newdata, nsim, terms, typical, condition)
  } else {
    prdat.sim <- get_glmmTMB_predictions(model, newdata, nsim, terms, typical, condition)
  }

  if (is.null(prdat.sim))
    stop("Predicted values could not be computed. Try reducing number of simulation, using argument `nsim` (e.g. `nsim = 100`)", call. = FALSE)

  sims <- exp(prdat.sim$cond) * (1 - stats::plogis(prdat.sim$zi))
  fitfram <- get_zeroinfl_fitfram(fitfram, newdata, prdat, sims, ci, cleaned.terms)

  if (type == "re.zi") {
    revar <- .get_random_effect_variance(model)
    # get link-function and back-transform fitted values
    # to original scale, so we compute proper CI
    lf <- insight::link_function(model)
    fitfram$conf.low <- exp(lf(fitfram$conf.low) - stats::qnorm(ci) * sqrt(revar))
    fitfram$conf.high <- exp(lf(fitfram$conf.high) + stats::qnorm(ci) * sqrt(revar))
    fitfram$std.error <- sqrt(fitfram$std.error^2 + revar)
  }

  fitfram
}
