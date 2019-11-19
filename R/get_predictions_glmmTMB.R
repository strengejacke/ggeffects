#' @importFrom stats predict qnorm plogis
#' @importFrom insight link_function print_color
get_predictions_glmmTMB <- function(model, data_grid, ci.lvl, linv, type, terms, typical, condition, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  # copy object
  predicted_data <- data_grid

  # check if we have zero-inflated model part

  model_info <- insight::model_info(model)
  clean_terms <- .clean_terms(terms)

  if (!model_info$is_zero_inflated && type %in% c("fe.zi", "re.zi")) {
    if (type == "fe.zi")
      type <- "fe"
    else
      type <- "re"

    message(sprintf("Model has no zero-inflation part. Changing prediction-type to \"%s\".", type))
  }


  # check whether predictions should be conditioned
  # on random effects (grouping level) or not.

  if (type %in% c("fe", "fe.zi"))
    ref <- NA
  else
    ref <- NULL


  additional_dot_args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

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
      ## FIXME not implemented in glmmTMB <= 0.2.2
      # re.form = ref,
      ...
    ))

    if (!se) {

      predicted_data$predicted <- prdat
      predicted_data$conf.low <- NA
      predicted_data$conf.high <- NA

    } else {

      model_frame <- insight::get_data(model)

      newdata <- .data_grid(
        model = model,
        model_frame = model_frame,
        terms = terms,
        value_adjustment = typical,
        factor_adjustment = FALSE,
        show_pretty_message = FALSE,
        condition = condition
      )

      prdat.sim <- get_glmmTMB_predictions(model, newdata, nsim, terms, typical, condition)

      if (is.null(prdat.sim) || inherits(prdat.sim, c("error", "simpleError"))) {

        insight::print_color("Error: Confidence intervals could not be computed.\n", "red")
        if (inherits(prdat.sim, c("error", "simpleError"))) {
          cat(sprintf("* Reason: %s\n", .safe_deparse(prdat.sim[[1]])))
          cat(sprintf("* Source: %s\n", .safe_deparse(prdat.sim[[2]])))
        }

        predicted_data$predicted <- prdat
        predicted_data$conf.low <- NA
        predicted_data$conf.high <- NA

      } else {

        sims <- exp(prdat.sim$cond) * (1 - stats::plogis(prdat.sim$zi))
        predicted_data <- .zeroinflated_prediction_data(data_grid, newdata, prdat, sims, ci, clean_terms)

        if (type == "re.zi") {
          revar <- .get_random_effect_variance(model)
          # get link-function and back-transform fitted values
          # to original scale, so we compute proper CI
          lf <- insight::link_function(model)
          predicted_data$conf.low <- exp(lf(predicted_data$conf.low) - stats::qnorm(ci) * sqrt(revar))
          predicted_data$conf.high <- exp(lf(predicted_data$conf.high) + stats::qnorm(ci) * sqrt(revar))
          predicted_data$std.error <- sqrt(predicted_data$std.error^2 + revar)
        }
      }
    }

  } else if (type == "sim") {

    # predictions conditioned on zero-inflation component and random
    # effects, based on simulations
    predicted_data <- simulate_predictions(model, nsim, clean_terms, ci)

  } else {

    # predictions conditioned on count component only

    prdat <- stats::predict(
      model,
      newdata = data_grid,
      type = "link",
      se.fit = se,
      ## FIXME not implemented in glmmTMB <= 0.2.2
      ## TODO once this is fixed, update docs in ggpredict, argument type
      # re.form = ref,
      ...
    )

    # did user request standard errors? if yes, compute CI
    if (se) {
      predicted_data$predicted <- linv(prdat$fit)

      # add random effect uncertainty to s.e.
      if (type %in% c("re", "re.zi")) {
        pvar <- prdat$se.fit^2
        prdat$se.fit <- sqrt(pvar + .get_random_effect_variance(model))
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
