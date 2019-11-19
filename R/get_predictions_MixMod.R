get_predictions_MixMod <- function(model, fitfram, ci.lvl, linv, type, terms, typical, condition, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  # get info about model
  model_info <- insight::model_info(model)

  if (!model_info$is_zero_inflated && type %in% c("fe.zi", "re.zi")) {
    if (type == "fe.zi")
      type <- "fe"
    else
      type <- "re"

    message(sprintf("Model has no zero-inflation part. Changing prediction-type to \"%s\".", type))
  }

  if (model_info$is_zero_inflated && type %in% c("fe", "re")) {
    if (type == "fe")
      type <- "fe.zi"
    else
      type <- "re.zi"

    message(sprintf("Model has zero-inflation part, predicted values can only be conditioned on zero-inflation part. Changing prediction-type to \"%s\".", type))
  }

  prtype <- switch(
    type,
    "fe" = ,
    "fe.zi" = "mean_subject",
    "re" = ,
    "re.zi" = "subject_specific",
    "mean_subject"
  )

  prdat <- stats::predict(
    model,
    newdata = fitfram,
    type = prtype,
    type_pred = "response",
    se.fit = se,
    level = ci.lvl,
    ...
  )

  if (!is.list(prdat))
    prdat <- list(pred = prdat)

  fitfram$predicted <- prdat$pred


  if (model_info$is_zero_inflated && prtype == "mean_subject") {
    add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

    if ("nsim" %in% names(add.args))
      nsim <- eval(add.args[["nsim"]])
    else
      nsim <- 1000

    model_frame <- insight::get_data(model)
    clean_terms <- .get_cleaned_terms(terms)

    newdata <- .get_data_grid(
      model = model,
      model_frame = model_frame,
      terms = terms,
      typ.fun = typical,
      fac.typical = FALSE,
      pretty.message = FALSE,
      condition = condition
    )

    prdat.sim <- get_MixMod_predictions(model, newdata, nsim, terms, typical, condition)

    if (is.null(prdat.sim) || inherits(prdat.sim, c("error", "simpleError"))) {
      insight::print_color("Error: Confidence intervals could not be computed.\n", "red")
      if (inherits(prdat.sim, c("error", "simpleError"))) {
        cat(sprintf("* Reason: %s\n", .safe_deparse(prdat.sim[[1]])))
        cat(sprintf("* Source: %s\n", .safe_deparse(prdat.sim[[2]])))
      }

      fitfram$conf.low <- NA
      fitfram$conf.high <- NA
    } else {
      sims <- exp(prdat.sim$cond) * (1 - stats::plogis(prdat.sim$zi))
      fitfram <- .zeroinflated_prediction_data(fitfram, newdata, prdat, sims, ci, clean_terms)
    }
  } else {
    if (.obj_has_name(prdat, "upp")) {
      fitfram$conf.low <- prdat$low
      fitfram$conf.high <- prdat$upp
    } else if (!is.null(prdat$se.fit)) {
      lf <- insight::link_function(model)
      if (is.null(lf)) lf <- function(x) x
      fitfram$conf.low <- linv(lf(fitfram$predicted) - stats::qnorm(ci) * prdat$se.fit)
      fitfram$conf.high <- linv(lf(fitfram$predicted) + stats::qnorm(ci) * prdat$se.fit)
    } else {
      fitfram$conf.low <- NA
      fitfram$conf.high <- NA
    }
  }


  # copy standard errors
  attr(fitfram, "std.error") <- prdat$se.fit
  attr(fitfram, "prediction.interval") <- type %in% c("re", "re.zi")

  fitfram
}
