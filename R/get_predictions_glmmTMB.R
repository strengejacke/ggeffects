#' @importFrom dplyr select
#' @importFrom stats predict qnorm plogis
#' @importFrom insight link_function print_color
get_predictions_glmmTMB <- function(model, fitfram, ci.lvl, linv, type, terms, typical, condition, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975


  # check if we have zero-inflated model part

  modfam <- insight::model_info(model)
  clean_terms <- get_clear_vars(terms)

  if (!modfam$is_zeroinf && type %in% c("fe.zi", "re.zi")) {
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


  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

  if ("nsim" %in% names(add.args))
    nsim <- eval(add.args[["nsim"]])
  else
    nsim <- 1000


  # predictions conditioned on zero-inflation component

  if (type %in% c("fe.zi", "re.zi")) {

    prdat <- as.vector(stats::predict(
      model,
      newdata = fitfram,
      type = "response",
      se.fit = FALSE,
      ## FIXME not implemented in glmmTMB <= 0.2.2
      # re.form = ref,
      ...
    ))

    if (!se) {

      fitfram$predicted <- prdat
      fitfram$conf.low <- NA
      fitfram$conf.high <- NA

    } else {

      mf <- insight::get_data(model)

      newdata <- get_expanded_data(
        model = model,
        mf = mf,
        terms = terms,
        typ.fun = typical,
        fac.typical = FALSE,
        pretty.message = FALSE,
        condition = condition
      )

      prdat.sim <- get_glmmTMB_predictions(model, newdata, nsim, terms, typical, condition)

      if (is.null(prdat.sim) || inherits(prdat.sim, c("error", "simpleError"))) {

        insight::print_color("Error: Confidence intervals could not be computed.\n", "red")
        if (inherits(prdat.sim, c("error", "simpleError"))) {
          cat(sprintf("* Reason: %s\n", deparse(prdat.sim[[1]], width.cutoff = 500)))
          cat(sprintf("* Source: %s\n", deparse(prdat.sim[[2]], width.cutoff = 500)))
        }

        fitfram$predicted <- prdat
        fitfram$conf.low <- NA
        fitfram$conf.high <- NA

      } else {

        sims <- exp(prdat.sim$cond) * (1 - stats::plogis(prdat.sim$zi))
        fitfram <- get_zeroinfl_fitfram(fitfram, newdata, prdat, sims, ci, clean_terms)

        if (type == "re.zi") {
          revar <- getVarRand(model)
          # get link-function and back-transform fitted values
          # to original scale, so we compute proper CI
          lf <- insight::link_function(model)
          fitfram$conf.low <- exp(lf(fitfram$conf.low) - stats::qnorm(ci) * sqrt(revar))
          fitfram$conf.high <- exp(lf(fitfram$conf.high) + stats::qnorm(ci) * sqrt(revar))
          fitfram$std.error <- sqrt(fitfram$std.error^2 + revar)
        }
      }
    }

  } else if (type == "sim") {

    # predictions conditioned on zero-inflation component and random
    # effects, based on simulations
    fitfram <- simulate_predictions(model, nsim, clean_terms, ci)

  } else {

    # predictions conditioned on count component only

    prdat <- stats::predict(
      model,
      newdata = fitfram,
      type = "link",
      se.fit = se,
      ## FIXME not implemented in glmmTMB <= 0.2.2
      ## TODO once this is fixed, update docs in ggpredict, argument type
      # re.form = ref,
      ...
    )

    # did user request standard errors? if yes, compute CI
    if (se) {
      fitfram$predicted <- linv(prdat$fit)

      # add random effect uncertainty to s.e.
      if (type %in% c("re", "re.zi")) {
        pvar <- prdat$se.fit^2
        prdat$se.fit <- sqrt(pvar + getVarRand(model))
      }

      # calculate CI
      fitfram$conf.low <- linv(prdat$fit - stats::qnorm(ci) * prdat$se.fit)
      fitfram$conf.high <- linv(prdat$fit + stats::qnorm(ci) * prdat$se.fit)
      fitfram$std.error <- prdat$se.fit
    } else {
      # copy predictions
      fitfram$predicted <- linv(as.vector(prdat))

      # no CI
      fitfram$conf.low <- NA
      fitfram$conf.high <- NA
    }
  }

  if (obj_has_name(fitfram, "std.error")) {
    # copy standard errors
    attr(fitfram, "std.error") <- fitfram$std.error
    fitfram <- dplyr::select(fitfram, -.data$std.error)
  }

  attr(fitfram, "prediction.interval") <- type %in% c("re", "re.zi")

  fitfram
}
