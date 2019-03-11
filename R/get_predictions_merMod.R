get_predictions_merMod <- function(model, fitfram, ci.lvl, linv, type, terms, typical, condition, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  # check whether predictions should be conditioned
  # on random effects (grouping level) or not.
  if (type == "fe")
    ref <- NA
  else
    ref <- NULL

  clean_terms <- get_clear_vars(terms)

  if (type == "sim") {

    add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

    if ("nsim" %in% names(add.args))
      nsim <- eval(add.args[["nsim"]])
    else
      nsim <- 1000

    fitfram <- simulate_predictions(model, nsim, clean_terms, ci)

  } else {


    fitfram$predicted <- stats::predict(
      model,
      newdata = fitfram,
      type = "response",
      re.form = ref,
      allow.new.levels = TRUE,
      ...
    )

    if (se) {
      # get standard errors from variance-covariance matrix
      se.pred <-
        get_se_from_vcov(
          model = model,
          fitfram = fitfram,
          typical = typical,
          terms = terms,
          type = type,
          condition = condition
        )

      if (!is.null(se.pred)) {
        se.fit <- se.pred$se.fit
        fitfram <- se.pred$fitfram

        if (is.null(linv)) {
          # calculate CI for linear mixed models
          fitfram$conf.low <- fitfram$predicted - stats::qnorm(ci) * se.fit
          fitfram$conf.high <- fitfram$predicted + stats::qnorm(ci) * se.fit
        } else {
          # get link-function and back-transform fitted values
          # to original scale, so we compute proper CI
          lf <- insight::link_function(model)

          # calculate CI for glmm
          fitfram$conf.low <- linv(lf(fitfram$predicted) - stats::qnorm(ci) * se.fit)
          fitfram$conf.high <- linv(lf(fitfram$predicted) + stats::qnorm(ci) * se.fit)
        }

        # copy standard errors
        attr(fitfram, "std.error") <- se.fit

      } else {
        fitfram$conf.low <- NA
        fitfram$conf.high <- NA
      }

    } else {
      fitfram$conf.low <- NA
      fitfram$conf.high <- NA
    }

  }

  fitfram
}
