get_predictions_wbm <- function(model, fitfram, ci.lvl, linv, type, terms, typical, condition, ...) {
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

  clean_terms <- .get_cleaned_terms(terms)

  if (type == "sim") {

    add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

    if ("nsim" %in% names(add.args))
      nsim <- eval(add.args[["nsim"]])
    else
      nsim <- 1000

    fitfram <- simulate_predictions(model, nsim, clean_terms, ci)

  } else {

    pred <- suppressWarnings(stats::predict(
      model,
      newdata = fitfram,
      type = "link",
      re.form = ref,
      allow.new.levels = TRUE,
      use.re.var = type == "re",
      se.fit = se,
      ...
    ))

    if (se) {
      fitfram$predicted <- linv(pred$fit)
      fitfram$conf.low <- linv(pred$fit - stats::qnorm(ci) * pred$se.fit)
      fitfram$conf.high <- linv(pred$fit + stats::qnorm(ci) * pred$se.fit)
      # copy standard errors
      attr(fitfram, "std.error") <- pred$se.fit
    } else {
      fitfram$predicted <- linv(as.vector(pred))
      fitfram$conf.low <- NA
      fitfram$conf.high <- NA
    }
  }

  fitfram
}
