get_predictions_svyglmnb <- function(model, fitfram, ci.lvl, linv, fun, typical, terms, vcov.fun, vcov.type, vcov.args, condition, interval, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "link",
      se.fit = se,
      ...
    )

  # copy predictions
  get_base_fitfram(model, fitfram, linv, prdat, se, ci.lvl, fun, typical, terms, vcov.fun, vcov.type, vcov.args, condition, interval)
}
