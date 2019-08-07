get_predictions_glm <- function(model, fitfram, ci.lvl, linv, typical, model.class, terms, vcov.fun, vcov.type, vcov.args, condition, interval, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl) && is.null(vcov.fun)

  # for models from "robust"-pkg (glmRob) we need to
  # suppress warnings about fake models
  prdat <-
    suppressWarnings(stats::predict.glm(
      model,
      newdata = fitfram,
      type = "link",
      se.fit = se,
      ...
    ))

  # copy predictions
  .get_base_fitfram(model, fitfram, linv, prdat, se, ci.lvl, model.class, typical, terms, vcov.fun, vcov.type, vcov.args, condition, interval)
}
