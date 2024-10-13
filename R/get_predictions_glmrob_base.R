get_predictions_glmrob_base <- function(model, data_grid, ci_level, linv, ...) {
  # does user want standard errors?
  se <- !is.null(ci_level) && !is.na(ci_level)

  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level))
    ci <- (1 + ci_level) / 2
  else
    ci <- 0.975

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  # for models from "robust"-pkg (glmRob) we need to
  # suppress warnings about fake models
  prdat <- stats::predict(
    model,
    newdata = data_grid,
    type = "link",
    se.fit = se,
    ...
  )

  # get predicted values, on link-scale
  data_grid$predicted <- linv(prdat$fit)

  if (se) {
    data_grid$conf.low <- linv(prdat$fit - tcrit * prdat$se.fit)
    data_grid$conf.high <- linv(prdat$fit + tcrit * prdat$se.fit)
    # copy standard errors
    attr(data_grid, "std.error") <- prdat$se.fit
  } else {
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  data_grid
}
