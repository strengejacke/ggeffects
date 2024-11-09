#' @export
get_predictions.ols <- function(model,
                                data_grid = NULL,
                                terms = NULL,
                                ci_level = 0.95,
                                type = NULL,
                                typical = NULL,
                                vcov = NULL,
                                vcov_args = NULL,
                                condition = NULL,
                                interval = "confidence",
                                bias_correction = FALSE,
                                link_inverse = insight::link_inverse(model),
                                model_info = NULL,
                                verbose = TRUE,
                                ...) {
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

  prdat <- stats::predict(
    model,
    newdata = data_grid,
    type = "lp",
    se.fit = se,
    ...
  )

  if (se) {
    # copy predictions
    data_grid$predicted <- prdat$linear.predictors

    # calculate CI
    data_grid$conf.low <- prdat$linear.predictors - tcrit * prdat$se.fit
    data_grid$conf.high <- prdat$linear.predictors + tcrit * prdat$se.fit

    # copy standard errors
    attr(data_grid, "std.error") <- prdat$se.fit

  } else {
    # copy predictions
    data_grid$predicted <- as.vector(prdat$linear.predictors)

    # no CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  data_grid
}
