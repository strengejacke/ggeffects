#' @export
get_predictions.glimML <- function(model,
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

  # ci-level
  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- (1 + ci_level) / 2
  } else {
    ci <- 0.975
  }

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  insight::check_if_installed("aod")

  prdat <- aod::predict(
    model,
    newdata = data_grid,
    type = "link",
    se.fit = se,
    ...
  )

  # copy predictions
  data_grid$predicted <- link_inverse(prdat$fit)

  # did user request standard errors? if yes, compute CI
  if (se) {
    # calculate CI
    data_grid$conf.low <- link_inverse(prdat$fit - tcrit * prdat$se.fit)
    data_grid$conf.high <- link_inverse(prdat$fit + tcrit * prdat$se.fit)

    # copy standard errors
    attr(data_grid, "std.error") <- prdat$se.fit
  } else {
    # No CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  data_grid
}
