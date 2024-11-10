#' @export
get_predictions.glmgee <- function(model,
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
  if (is.null(vcov)) {
    vcov <- "robust"
  }
  vcov <- insight::validate_argument(vcov, c("robust", "df-adjusted", "model", "bias-corrected"))
  se <- (!is.null(ci_level) && !is.na(ci_level))

  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level))
    ci <- (1 + ci_level) / 2
  else
    ci <- 0.975

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  # get predictions
  prdat <- as.data.frame(stats::predict(
    model,
    newdata = data_grid,
    se.fit = TRUE,
    type = "link",
    varest = vcov,
    ...
  ))

  data_grid$predicted <- prdat$fit

  if (isTRUE(se)) {
    # CI
    data_grid$conf.low <- link_inverse(data_grid$predicted - tcrit * prdat$se.fit)
    data_grid$conf.high <- link_inverse(data_grid$predicted + tcrit * prdat$se.fit)

    # copy standard errors
    attr(data_grid, "std.error") <- prdat$se.fit
    attr(data_grid, "prediction.interval") <- FALSE
  } else {
    # CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  data_grid$predicted <- link_inverse(data_grid$predicted)
  data_grid
}
