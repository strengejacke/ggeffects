#' @export
get_predictions.Gam <- function(model,
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
    type = "link",
    ## TODO currently not supported
    se.fit = FALSE
  )

  # copy predictions
  data_grid$predicted <- link_inverse(as.vector(prdat))

  # did user request standard errors? if yes, compute CI
  if (se) {
    se.pred <- .standard_error_predictions(
      model = model,
      prediction_data = data_grid,
      typical = typical,
      terms = terms,
      condition = condition
    )

    if (.check_returned_se(se.pred)) {
      se.fit <- se.pred$se.fit
      data_grid <- se.pred$prediction_data

      # calculate CI
      data_grid$conf.low <- link_inverse(as.vector(prdat) - tcrit * se.fit)
      data_grid$conf.high <- link_inverse(as.vector(prdat) + tcrit * se.fit)

      # copy standard errors
      attr(data_grid, "std.error") <- se.fit
    } else {
      # no CI
      data_grid$conf.low <- NA
      data_grid$conf.high <- NA
    }
  } else {
    # no CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  data_grid
}
