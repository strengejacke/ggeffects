#' @export
get_predictions.mixor <- function(model,
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
  se <- (!is.null(ci_level) && !is.na(ci_level))

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
    ...
  )

  prdat <- as.data.frame(prdat$predicted)

  # bind predictions to model frame
  data_grid <- cbind(prdat, data_grid)

  # for proportional ordinal logistic regression (see MASS::polr),
  # we have predicted values for each response category. Hence,
  # gather columns

  data_grid <- .gather(data_grid, names_to = "response.level", values_to = "predicted", colnames(prdat))

  se.pred <- .standard_error_predictions(
    model = model,
    prediction_data = data_grid,
    typical = typical,
    terms = terms,
    condition = condition,
    interval = interval
  )

  if (.check_returned_se(se.pred) && isTRUE(se)) {
    se.fit <- se.pred$se.fit
    data_grid <- se.pred$prediction_data

    # CI
    data_grid$conf.low <- link_inverse(stats::qlogis(data_grid$predicted) - tcrit * se.fit)
    data_grid$conf.high <- link_inverse(stats::qlogis(data_grid$predicted) + tcrit * se.fit)

    # copy standard errors
    attr(data_grid, "std.error") <- se.fit
    attr(data_grid, "prediction.interval") <- attr(se.pred, "prediction_interval")
  } else {
    # CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  data_grid
}
