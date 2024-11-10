#' @export
get_predictions.cgam <- function(model,
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
                                 link_inverse = NULL,
                                 model_info = NULL,
                                 verbose = TRUE,
                                 ...) {
  # does user want standard errors?
  se <- !is.null(ci_level) && !is.na(ci_level)

  # does user want standard errors?
  if (se) {
    interval <- "confidence"
  } else {
    interval <- "none"
  }

  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- (1 + ci_level) / 2
  } else {
    ci <- 0.975
  }

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  prdat <- stats::predict(
    model,
    newData = data_grid,
    type = "link",
    interval = "none",
    ...
  )

  # copy predictions
  if (typeof(prdat) == "double") {
    .predicted <- prdat
  } else {
    .predicted <- prdat$fit
  }


  # get predicted values, on link-scale
  data_grid$predicted <- .predicted

  # get standard errors, if computed
  if (se) {
    se.pred <- .standard_error_predictions(
      model = model,
      prediction_data = data_grid,
      typical = typical,
      terms = terms,
      vcov = NULL,
      vcov_args = NULL,
      condition = condition,
      interval = interval
    )

    if (.check_returned_se(se.pred)) {
      data_grid <- se.pred$prediction_data
      se.fit <- se.pred$se.fit
      se <- TRUE
    } else {
      se.fit <- NULL
      se <- FALSE
    }
  } else {
    se.pred <- NULL
  }

  if (se) {
    data_grid$conf.low <- link_inverse(data_grid$predicted - tcrit * se.fit)
    data_grid$conf.high <- link_inverse(data_grid$predicted + tcrit * se.fit)
    # copy standard errors
    attr(data_grid, "std.error") <- se.fit
    if (!is.null(se.pred) && length(se.pred) > 0) {
      attr(data_grid, "prediction.interval") <- attr(se.pred, "prediction_interval")
    }
  } else {
    # No CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  # transform predicted values
  data_grid$predicted <- link_inverse(data_grid$predicted)

  data_grid
}
