#' @export
get_predictions.lme <- function(model,
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
  se <- (!is.null(ci_level) && !is.na(ci_level)) || !is.null(vcov)

  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- (1 + ci_level) / 2
  } else {
    ci <- 0.975
  }

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  if (inherits(model, "glmmPQL")) {
    pr.type <- "link"
  } else {
    pr.type <- "response"
  }

  prdat <- stats::predict(
    model,
    newdata = data_grid,
    type = pr.type,
    level = 0, # always population level, see #267
    ...
  )

  # copy predictions
  data_grid$predicted <- as.vector(prdat)

  # did user request standard errors? if yes, compute CI
  if (se) {
    se.pred <- .standard_error_predictions(
      model = model,
      prediction_data = data_grid,
      typical = typical,
      terms = terms,
      type = type,
      vcov = vcov,
      vcov_args = vcov_args,
      condition = condition,
      interval = interval
    )

    if (.check_returned_se(se.pred)) {
      se.fit <- se.pred$se.fit
      data_grid <- se.pred$prediction_data

      # calculate CI
      data_grid$conf.low <- data_grid$predicted - tcrit * se.fit
      data_grid$conf.high <- data_grid$predicted + tcrit * se.fit

      # copy standard errors
      attr(data_grid, "std.error") <- se.fit
      attr(data_grid, "prediction.interval") <- attr(se.pred, "prediction_interval")
    } else {
      # No CI
      data_grid$conf.low <- NA
      data_grid$conf.high <- NA
    }
  } else {
    # No CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  # for glmmPQL, we need to back-transform using link-inverse

  if (inherits(model, "glmmPQL")) {
    data_grid$predicted <- link_inverse(data_grid$predicted)
    data_grid$conf.low <- link_inverse(data_grid$conf.low)
    data_grid$conf.high <- link_inverse(data_grid$conf.high)
  }

  data_grid
}

#' @export
get_predictions.gls <- get_predictions.lme

#' @export
get_predictions.glmmPQL <- get_predictions.lme

#' @export
get_predictions.plm <- get_predictions.lme
