#' @export
get_predictions.betareg <- function(model,
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
  # get prediction type.
  prediction_type <- switch(class(model)[1],
    betareg = ,
    vgam = ,
    feglm = ,
    glmx = ,
    fixest = "link",
    "response"
  )

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

  # get predictions
  prdat <- stats::predict(
    model,
    newdata = data_grid,
    type = prediction_type,
    ...
  )

  data_grid$predicted <- as.vector(prdat)


  # get standard errors from variance-covariance matrix
  se.pred <- .standard_error_predictions(
    model = model,
    prediction_data = data_grid,
    typical = typical,
    type = type,
    terms = terms,
    vcov = vcov,
    vcov_args = vcov_args,
    condition = condition,
    interval = interval,
    verbose = verbose
  )


  if (.check_returned_se(se.pred) && isTRUE(se)) {
    se.fit <- se.pred$se.fit
    data_grid <- se.pred$prediction_data

    # CI
    data_grid$conf.low <- link_inverse(data_grid$predicted - tcrit * se.fit)
    data_grid$conf.high <- link_inverse(data_grid$predicted + tcrit * se.fit)

    # copy standard errors
    attr(data_grid, "std.error") <- se.fit
    attr(data_grid, "prediction.interval") <- attr(se.pred, "prediction_interval")
  } else {
    # CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  data_grid$predicted <- link_inverse(data_grid$predicted)

  data_grid
}

#' @export
get_predictions.truncreg <- get_predictions.betareg

#' @export
get_predictions.ivreg <- get_predictions.betareg

#' @export
get_predictions.vgam <- get_predictions.betareg

#' @export
get_predictions.fixest <- get_predictions.betareg

#' @export
get_predictions.feglm <- get_predictions.betareg

#' @export
get_predictions.glmx <- get_predictions.betareg

#' @export
get_predictions.phylolm <- get_predictions.betareg

#' @export
get_predictions.phyloglm <- get_predictions.betareg
