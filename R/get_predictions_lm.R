#' @export
get_predictions.lm <- function(model,
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
  se <- !is.null(ci_level) && !is.na(ci_level) && is.null(vcov)

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
    newdata = data_grid,
    type = "response",
    se.fit = se,
    ...
  )

  if (type == "simulate") {
    # simulate predictions
    data_grid <- .do_simulate(model, terms, ci, interval = interval, ...)
  } else if (!is.null(vcov) || (!is.null(interval) && interval == "prediction")) {
    # did user request standard errors? if yes, compute CI

    # copy predictions
    if ("fit" %in% names(prdat)) {
      data_grid$predicted <- as.vector(prdat$fit)
    } else {
      data_grid$predicted <- as.vector(prdat)
    }

    se.pred <- .standard_error_predictions(
      model = model,
      prediction_data = data_grid,
      typical = typical,
      terms = terms,
      vcov = vcov,
      vcov_args = vcov_args,
      condition = condition,
      interval = interval
    )

    if (.check_returned_se(se.pred)) {
      se.fit <- se.pred$se.fit
      data_grid <- se.pred$prediction_data

      # CI
      data_grid$conf.low <- data_grid$predicted - tcrit * se.fit
      data_grid$conf.high <- data_grid$predicted + tcrit * se.fit

      # copy standard errors
      attr(data_grid, "std.error") <- se.fit
      attr(data_grid, "prediction.interval") <- attr(se.pred, "prediction_interval")
    } else {
      # CI
      data_grid$conf.low <- NA
      data_grid$conf.high <- NA
    }
  } else if (se) {
    # copy predictions
    data_grid$predicted <- prdat$fit

    # calculate CI
    data_grid$conf.low <- prdat$fit - tcrit * prdat$se.fit
    data_grid$conf.high <- prdat$fit + tcrit * prdat$se.fit

    # copy standard errors
    attr(data_grid, "std.error") <- prdat$se.fit
  } else {
    # check if we have a multivariate response model
    pdim <- dim(prdat)
    if (!is.null(pdim) && pdim[2] > 1) {
      tmp <- cbind(data_grid, as.data.frame(prdat))
      gather.vars <- (ncol(data_grid) + 1):ncol(tmp)

      data_grid <- .gather(
        tmp,
        names_to = "response.level",
        values_to = "predicted",
        colnames(tmp)[gather.vars]
      )
    } else {
      # copy predictions
      data_grid$predicted <- as.vector(prdat)
    }

    # no CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  data_grid
}

#' @export
get_predictions.lmRob <- get_predictions.lm

#' @export
get_predictions.lm_robust <- get_predictions.lm

#' @export
get_predictions.biglm <- get_predictions.lm

#' @export
get_predictions.speedlm <- get_predictions.lm
