#' @export
get_predictions.mlogit <- function(model,
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
  # bind IDX to new data
  dat <- insight::get_data(model, source = "frame", verbose = FALSE)
  data_grid <- do.call(rbind, lapply(seq_along(levels(dat$idx$id2)), function(i) {
    data_grid$idx <- sprintf("%g:%s", i, levels(dat$idx$id2)[i])
    data_grid
  }))

  prdat <- stats::predict(
    model,
    newdata = data_grid,
    ...
  )

  # stack columns
  prdat <- utils::stack(as.data.frame(prdat))
  colnames(prdat) <- c("predicted", "response.level")
  cbind(data_grid, prdat)
}


#' @export
get_predictions.mblogit <- function(model,
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

  pr <- stats::predict(
    model,
    newdata = data_grid,
    type = "response",
    se.fit = se,
    ...
  )

  if (se) {
    prdat <- as.data.frame(pr$fit)
    sedat <- as.data.frame(pr$se.fit)
  } else {
    prdat <- as.data.frame(pr$fit)
    sedat <- NULL
  }

  # bind predictions to model frame
  data_grid <- cbind(prdat, data_grid)

  # for proportional ordinal logistic regression (see MASS::polr),
  # we have predicted values for each response category. Hence,
  # gather columns

  data_grid <- .gather(data_grid, names_to = "response.level", values_to = "predicted", colnames(prdat))

  if (se && !is.null(sedat)) {
    sefram <- .gather(sedat, names_to = "response.level", values_to = "se", colnames(sedat))
    lf <- insight::link_function(model)
    # CI
    data_grid$conf.low <- link_inverse(lf(data_grid$predicted) - tcrit * lf(sefram$se))
    data_grid$conf.high <- link_inverse(lf(data_grid$predicted) + tcrit * lf(sefram$se))
  } else {
    # CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }
  data_grid
}


#' @export
get_predictions.mclogit <- function(model,
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

  # add response to new data
  resp <- insight::find_response(model, combine = FALSE)
  cn <- c(colnames(data_grid), resp)
  for (r in resp) {
    data_grid <- cbind(data_grid, 1)
  }
  colnames(data_grid) <- cn

  prdat <- stats::predict(
    model,
    newdata = data_grid,
    type = "response",
    se.fit = se,
    ...
  )

  # did user request standard errors? if yes, compute CI
  if (!is.null(vcov)) {
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
      condition = condition
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
    # copy predictions
    data_grid$predicted <- as.vector(prdat)

    # no CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  data_grid
}
