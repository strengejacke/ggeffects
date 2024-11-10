#' @export
get_predictions.lrm <- function(model,
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
  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- (1 + ci_level) / 2
  } else {
    ci <- 0.975
  }

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  # for ordinal models, we need special handling
  if (isTRUE(model_info$is_ordinal)) {
    prdat <- stats::predict(
      model,
      newdata = data_grid,
      type = "fitted.ind",
      se.fit = FALSE,
      ...
    )

    # bind predictions to model frame
    data_grid <- cbind(prdat, data_grid)

    # reshape
    data_grid <- .gather(
      data_grid,
      names_to = "response.level",
      values_to = "predicted",
      colnames(prdat)
    )

    # No CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA

  } else {
    prdat <- stats::predict(
      model,
      newdata = data_grid,
      type = "lp",
      se.fit = se,
      ...
    )

    # copy predictions
    data_grid$predicted <- stats::plogis(prdat$linear.predictors)

    # did user request standard errors? if yes, compute CI
    if (se) {
      # calculate CI
      data_grid$conf.low <- stats::plogis(prdat$linear.predictors - tcrit * prdat$se.fit)
      data_grid$conf.high <- stats::plogis(prdat$linear.predictors + tcrit * prdat$se.fit)

      # copy standard errors
      attr(data_grid, "std.error") <- prdat$se.fit
    } else {
      # No CI
      data_grid$conf.low <- NA
      data_grid$conf.high <- NA
    }
  }

  data_grid
}

#' @export
get_predictions.orm <- get_predictions.lrm
