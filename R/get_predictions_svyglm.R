#' @export
get_predictions.svyglm <- function(model,
                                   data_grid = NULL,
                                   terms = NULL,
                                   ci_level,
                                   type = NULL,
                                   typical = NULL,
                                   vcov = NULL,
                                   vcov_args = NULL,
                                   condition = NULL,
                                   interval = "confidence",
                                   bias_correction = FALSE,
                                   model_info = NULL,
                                   verbose = TRUE,
                                   ...) {
  # get link-inverse-function
  linv <- .link_inverse(model, bias_correction = bias_correction, ...)
  if (is.null(linv)) linv <- function(x) x

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

  # get predictions
  prdat <- stats::predict(
    model,
    newdata = data_grid,
    type = "link",
    se.fit = se,
    ...
  )

  # check if user wants standard errors
  if (se) {
    # get variance matrix for standard errors. "survey" stores the information
    # somewhat different from classical predict function
    vv <- attr(prdat, "var")

    # compute standard errors
    if (is.matrix(vv)) {
      prdat <- as.data.frame(cbind(prdat, sqrt(diag(vv))))
    } else {
      prdat <- as.data.frame(cbind(prdat, sqrt(vv)))
    }

    # consistent column names
    colnames(prdat) <- c("fit", "se.fit")

    # copy predictions
    data_grid$predicted <- linv(prdat$fit)

    # calculate CI
    data_grid$conf.low <- linv(prdat$fit - tcrit * prdat$se.fit)
    data_grid$conf.high <- linv(prdat$fit + tcrit * prdat$se.fit)

    # copy standard errors
    attr(data_grid, "std.error") <- prdat$se.fit

  } else {
    # copy predictions
    data_grid$predicted <- linv(as.vector(prdat))

    # no CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  data_grid
}


get_predictions_svyglm <- function(model, fitfram, ci_level, linv, ...) {
  # does user want standard errors?
  se <- !is.null(ci_level) && !is.na(ci_level)

  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level))
    ci <- (1 + ci_level) / 2
  else
    ci <- 0.975

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  # get predictions
  prdat <- stats::predict(
    model,
    newdata = fitfram,
    type = "link",
    se.fit = se,
    ...
  )

  # check if user wants standard errors
  if (se) {
    # get variance matrix for standard errors. "survey" stores the information
    # somewhat different from classical predict function
    vv <- attr(prdat, "var")

    # compute standard errors
    if (is.matrix(vv))
      prdat <- as.data.frame(cbind(prdat, sqrt(diag(vv))))
    else
      prdat <- as.data.frame(cbind(prdat, sqrt(vv)))

    # consistent column names
    colnames(prdat) <- c("fit", "se.fit")

    # copy predictions
    fitfram$predicted <- linv(prdat$fit)

    # calculate CI
    fitfram$conf.low <- linv(prdat$fit - tcrit * prdat$se.fit)
    fitfram$conf.high <- linv(prdat$fit + tcrit * prdat$se.fit)

    # copy standard errors
    attr(fitfram, "std.error") <- prdat$se.fit

  } else {
    # copy predictions
    fitfram$predicted <- linv(as.vector(prdat))

    # no CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}
