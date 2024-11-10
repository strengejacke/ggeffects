#' @export
get_predictions.gamlss <- function(model,
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
  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- (1 + ci_level) / 2
  } else {
    ci <- 0.975
  }

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  prdat <- suppressMessages(stats::predict(
    model,
    newdata = data_grid,
    type = "link",
    se.fit = FALSE,
    ...
  ))

  data_grid$predicted <- as.vector(prdat)

  # check whether prediction are requested for specific distribution parameter
  # and if so, use correct link-inverse function.

  add.args <- match.call(expand.dots = FALSE)[["..."]]

  if ("what" %in% names(add.args)) {
    what <- eval(add.args[["what"]])
  } else {
    what <- "mu"
  }

  link_inverse <- insight::link_inverse(model, what = what)


  # did user request standard errors? if yes, compute CI
  se.pred <- .standard_error_predictions(
    model = model,
    prediction_data = data_grid,
    typical = typical,
    terms = terms,
    condition = condition,
    verbose = verbose
  )

  if (se && .check_returned_se(se.pred)) {

    se.fit <- se.pred$se.fit
    data_grid <- se.pred$prediction_data

    # CI
    data_grid$conf.low <- link_inverse(data_grid$predicted - tcrit * se.fit)
    data_grid$conf.high <- link_inverse(data_grid$predicted + tcrit * se.fit)

    # copy standard errors
    attr(data_grid, "std.error") <- se.fit

  } else {
    # CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  data_grid$predicted <- link_inverse(data_grid$predicted)

  data_grid
}
