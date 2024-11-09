#' @export
get_predictions.wbm <- function(model,
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
  if (!is.null(ci_level) && !is.na(ci_level))
    ci <- (1 + ci_level) / 2
  else
    ci <- 0.975

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  # check whether predictions should be conditioned
  # on random effects (grouping level) or not.
  if (type == "fixed") {
    ref <- NA
  } else {
    ref <- NULL
  }

  if (type == "simulate") {

    # simulate predictions
    data_grid <- .do_simulate(model, terms, ci, ...)

  } else {
    # get model frame, that also includes prepared data, like demeaned etc.
    transformed_data <- insight::get_data(model, source = "frame", verbose = FALSE)
    # find variables that are in the model frame, but not in the new data
    new_vars <- setdiff(colnames(transformed_data), colnames(data_grid))
    # bind to data grid
    data_grid <- cbind(data_grid, transformed_data[1, new_vars])

    pred <- suppressWarnings(stats::predict(
      model,
      newdata = data_grid,
      type = "link",
      re.form = ref,
      allow.new.levels = TRUE,
      use.re.var = type == "random",
      se.fit = se,
      ...
    ))

    if (se) {
      data_grid$predicted <- link_inverse(pred$fit)
      data_grid$conf.low <- link_inverse(pred$fit - tcrit * pred$se.fit)
      data_grid$conf.high <- link_inverse(pred$fit + tcrit * pred$se.fit)
      # copy standard errors
      attr(data_grid, "std.error") <- pred$se.fit
    } else {
      data_grid$predicted <- link_inverse(as.vector(pred))
      data_grid$conf.low <- NA
      data_grid$conf.high <- NA
    }
  }

  data_grid
}

#' @export
get_predictions.wblm <- get_predictions.wbm
