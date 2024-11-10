#' @export
get_predictions.logistf <- function(model,
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

  prdat <- stats::predict(
    model,
    newdata = data_grid,
    type = "link",
    se.fit = se,
    ...
  )

  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- (1 + ci_level) / 2
  } else {
    ci <- 0.975
  }

  # get predicted values, on link-scale
  data_grid$predicted <- prdat$fit

  # did user request standard errors? if yes, compute CI
  if (se && !is.null(prdat$se.fit)) {
    tcrit <- stats::qnorm(ci)
    data_grid$conf.low <- link_inverse(data_grid$predicted - tcrit * prdat$se.fit)
    data_grid$conf.high <- link_inverse(data_grid$predicted + tcrit * prdat$se.fit)
    # copy standard errors
    attr(data_grid, "std.error") <- prdat$se.fit
  } else {
    # No CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  # transform predicted values
  data_grid$predicted <- link_inverse(data_grid$predicted)

  data_grid
}

#' @export
get_predictions.flic <- get_predictions.logistf

#' @export
get_predictions.flac <- get_predictions.logistf
