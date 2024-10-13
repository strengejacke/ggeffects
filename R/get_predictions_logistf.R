get_predictions_logistf <- function(model,
                                    data_grid,
                                    ci_level,
                                    linv,
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
    data_grid$conf.low <- linv(data_grid$predicted - tcrit * prdat$se.fit)
    data_grid$conf.high <- linv(data_grid$predicted + tcrit * prdat$se.fit)
    # copy standard errors
    attr(data_grid, "std.error") <- prdat$se.fit
  } else {
    # No CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  # transform predicted values
  data_grid$predicted <- linv(data_grid$predicted)

  data_grid
}
