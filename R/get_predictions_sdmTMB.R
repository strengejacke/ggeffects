get_predictions_sdmTMB <- function(model, data_grid, ci.lvl, linv, type, ...) {

  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci
  if (se) ci <- (1 + ci.lvl) / 2

  # copy object
  predicted_data <- data_grid

  if (type != "fe") {
    insight::format_error("Only `type = \"fixed\"` is currently supported for sdmTMB models.")
  }

  # predict
  prdat <- stats::predict(
    model,
    newdata = data_grid,
    se_fit = se,
    re_form = NA, # i.e., spatial/spatiotemporal random fields off
    re_form_iid = NA,
    ...
  )

  # form output data frame
  predicted_data$predicted <- linv(prdat$est)
  if (se) {
    predicted_data$conf.low <- linv(prdat$est - stats::qnorm(ci) * prdat$est_se)
    predicted_data$conf.high <- linv(prdat$est + stats::qnorm(ci) * prdat$est_se)
    predicted_data$std.error <- prdat$est_se
  } else {
    predicted_data$conf.low <- NA
    predicted_data$conf.high <- NA
    predicted_data$std.error <- NA
  }
  predicted_data
}
