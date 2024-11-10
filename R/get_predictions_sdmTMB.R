#' @export
get_predictions.sdmTMB <- function(model,
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

  # compute ci
  if (se) ci <- (1 + ci_level) / 2

  # copy object
  predicted_data <- data_grid

  if (type != "fixed") {
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
  predicted_data$predicted <- link_inverse(prdat$est)
  if (se) {
    predicted_data$conf.low <- link_inverse(prdat$est - stats::qnorm(ci) * prdat$est_se)
    predicted_data$conf.high <- link_inverse(prdat$est + stats::qnorm(ci) * prdat$est_se)
    predicted_data$std.error <- prdat$est_se
  } else {
    predicted_data$conf.low <- NA
    predicted_data$conf.high <- NA
    predicted_data$std.error <- NA
  }
  predicted_data
}
