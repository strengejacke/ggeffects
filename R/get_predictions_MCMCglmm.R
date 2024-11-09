#' @export
get_predictions.MCMCglmm <- function(model,
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
  if (!(interval %in% c("confidence", "prediction"))) {
    interval <- "confidence"
  }

  new_grid <- .data_grid(
    model,
    model_frame = insight::get_data(model, source = "frame", verbose = FALSE),
    terms = terms,
    typical = typical,
    factor_adjustment = FALSE,
    show_pretty_message = FALSE,
    condition = condition,
    emmeans_only = FALSE
  )

  prdat <- stats::predict(
    model,
    newdata = new_grid,
    type = "response",
    interval = interval,
    level = ci_level,
    ...
  )

  new_grid$predicted <- prdat[, 1]
  new_grid$conf.low <- prdat[, 2]
  new_grid$conf.high <- prdat[, 3]
  data_grid <- merge(new_grid, data_grid, sort = FALSE)

  # copy standard errors
  attr(data_grid, "std.error") <- NULL
  attr(data_grid, "prediction.interval") <- interval == "prediction"

  data_grid
}
