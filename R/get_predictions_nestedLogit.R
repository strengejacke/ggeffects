#' @export
get_predictions.nestedLogit <- function(model,
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
  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- (1 + ci_level) / 2
  } else {
    ci <- 0.975
  }

  predictions <- as.data.frame(stats::predict(
    model,
    newdata = data_grid,
    ...
  ), newdata = data_grid)

  colnames(predictions)[colnames(predictions) == "response"] <- "response.level"
  colnames(predictions)[colnames(predictions) == "logit"] <- "predicted"

  # CI
  predictions$conf.low <- linv(predictions$predicted - stats::qnorm(ci) * predictions$se.logit)
  predictions$conf.high <- linv(predictions$predicted + stats::qnorm(ci) * predictions$se.logit)
  predictions$predicted <- linv(predictions$predicted)

  # remove SE
  predictions$se.logit <- NULL
  predictions$se.p <- NULL
  predictions[["p"]] <- NULL

  predictions
}
