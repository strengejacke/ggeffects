#' @export
get_predictions.svyglm.nb <- function(model,
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

  # copy predictions
  .generic_prediction_data(
    model,
    data_grid = data_grid,
    link_inverse = link_inverse,
    prediction_data = prdat,
    se = se,
    ci_level = ci_level,
    typical = typical,
    terms = terms,
    vcov = vcov,
    vcov_args = vcov_args,
    condition = condition,
    interval = interval
  )
}
