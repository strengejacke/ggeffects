#' @export
get_predictions.averaging <- function(model,
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
                                      link_inverse = insight::link_inverse(model),
                                      model_info = NULL,
                                      verbose = TRUE,
                                      ...) {
  # does user want standard errors?
  se <- !is.null(ci_level) && !is.na(ci_level) && is.null(vcov)

  # check for valid prediction type
  if (model_info$is_linear) {
    pred_type <- "response"
  } else {
    pred_type <- "link"
  }

  prdat <- stats::predict(
    model,
    newdata = data_grid,
    type = pred_type,
    se.fit = se,
    ...
  )
  # copy predictions
  .generic_prediction_data(
    model,
    data_grid,
    link_inverse,
    prediction_data = prdat,
    se,
    ci_level,
    typical,
    terms,
    vcov,
    vcov_args,
    condition,
    interval
  )
}
