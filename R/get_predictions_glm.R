#' @export
get_predictions.glm <- function(model,
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

  if (type == "simulate") {
    # compute ci, two-ways
    if (!is.null(ci_level) && !is.na(ci_level)) {
      ci <- (1 + ci_level) / 2
    } else {
      ci <- 0.975
    }
    # simulate predictions
    .do_simulate(model, terms, ci, interval = interval, ...)
  } else {
    # for models from "robust"-pkg (glmRob) we need to
    # suppress warnings about fake models
    prdat <- suppressWarnings(stats::predict.glm(
      model,
      newdata = data_grid,
      type = "link",
      se.fit = se,
      ...
    ))
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
}

#' @export
get_predictions.brglm <- get_predictions.glm

#' @export
get_predictions.negbin <- get_predictions.glm
