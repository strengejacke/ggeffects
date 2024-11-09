# select prediction method, based on model-object
select_prediction_method <- function(model_class,
                                     model,
                                     data_grid,
                                     ci_level,
                                     type,
                                     model_info,
                                     terms,
                                     value_adjustment,
                                     vcov,
                                     vcov_args,
                                     condition,
                                     interval,
                                     bias_correction = FALSE,
                                     verbose = TRUE,
                                     ...) {
  # get link-inverse-function
  linv <- .link_inverse(model, bias_correction = bias_correction, ...)
  if (is.null(linv)) linv <- function(x) x

if (model_class == "logitr") {
    prediction_data <- get_predictions_logitr(model, data_grid, ci_level, ...)
  } else if (model_class == "glimML") {
    prediction_data <- get_predictions_glimML(model, data_grid, ci_level, linv, ...)
  } else if (model_class == "sdmTMB") {
    prediction_data <- get_predictions_sdmTMB(model, data_grid, ci_level, linv, type, ...)
  } else if (model_class == "gamlss") {
    prediction_data <- get_predictions_gamlss(model, data_grid, ci_level, terms, model_class, value_adjustment, condition, verbose = verbose, ...) # nolint
  } else if (model_class == "vglm") {
    prediction_data <- get_predictions_vglm(model, data_grid, ci_level, linv, ...)
  } else if (model_class == "Zelig-relogit") {
    prediction_data <- get_predictions_zelig(model, data_grid, ci_level, linv, ...)
  } else {
    prediction_data <- get_predictions(model,
      data_grid = data_grid,
      terms = terms,
      ci_level = ci_level,
      type = type,
      typical = value_adjustment,
      vcov = vcov,
      vcov_args = vcov_args,
      condition = condition,
      interval = interval,
      bias_correction = bias_correction,
      model_info = model_info,
      verbose = verbose,
      ...)
  }

  prediction_data
}
