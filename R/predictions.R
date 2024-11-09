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
  } else if (model_class == "glmgee") {
    prediction_data <- get_predictions_glmgee(model, data_grid, ci_level, linv, vcov = vcov, ...)
  } else if (model_class == "wbm") {
    prediction_data <- get_predictions_wbm(model, data_grid, ci_level, linv, type, terms, condition, ...)
  } else if (model_class == "geeglm") {
    prediction_data <- get_predictions_geeglm(model, data_grid, ci_level, linv, type, model_class, value_adjustment, terms, condition, ...) # nolint
  } else if (model_class == "gamlss") {
    prediction_data <- get_predictions_gamlss(model, data_grid, ci_level, terms, model_class, value_adjustment, condition, verbose = verbose, ...) # nolint
  } else if (model_class == "vglm") {
    prediction_data <- get_predictions_vglm(model, data_grid, ci_level, linv, ...)
  } else if (model_class == "gee") {
    prediction_data <- get_predictions_gee(model, terms, ...)
  } else if (model_class == "clmm") {
    prediction_data <- get_predictions_clmm(model, terms, value_adjustment, condition, ci_level, linv, ...)
  } else if (model_class == "clm2") {
    prediction_data <- get_predictions_clm2(model, data_grid, ci_level, linv, ...)
  } else if (model_class == "Zelig-relogit") {
    prediction_data <- get_predictions_zelig(model, data_grid, ci_level, linv, ...)
  } else if (model_class == "rqs") {
    prediction_data <- get_predictions_rqs(model, data_grid, ci_level, ...)
  } else if (model_class == "lmrob") {
    prediction_data <- get_predictions_lmrob_base(model, data_grid, ci_level, ...)
  } else if (model_class == "glmrob") {
    prediction_data <- get_predictions_glmrob_base(model, data_grid, ci_level, linv, ...)
  } else if (model_class == "glmRob") {
    prediction_data <- get_predictions_glmRob(model, data_grid, ci_level, linv, value_adjustment, model_class, terms, vcov, vcov_args, condition, interval, ...) # nolint
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
