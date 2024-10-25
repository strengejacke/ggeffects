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

  if (model_class == "svyglm.nb") {
    prediction_data <- get_predictions_svyglmnb(model, data_grid, ci_level, linv, model_class, value_adjustment, terms, vcov, vcov_args, condition, interval, ...) # nolint
  } else if (model_class %in% c("stanreg", "brmsfit")) {
    prediction_data <- get_predictions_stan(model, data_grid, ci_level, type, model_info, interval, terms, verbose = verbose, ...) # nolint
  } else if (model_class == "coxph" && type != "survival" && type != "cumulative_hazard") {
    prediction_data <- get_predictions_coxph(model, data_grid, ci_level, model_class, value_adjustment, terms, vcov, vcov_args, condition, interval, verbose = verbose, ...) # nolint
  } else if (model_class == "coxph" && type %in% c("survival", "cumulative_hazard")) {
    prediction_data <- get_predictions_survival(model, data_grid, ci_level, type, terms, ...)
  } else if (model_class == "ols") {
    prediction_data <- get_predictions_ols(model, data_grid, ci_level, ...)
  } else if (model_class == "logitr") {
    prediction_data <- get_predictions_logitr(model, data_grid, ci_level, ...)
  } else if (model_class == "nestedLogit") {
    prediction_data <- get_predictions_nestedLogit(model, data_grid, ci_level, linv, ...)
  } else if (model_class %in% c("lrm", "orm")) {
    prediction_data <- get_predictions_lrm(model, data_grid, ci_level, linv, ...)
  } else if (model_class == "glimML") {
    prediction_data <- get_predictions_glimML(model, data_grid, ci_level, linv, ...)
  } else if (model_class == "sdmTMB") {
    prediction_data <- get_predictions_sdmTMB(model, data_grid, ci_level, linv, type, ...)
  } else if (model_class == "glmgee") {
    prediction_data <- get_predictions_glmgee(model, data_grid, ci_level, linv, vcov = vcov, ...)
  } else if (model_class == "wbm") {
    prediction_data <- get_predictions_wbm(model, data_grid, ci_level, linv, type, terms, condition, ...)
  } else if (model_class %in% c("lmer", "nlmer", "glmer")) {
    prediction_data <- get_predictions_merMod(model, data_grid, ci_level, linv, type, terms, value_adjustment, condition, interval, bias_correction = bias_correction, ...) # nolint
  } else if (model_class == "geeglm") {
    prediction_data <- get_predictions_geeglm(model, data_grid, ci_level, linv, type, model_class, value_adjustment, terms, condition, ...) # nolint
  } else if (model_class == "gamlss") {
    prediction_data <- get_predictions_gamlss(model, data_grid, ci_level, terms, model_class, value_adjustment, condition, verbose = verbose, ...) # nolint
  } else if (model_class == "bamlss") {
    prediction_data <- get_predictions_bamlss(model, data_grid, linv, ...)
  } else if (model_class == "cgam") {
    prediction_data <- get_predictions_cgam(model, data_grid, ci_level, linv, value_adjustment, model_class, terms, condition, ...) # nolint
  } else if (model_class == "gam") {
    prediction_data <- get_predictions_gam(model, data_grid, ci_level, linv, type, ...)
  } else if (model_class == "Gam") {
    prediction_data <- get_predictions_Gam(model, data_grid, ci_level, linv, value_adjustment, terms, model_class, condition, ...) # nolint
  } else if (model_class == "vglm") {
    prediction_data <- get_predictions_vglm(model, data_grid, ci_level, linv, ...)
  } else if (model_class == "tobit") {
    prediction_data <- get_predictions_tobit(model, data_grid, ci_level, linv, ...)
  } else if (model_class %in% c("lme", "gls", "plm")) {
    prediction_data <- get_predictions_lme(model, data_grid, ci_level, linv, type, terms, value_adjustment, model_class, vcov, vcov_args, condition, interval, ...) # nolint
  } else if (model_class == "gee") {
    prediction_data <- get_predictions_gee(model, terms, ...)
  } else if (model_class %in% c("multinom", "bracl", "brmultinom", "multinom_weightit", "ordinal_weightit")) {
    prediction_data <- get_predictions_multinom(model, data_grid, ci_level, linv, value_adjustment, terms, model_class, ...) # nolint
  } else if (model_class == "clmm") {
    prediction_data <- get_predictions_clmm(model, terms, value_adjustment, condition, ci_level, linv, ...)
  } else if (model_class == "clm") {
    prediction_data <- get_predictions_clm(model, data_grid, ci_level, linv, ...)
  } else if (model_class == "clm2") {
    prediction_data <- get_predictions_clm2(model, data_grid, ci_level, linv, ...)
  } else if (model_class == "Zelig-relogit") {
    prediction_data <- get_predictions_zelig(model, data_grid, ci_level, linv, ...)
  } else if (model_class == "mixor") {
    prediction_data <- get_predictions_mixor(model, data_grid, ci_level, linv, value_adjustment, terms, model_class, condition, interval, ...) # nolint
  } else if (model_class %in% c("zeroinfl", "hurdle", "zerotrunc")) {
    prediction_data <- get_predictions_zeroinfl(model, data_grid, ci_level, linv, type, model_class, value_adjustment, terms, vcov, vcov_args, condition, interval, verbose = verbose, ...) # nolint
  } else if (model_class == "rq") {
    prediction_data <- get_predictions_rq(model, data_grid, ci_level, ...)
  } else if (model_class == "rqs") {
    prediction_data <- get_predictions_rqs(model, data_grid, ci_level, ...)
  } else if (model_class == "lmrob") {
    prediction_data <- get_predictions_lmrob_base(model, data_grid, ci_level, ...)
  } else if (model_class == "glmrob") {
    prediction_data <- get_predictions_glmrob_base(model, data_grid, ci_level, linv, ...)
  } else if (model_class == "glmRob") {
    prediction_data <- get_predictions_glmRob(model, data_grid, ci_level, linv, value_adjustment, model_class, terms, vcov, vcov_args, condition, interval, ...) # nolint
  } else if (model_class == "logistf") {
    prediction_data <- get_predictions_logistf(model, data_grid, ci_level, linv, ...)
  } else if (model_class == "mblogit") {
    prediction_data <- get_predictions_mblogit(model, data_grid, ci_level, linv, ...)
  } else if (model_class == "mclogit") {
    prediction_data <- get_predictions_mclogit(model, data_grid, ci_level, model_class, value_adjustment, terms, vcov, vcov_args, condition, ...) # nolint
  } else if (model_class == "mlogit") {
    prediction_data <- get_predictions_mlogit(model, data_grid, ...)
  } else if (model_class == "MixMod") {
    prediction_data <- get_predictions_MixMod(model, data_grid, ci_level, linv, type, terms, value_adjustment, condition, bias_correction = bias_correction, ...) # nolint
  } else if (model_class == "MCMCglmm") {
    prediction_data <- get_predictions_MCMCglmm(model, data_grid, ci_level, interval, terms, value_adjustment, condition, ...) # nolint
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
