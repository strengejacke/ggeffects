# select prediction method, based on model-object
select_prediction_method <- function(model_class,
                                     model,
                                     data_grid,
                                     ci.lvl,
                                     type,
                                     model_info,
                                     ppd,
                                     terms,
                                     value_adjustment,
                                     vcov.fun,
                                     vcov.type,
                                     vcov.args,
                                     condition,
                                     interval,
                                     verbose = TRUE,
                                     ...) {
  # get link-inverse-function
  linv <- insight::link_inverse(model)
  if (is.null(linv)) linv <- function(x) x

  if (model_class == "svyglm") {
    prediction_data <- get_predictions_svyglm(model, data_grid, ci.lvl, linv, ...)
  } else if (model_class == "svyglm.nb") {
    prediction_data <- get_predictions_svyglmnb(model, data_grid, ci.lvl, linv, model_class, value_adjustment, terms, vcov.fun, vcov.type, vcov.args, condition, interval, ...)
  } else if (model_class == "stanreg") {
    prediction_data <- get_predictions_stan(model, data_grid, ci.lvl, type, model_info, ppd, terms, verbose = verbose, ...)
  } else if (model_class == "brmsfit") {
    prediction_data <- get_predictions_stan(model, data_grid, ci.lvl, type, model_info, ppd, terms, ...)
  } else if (model_class == "coxph" && type != "surv" && type != "cumhaz") {
    prediction_data <- get_predictions_coxph(model, data_grid, ci.lvl, model_class, value_adjustment, terms, vcov.fun, vcov.type, vcov.args, condition, interval, ...)
  } else if (model_class == "coxph" && type %in% c("surv", "cumhaz")) {
    prediction_data <- get_predictions_survival(model, data_grid, ci.lvl, type, terms, ...)
  } else if (model_class == "ols") {
    prediction_data <- get_predictions_ols(model, data_grid, ci.lvl, ...)
  } else if (model_class == "logitr") {
    prediction_data <- get_predictions_logitr(model, data_grid, ci.lvl, ...)
  } else if (model_class %in% c("lrm", "orm")) {
    prediction_data <- get_predictions_lrm(model, data_grid, ci.lvl, linv, ...)
  } else if (model_class == "glimML") {
    prediction_data <- get_predictions_glimML(model, data_grid, ci.lvl, linv, ...)
  } else if (model_class == "glmmTMB") {
    prediction_data <- get_predictions_glmmTMB(model, data_grid, ci.lvl, linv, type, terms, value_adjustment, condition, verbose = verbose, ...)
  } else if (model_class == "wbm") {
    prediction_data <- get_predictions_wbm(model, data_grid, ci.lvl, linv, type, terms, condition, ...)
  } else if (model_class %in% c("lmer", "nlmer", "glmer")) {
    prediction_data <- get_predictions_merMod(model, data_grid, ci.lvl, linv, type, terms, value_adjustment, condition, ...)
  } else if (model_class == "geeglm") {
    prediction_data <- get_predictions_geeglm(model, data_grid, ci.lvl, linv, type, model_class, value_adjustment, terms, condition, ...)
  } else if (model_class == "gamlss") {
    prediction_data <- get_predictions_gamlss(model, data_grid, ci.lvl, terms, model_class, value_adjustment, condition, ...)
  } else if (model_class == "bamlss") {
    prediction_data <- get_predictions_bamlss(model, data_grid, linv, ...)
  } else if (model_class == "bayesx") {
    prediction_data <- get_predictions_bayesx(model, data_grid, ...)
  } else if (model_class == "cgam") {
    prediction_data <- get_predictions_cgam(model, data_grid, ci.lvl, linv, value_adjustment, model_class, terms, condition, ...)
  } else if (model_class == "gam") {
    prediction_data <- get_predictions_gam(model, data_grid, ci.lvl, linv, type, ...)
  } else if (model_class == "Gam") {
    prediction_data <- get_predictions_Gam(model, data_grid, ci.lvl, linv, value_adjustment, terms, model_class, condition, ...)
  # } else if (model_class == "vgam") {
  # prediction_data <- get_predictions_vgam(model, data_grid, ci.lvl, linv, ...)
  } else if (model_class == "vglm") {
    prediction_data <- get_predictions_vglm(model, data_grid, ci.lvl, linv, ...)
  } else if (model_class == "tobit") {
    prediction_data <- get_predictions_tobit(model, data_grid, ci.lvl, linv, ...)
  } else if (model_class %in% c("lme", "gls", "plm")) {
    prediction_data <- get_predictions_lme(model, data_grid, ci.lvl, linv, type, terms, value_adjustment, model_class, vcov.fun, vcov.type, vcov.args, condition, ...)
  } else if (model_class == "gee") {
    prediction_data <- get_predictions_gee(model, terms, ...)
  } else if (model_class %in% c("multinom", "bracl", "brmultinom")) {
    prediction_data <- get_predictions_multinom(model, data_grid, ci.lvl, linv, value_adjustment, terms, model_class, ...)
  } else if (model_class == "clmm") {
    prediction_data <- get_predictions_clmm(model, terms, value_adjustment, condition, ci.lvl, linv, ...)
  } else if (model_class == "clm") {
    prediction_data <- get_predictions_clm(model, data_grid, ci.lvl, linv, ...)
  } else if (model_class == "clm2") {
    prediction_data <- get_predictions_clm2(model, data_grid, ci.lvl, linv, ...)
  } else if (model_class == "Zelig-relogit") {
    prediction_data <- get_predictions_zelig(model, data_grid, ci.lvl, linv, ...)
  } else if (model_class == "mixor") {
    prediction_data <- get_predictions_mixor(model, data_grid, ci.lvl, linv, value_adjustment, terms, model_class, condition, ...)
  } else if (model_class == "polr") {
    prediction_data <- get_predictions_polr(model, data_grid, ci.lvl, linv, value_adjustment, terms, model_class, vcov.fun, vcov.type, vcov.args, condition, interval, ...)
  } else if (model_class %in% c("averaging", "betareg", "truncreg", "ivreg", "vgam", "fixest", "feglm", "glmx")) {
    prediction_data <- get_predictions_generic2(model, data_grid, ci.lvl, linv, type, model_class, value_adjustment, terms, vcov.fun, vcov.type, vcov.args, condition, interval, ...)
  } else if (model_class %in% c("zeroinfl", "hurdle", "zerotrunc")) {
    prediction_data <- get_predictions_zeroinfl(model, data_grid, ci.lvl, linv, type, model_class, value_adjustment, terms, vcov.fun, vcov.type, vcov.args, condition, interval, ...)
  } else if (model_class %in% c("glm", "glm.nb")) {
    prediction_data <- get_predictions_glm(model, data_grid, ci.lvl, linv, value_adjustment, model_class, terms, vcov.fun, vcov.type, vcov.args, condition, interval, type, ...)
  } else if (model_class %in% c("rq")) {
    prediction_data <- get_predictions_rq(model, data_grid, ci.lvl, ...)
  } else if (model_class %in% c("lmrob")) {
    prediction_data <- get_predictions_lmrob_base(model, data_grid, ci.lvl, ...)
  } else if (model_class %in% c("glmrob")) {
    prediction_data <- get_predictions_glmrob_base(model, data_grid, ci.lvl, linv, ...)
  } else if (model_class %in% c("glmRob")) {
    prediction_data <- get_predictions_glmRob(model, data_grid, ci.lvl, linv, value_adjustment, model_class, terms, vcov.fun, vcov.type, vcov.args, condition, interval, ...)
  } else if (model_class == "logistf") {
    prediction_data <- get_predictions_logistf(model, data_grid, terms, ...)
  } else if (model_class == "mblogit") {
    prediction_data <- get_predictions_mblogit(model, data_grid, ci.lvl, linv, ...)
  } else if (model_class == "phylolm") {
    prediction_data <- get_predictions_generic2(model, data_grid, ci.lvl, linv, type, model_class, value_adjustment, terms, vcov.fun, vcov.type, vcov.args, condition, interval, ...)
  } else if (model_class == "mclogit") {
    prediction_data <- get_predictions_mclogit(model, data_grid, ci.lvl, model_class, value_adjustment, terms, vcov.fun, vcov.type, vcov.args, condition, ...)
  } else if (model_class == "mlogit") {
    prediction_data <- get_predictions_mlogit(model, data_grid, ...)
  } else if (model_class == "lm") {
    prediction_data <- get_predictions_lm(model, data_grid, ci.lvl, model_class, value_adjustment, terms, vcov.fun, vcov.type, vcov.args, condition, interval, type, ...)
  } else if (model_class == "MixMod") {
    prediction_data <- get_predictions_MixMod(model, data_grid, ci.lvl, linv, type, terms, value_adjustment, condition, ...)
  } else if (model_class == "MCMCglmm") {
    prediction_data <- get_predictions_MCMCglmm(model, data_grid, ci.lvl, interval, terms, value_adjustment, condition, ...)
  } else {
    prediction_data <- get_predictions_generic(model, data_grid, ci.lvl, linv, ...)
  }

  prediction_data
}


.get_df <- function(model) {
  dof <- .safe(unique(insight::get_df(model, type = "wald")), Inf)
  if (length(dof) > 1) {
    dof <- Inf
  }
  dof
}


.generic_prediction_data <- function(model, data_grid, linv, prdat, se, ci.lvl, model_class, value_adjustment, terms, vcov.fun, vcov.type, vcov.args, condition = NULL, interval = NULL) {

  # compute ci, two-ways

  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- 0.975

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  # copy predictions

  if (typeof(prdat) == "double")
    .predicted <- prdat
  else
    .predicted <- prdat$fit


  # get standard errors, if computed

  if (.obj_has_name(prdat, "se.fit")) {
    se.fit <- prdat$se.fit
    # reset interval, since we have normal confidence intervals already here
    if (interval == "confidence") interval <- NULL
  } else {
    se.fit <- NULL
  }

  # get predicted values, on link-scale
  data_grid$predicted <- .predicted

  # did user request robust standard errors?

  if (!is.null(vcov.fun) || (!is.null(interval) && se)) {
    se.pred <-
      .standard_error_predictions(
        model = model,
        prediction_data = data_grid,
        value_adjustment = value_adjustment,
        terms = terms,
        model_class = model_class,
        vcov.fun = vcov.fun,
        vcov.type = vcov.type,
        vcov.args = vcov.args,
        condition = condition,
        interval = interval
      )

    if (.check_returned_se(se.pred)) {
      fitfram <- se.pred$prediction_data
      se.fit <- se.pred$se.fit
      se <- TRUE
    } else {
      se.fit <- NULL
      se <- FALSE
    }
  } else {
    se.pred <- NULL
  }


  # did user request standard errors? if yes, compute CI

  if (se && !is.null(se.fit)) {
    data_grid$conf.low <- linv(data_grid$predicted - tcrit * se.fit)
    data_grid$conf.high <- linv(data_grid$predicted + tcrit * se.fit)
    # copy standard errors
    attr(data_grid, "std.error") <- se.fit
    if (!is.null(se.pred) && length(se.pred) > 0)
      attr(data_grid, "prediction.interval") <- attr(se.pred, "prediction_interval")
  } else {
    # No CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  # transform predicted values
  data_grid$predicted <- linv(data_grid$predicted)

  data_grid
}
