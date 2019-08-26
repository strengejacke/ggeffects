# select prediction method, based on model-object
#' @importFrom sjmisc add_variables
#' @importFrom insight find_response get_response get_data model_info link_inverse is_multivariate
select_prediction_method <- function(model.class,
                                     model,
                                     expanded_frame,
                                     ci.lvl,
                                     type,
                                     faminfo,
                                     ppd,
                                     terms,
                                     typical,
                                     vcov.fun,
                                     vcov.type,
                                     vcov.args,
                                     condition,
                                     interval,
                                     ...) {
  # get link-inverse-function
  linv <- insight::link_inverse(model)
  if (is.null(linv)) linv <- function(x) x

  if (model.class == "svyglm") {
    fitfram <- get_predictions_svyglm(model, expanded_frame, ci.lvl, linv, ...)
  } else if (model.class == "svyglm.nb") {
    fitfram <- get_predictions_svyglmnb(model, expanded_frame, ci.lvl, linv, model.class, typical, terms, vcov.fun, vcov.type, vcov.args, condition, interval, ...)
  } else if (model.class == "stanreg") {
    fitfram <- get_predictions_stan(model, expanded_frame, ci.lvl, type, faminfo, ppd, terms, ...)
  } else if (model.class == "brmsfit") {
    fitfram <- get_predictions_stan(model, expanded_frame, ci.lvl, type, faminfo, ppd, terms, ...)
  } else if (model.class == "coxph" && type != "surv" && type != "cumhaz") {
    fitfram <- get_predictions_coxph(model, expanded_frame, ci.lvl, typical, model.class, vcov.fun, vcov.type, vcov.args, condition, interval, ...)
  } else if (model.class == "coxph" && type %in% c("surv", "cumhaz")) {
    fitfram <- get_predictions_survival(model, expanded_frame, ci.lvl, type, terms, ...)
  } else if (model.class == "ols") {
    fitfram <- get_predictions_ols(model, expanded_frame, ci.lvl, ...)
  } else if (model.class == "lrm") {
    fitfram <- get_predictions_lrm(model, expanded_frame, ci.lvl, linv, ...)
  } else if (model.class == "glimML") {
    fitfram <- get_predictions_glimML(model, expanded_frame, ci.lvl, linv, ...)
  } else if (model.class == "glmmTMB") {
    fitfram <- get_predictions_glmmTMB(model, expanded_frame, ci.lvl, linv, type, terms, typical, condition, ...)
  } else if (model.class == "wbm") {
    fitfram <- get_predictions_wbm(model, expanded_frame, ci.lvl, linv, type, terms, typical, condition, ...)
  } else if (model.class %in% c("lmer", "nlmer", "glmer")) {
    fitfram <- get_predictions_merMod(model, expanded_frame, ci.lvl, linv, type, terms, typical, condition, ...)
  } else if (model.class == "geeglm") {
    fitfram <- get_predictions_geeglm(model, expanded_frame, ...)
  } else if (model.class == "gamlss") {
    fitfram <- get_predictions_gamlss(model, expanded_frame, ci.lvl, linv, terms, model.class, typical, condition, ...)
  } else if (model.class == "gam") {
    fitfram <- get_predictions_gam(model, expanded_frame, ci.lvl, linv, type, ...)
  } else if (model.class == "Gam") {
    fitfram <- get_predictions_Gam(model, expanded_frame, ci.lvl, linv, typical, terms, model.class, condition, ...)
  # } else if (model.class == "vgam") {
  # fitfram <- get_predictions_vgam(model, expanded_frame, ci.lvl, linv, ...)
  } else if (model.class == "vglm") {
    fitfram <- get_predictions_vglm(model, expanded_frame, ci.lvl, linv, ...)
  } else if (model.class == "tobit") {
    fitfram <- get_predictions_tobit(model, expanded_frame, ci.lvl, linv, ...)
  } else if (model.class %in% c("lme", "gls", "plm")) {
    fitfram <- get_predictions_lme(model, expanded_frame, ci.lvl, linv, type, terms, typical, model.class, vcov.fun, vcov.type, vcov.args, condition, ...)
  } else if (model.class == "gee") {
    fitfram <- get_predictions_gee(model, terms, ...)
  } else if (model.class == "multinom") {
    fitfram <- get_predictions_multinom(model, expanded_frame, ci.lvl, linv, typical, terms, model.class, ...)
  } else if (model.class == "clmm") {
    fitfram <- get_predictions_clmm(model, terms, typical, condition, ci.lvl, linv, ...)
  } else if (model.class == "clm") {
    fitfram <- get_predictions_clm(model, expanded_frame, ci.lvl, linv, ...)
  } else if (model.class == "clm2") {
    fitfram <- get_predictions_clm2(model, expanded_frame, ci.lvl, linv, ...)
  } else if (model.class == "Zelig-relogit") {
    fitfram <- get_predictions_zelig(model, expanded_frame, ci.lvl, linv, ...)
  } else if (model.class == "polr") {
    fitfram <- get_predictions_polr(model, expanded_frame, ci.lvl, linv, typical, terms, model.class, vcov.fun, vcov.type, vcov.args, condition, interval, ...)
  } else if (model.class %in% c("betareg", "truncreg", "ivreg", "vgam")) {
    fitfram <- get_predictions_generic2(model, expanded_frame, ci.lvl, linv, type, model.class, typical, terms, vcov.fun, vcov.type, vcov.args, condition, interval, ...)
  } else if (model.class %in% c("zeroinfl", "hurdle", "zerotrunc")) {
    fitfram <- get_predictions_zeroinfl(model, expanded_frame, ci.lvl, linv, type, model.class, typical, terms, vcov.fun, vcov.type, vcov.args, condition, ...)
  } else if (model.class %in% c("glm", "glm.nb")) {
    fitfram <- get_predictions_glm(model, expanded_frame, ci.lvl, linv, typical, model.class, terms, vcov.fun, vcov.type, vcov.args, condition, interval, ...)
  } else if (model.class %in% c("rq")) {
    fitfram <- get_predictions_rq(model, expanded_frame, ci.lvl, ...)
  } else if (model.class %in% c("lmrob")) {
    fitfram <- get_predictions_lmrob_base(model, expanded_frame, ci.lvl, ...)
  } else if (model.class %in% c("glmrob")) {
    fitfram <- get_predictions_glmrob_base(model, expanded_frame, ci.lvl, linv, ...)
  } else if (model.class %in% c("glmRob")) {
    fitfram <- get_predictions_glmRob(model, expanded_frame, ci.lvl, linv, typical, model.class, terms, vcov.fun, vcov.type, vcov.args, condition, interval, ...)
  } else if (model.class == "logistf") {
    fitfram <- get_predictions_logistf(model, expanded_frame, terms, ...)
  } else if (model.class == "lm") {
    fitfram <- get_predictions_lm(model, expanded_frame, ci.lvl, model.class, typical, terms, vcov.fun, vcov.type, vcov.args, condition, interval, ...)
  } else if (model.class == "MixMod") {
    fitfram <- get_predictions_MixMod(model, expanded_frame, ci.lvl, linv, type, terms, typical, condition, ...)
  } else if (model.class == "MCMCglmm") {
    fitfram <- get_predictions_MCMCglmm(model, expanded_frame, ci.lvl, interval, ...)
  } else {
    fitfram <- get_predictions_generic(model, expanded_frame, linv, ...)
  }

  fitfram
}



.get_base_fitfram <- function(model, fitfram, linv, prdat, se, ci.lvl, model.class, typical, terms, vcov.fun, vcov.type, vcov.args, condition = NULL, interval = NULL) {

  # compute ci, two-ways

  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975


  # copy predictions

  if (typeof(prdat) == "double")
    .predicted <- prdat
  else
    .predicted <- prdat$fit


  # get standard errors, if computed

  if (obj_has_name(prdat, "se.fit")) {
    se.fit <- prdat$se.fit
    # reset interval, since we have normal confidence intervals already here
    if (interval == "confidence") interval <- NULL
  } else {
    se.fit <- NULL
  }

  # get predicted values, on link-scale
  fitfram$predicted <- .predicted

  # did user request robust standard errors?

  if (!is.null(vcov.fun) || (!is.null(interval) && se)) {
    se.pred <-
      .get_se_from_vcov(
        model = model,
        fitfram = fitfram,
        typical = typical,
        terms = terms,
        model.class = model.class,
        vcov.fun = vcov.fun,
        vcov.type = vcov.type,
        vcov.args = vcov.args,
        condition = condition,
        interval = interval
      )

    if (!is.null(se.pred)) {
      fitfram <- se.pred$fitfram
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
    fitfram$conf.low <- linv(fitfram$predicted - stats::qnorm(ci) * se.fit)
    fitfram$conf.high <- linv(fitfram$predicted + stats::qnorm(ci) * se.fit)
    # copy standard errors
    attr(fitfram, "std.error") <- se.fit
    if (!is.null(se.pred))
      attr(fitfram, "prediction.interval") <- attr(se.pred, "prediction_interval")
  } else {
    # No CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  # transform predicted values
  fitfram$predicted <- linv(fitfram$predicted)

  fitfram
}
