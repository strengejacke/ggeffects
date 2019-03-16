# select prediction method, based on model-object
#' @importFrom sjmisc add_variables
#' @importFrom insight find_response get_response get_data model_info link_inverse is_multivariate
select_prediction_method <- function(fun,
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
                                     ...) {
  # get link-inverse-function
  linv <- insight::link_inverse(model)
  if (is.null(linv)) linv <- function(x) x

  if (fun == "svyglm") {
    # survey-objects -----
    fitfram <- get_predictions_svyglm(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "svyglm.nb") {
    # survey-glm.nb-objects -----
    fitfram <- get_predictions_svyglmnb(model, expanded_frame, ci.lvl, linv, fun, typical, terms, vcov.fun, vcov.type, vcov.args, condition, ...)
  } else if (fun == "stanreg") {
    # stan-objects -----
    fitfram <- get_predictions_stan(model, expanded_frame, ci.lvl, type, faminfo, ppd, terms, ...)
  } else if (fun == "brmsfit") {
    # brms-objects -----
    fitfram <- get_predictions_stan(model, expanded_frame, ci.lvl, type, faminfo, ppd, terms, ...)
  } else if (fun == "coxph" && type != "surv" && type != "cumhaz") {
    # coxph-objects -----
    fitfram <- get_predictions_coxph(model, expanded_frame, ci.lvl, ...)
  } else if (fun == "coxph" && type %in% c("surv", "cumhaz")) {
    # coxph-objects -----
    fitfram <- get_predictions_survival(model, expanded_frame, ci.lvl, type, terms, ...)
  } else if (fun == "lrm") {
    # lrm-objects -----
    fitfram <- get_predictions_lrm(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "glmmTMB") {
    # glmmTMB-objects -----
    fitfram <- get_predictions_glmmTMB(model, expanded_frame, ci.lvl, linv, type, terms, typical, condition, ...)
  } else if (fun %in% c("lmer", "nlmer", "glmer")) {
    # merMod-objects  -----
    fitfram <- get_predictions_merMod(model, expanded_frame, ci.lvl, linv, type, terms, typical, condition, ...)
  } else if (fun == "gam") {
    # gam-objects -----
    fitfram <- get_predictions_gam(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "Gam") {
    # Gam-objects -----
    fitfram <- get_predictions_Gam(model, expanded_frame, ci.lvl, linv, typical, terms, fun, condition, ...)
  # } else if (fun == "vgam") {
  # vgam-objects -----
  # fitfram <- get_predictions_vgam(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "vglm") {
    # vgam-objects -----
    fitfram <- get_predictions_vglm(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun %in% c("lme", "gls", "plm")) {
    # lme-objects -----
    fitfram <- get_predictions_lme(model, expanded_frame, ci.lvl, linv, type, terms, typical, condition, ...)
  } else if (fun == "gee") {
    # gee-objects -----
    fitfram <- get_predictions_gee(model, terms, ...)
  } else if (fun == "multinom") {
    # multinom-objects -----
    fitfram <- get_predictions_multinom(model, expanded_frame, ci.lvl, linv, typical, terms, fun, ...)
  } else if (fun == "clmm") {
    # clmm-objects -----
    fitfram <- get_predictions_clmm(model, terms, typical, condition, ci.lvl, linv, ...)
  } else if (fun == "clm") {
    # clm-objects -----
    fitfram <- get_predictions_clm(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "clm2") {
    # clm2-objects -----
    fitfram <- get_predictions_clm2(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "Zelig-relogit") {
    # Zelig-relogit-objects -----
    fitfram <- get_predictions_zelig(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "polr") {
    # polr-objects -----
    fitfram <- get_predictions_polr(model, expanded_frame, ci.lvl, linv, typical, terms, fun, condition, ...)
  } else if (fun %in% c("betareg", "truncreg", "ivreg", "vgam")) {
    # betareg, truncreg-objects -----
    fitfram <- get_predictions_generic2(model, expanded_frame, ci.lvl, linv, type, fun, typical, terms, vcov.fun, vcov.type, vcov.args, condition, ...)
  } else if (fun %in% c("zeroinfl", "hurdle", "zerotrunc")) {
    # zeroinfl and hurdle-objects -----
    fitfram <- get_predictions_zeroinfl(model, expanded_frame, ci.lvl, linv, type, fun, typical, terms, vcov.fun, vcov.type, vcov.args, condition, ...)
  } else if (fun %in% c("glm", "glm.nb", "glmRob")) {
    # glm-objects -----
    fitfram <- get_predictions_glm(model, expanded_frame, ci.lvl, linv, typical, fun, terms, vcov.fun, vcov.type, vcov.args, condition, ...)
  } else if (fun == "lm") {
    # lm-objects -----
    fitfram <- get_predictions_lm(model, expanded_frame, ci.lvl, fun, typical, terms, vcov.fun, vcov.type, vcov.args, condition, ...)
  } else if (fun == "MixMod") {
    # MixMod-objects -----
    fitfram <- get_predictions_MixMod(model, expanded_frame, ci.lvl, linv, type, terms, typical, condition, ...)
  } else if (fun == "MCMCglmm") {
    # MCMCglmm-objects -----
    fitfram <- get_predictions_MCMCglmm(model, expanded_frame, ci.lvl, ...)
  } else {
    # general-objects -----
    fitfram <- get_predictions_generic(model, expanded_frame, linv, ...)
  }

  fitfram
}


get_base_fitfram <- function(model, fitfram, linv, prdat, se, ci.lvl, fun, typical, terms, vcov.fun, vcov.type, vcov.args, condition = NULL) {

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

  if (obj_has_name(prdat, "se.fit"))
    se.fit <- prdat$se.fit
  else
    se.fit <- NULL

  # get predicted values, on link-scale
  fitfram$predicted <- .predicted

  # did user request robust standard errors?

  if (!is.null(vcov.fun)) {
    se.pred <-
      get_se_from_vcov(
        model = model,
        fitfram = fitfram,
        typical = typical,
        terms = terms,
        fun = fun,
        vcov.fun = vcov.fun,
        vcov.type = vcov.type,
        vcov.args = vcov.args,
        condition = condition
      )

    if (!is.null(se.pred)) {
      fitfram <- se.pred$fitfram
      se.fit <- se.pred$se.fit
      se <- TRUE
    } else {
      se.fit <- NULL
      se <- FALSE
    }
  }


  # did user request standard errors? if yes, compute CI

  if (se && !is.null(se.fit)) {
    fitfram$conf.low <- linv(fitfram$predicted - stats::qnorm(ci) * se.fit)
    fitfram$conf.high <- linv(fitfram$predicted + stats::qnorm(ci) * se.fit)
    # copy standard errors
    attr(fitfram, "std.error") <- se.fit
  } else {
    # No CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  # transform predicted values
  fitfram$predicted <- linv(fitfram$predicted)

  fitfram
}
