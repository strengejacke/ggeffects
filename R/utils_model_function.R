.get_model_function <- function(model) {
  # check class of fitted model

  lm_models <- c(
    "wblm", "wbm", "biglm", "speedlm", "gls", "ols", "ivreg", "gee", "plm", "lm",
    "rqss", "lmRob", "lm_robust", "lme", "truncreg", "nlmerMod",
    "lmerMod", "merModLmerTest", "rlmerMod", "bayesx", "mclogit"
  )

  info <- insight::model_info(model, verbose = FALSE)
  if (insight::is_multivariate(model) && !inherits(model, c("vglm", "vgam"))) {
    info <- info[[1]]
  }

  if (inherits(model, lm_models) && !inherits(model, "glm")) {
    "lm"
  } else if (inherits(model, "coxph")) {
    "coxph"
  } else if (inherits(model, "betareg")) {
    "betareg"
  } else if (isTRUE(info$is_linear)) {
    "lm"
  } else {
    "glm"
  }
}


get_predict_function <- function(model) {
  if (inherits(model, c("wblm", "wbm"))) {
    "wbm"
  } else if (inherits(model, c("phylolm", "phyloglm"))) {
    "phylolm"
  } else if (inherits(model, "nestedLogit")) {
    "nestedLogit"
  } else if (inherits(model, "mblogit")) {
    "mblogit"
  } else if (inherits(model, "glmgee")) {
    "glmgee"
  } else if (inherits(model, "mclogit")) {
    "mclogit"
  } else if (inherits(model, "logitr")) {
    "logitr"
  } else if (inherits(model, "averaging")) {
    "averaging"
  } else if (inherits(model, "orm")) {
    "orm"
  } else if (inherits(model, "mlogit")) {
    "mlogit"
  } else if (inherits(model, "glimML")) {
    "glimML"
  } else if (inherits(model, "cgam")) {
    "cgam"
  } else if (inherits(model, "ols")) {
    "ols"
  } else if (inherits(model, "mixor")) {
    "mixor"
  } else if (inherits(model, "glmx")) {
    "glmx"
  } else if (inherits(model, "lrm")) {
    "lrm"
  } else if (inherits(model, "lmrob")) {
    "lmrob"
  } else if (inherits(model, "feglm")) {
    "feglm"
  } else if (inherits(model, "glmrob")) {
    "glmrob"
  } else if (inherits(model, "glmRob")) {
    "glmRob"
  } else if (inherits(model, "brglm")) {
    "glm"
  } else if (inherits(model, "bigglm")) {
    "glm"
  } else if (inherits(model, "biglm")) {
    "lm"
  } else if (inherits(model, "speedglm")) {
    "glm"
  } else if (inherits(model, "speedlm")) {
    "lm"
  } else if (inherits(model, "svyglm.nb")) {
    "svyglm.nb"
  } else if (inherits(model, "svyglm")) {
    "svyglm"
  } else if (inherits(model, "stanreg")) {
    "stanreg"
  } else if (inherits(model, "brmsfit")) {
    "brmsfit"
  } else if (inherits(model, "bayesx")) {
    "bayesx"
  } else if (inherits(model, "gamlss")) {
    "gamlss"
  } else if (inherits(model, "bamlss")) {
    "bamlss"
  } else if (inherits(model, "gam")) {
    "gam"
  } else if (inherits(model, c("tobit", "survreg"))) {
    "tobit"
  } else if (inherits(model, "Gam")) {
    "Gam"
  } else if (inherits(model, "MCMCglmm")) {
    "MCMCglmm"
  } else if (inherits(model, "glmerMod")) {
    "glmer"
  } else if (inherits(model, "sdmTMB")) {
    "sdmTMB"
  } else if (inherits(model, "glmmTMB")) {
    "glmmTMB"
  } else if (inherits(model, "nlmerMod")) {
    "nlmer"
  } else if (inherits(model, c("lmerMod", "merModLmerTest", "rlmerMod"))) {
    "lmer"
  } else if (inherits(model, "lme")) {
    "lme"
  } else if (inherits(model, c("logistf", "flic", "flac"))) {
    "logistf"
  } else if (inherits(model, "ivreg")) {
    "ivreg"
  } else if (inherits(model, "fixest")) {
    "fixest"
  } else if (inherits(model, "gls")) {
    "gls"
  } else if (inherits(model, "geeglm")) {
    "geeglm"
  } else if (inherits(model, "clmm")) {
    "clmm"
  } else if (inherits(model, "clm")) {
    "clm"
  } else if (inherits(model, "clm2")) {
    "clm2"
  } else if (inherits(model, "polr")) {
    "polr"
  } else if (inherits(model, "rqs")) {
    "rqs"
  } else if (inherits(model, c("rq", "rqss"))) {
    "rq"
  } else if (inherits(model, "gee")) {
    "gee"
  } else if (inherits(model, "plm")) {
    "plm"
  } else if (inherits(model, "negbin")) {
    "glm.nb"
  } else if (inherits(model, "vgam")) {
    "vgam"
  } else if (inherits(model, "vglm")) {
    "vglm"
  } else if (inherits(model, "lm_robust")) {
    "lm"
  } else if (inherits(model, "lmrob")) {
    "lm"
  } else if (inherits(model, "lmRob")) {
    "lm"
  } else if (inherits(model, "betareg")) {
    "betareg"
  } else if (inherits(model, "truncreg")) {
    "truncreg"
  } else if (inherits(model, "coxph")) {
    "coxph"
  } else if (inherits(model, "brmultinom")) {
    "brmultinom"
  } else if (inherits(model, "multinom")) {
    "multinom"
  } else if (inherits(model, "bracl")) {
    "bracl"
  } else if (inherits(model, "Zelig-relogit")) {
    "Zelig-relogit"
  } else if (inherits(model, "zerotrunc")) {
    "zerotrunc"
  } else if (inherits(model, "zeroinfl")) {
    "zeroinfl"
  } else if (inherits(model, "hurdle")) {
    "hurdle"
  } else if (inherits(model, "MixMod")) {
    "MixMod"
  } else if (inherits(model, "glm")) {
    "glm"
  } else if (inherits(model, "lm")) {
    "lm"
  } else {
    "generic"
  }
}
