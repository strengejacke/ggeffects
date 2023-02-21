.get_model_function <- function(model) {
  # check class of fitted model

  lm_models <- c(
    "wblm", "wbm", "biglm", "speedlm", "gls", "ols", "ivreg", "gee", "plm", "lm",
    "rq", "rqss", "lmRob", "lm_robust", "lme", "truncreg", "nlmerMod", "lmerMod",
    "merModLmerTest", "rlmerMod", "bayesx", "mclogit"
  )

  info <- insight::model_info(model)
  if (insight::is_multivariate(model)) {
    info <- info[[1]]
  }

  if (inherits(model, lm_models) && !inherits(model, "glm"))
    return("lm")
  else if (inherits(model, "coxph"))
    return("coxph")
  else if (inherits(model, "betareg"))
    return("betareg")
  else if (info$is_linear)
    return("lm")
  else
    return("glm")
}

get_predict_function <- function(model) {
  if (inherits(model, c("wblm", "wbm"))) return("wbm")
  else if (inherits(model, "mclogit")) return("mclogit")
  else if (inherits(model, "logitr")) return("logitr")
  else if (inherits(model, "averaging")) return("averaging")
  else if (inherits(model, "orm")) return("orm")
  else if (inherits(model, "mlogit")) return("mlogit")
  else if (inherits(model, "glimML")) return("glimML")
  else if (inherits(model, "cgam")) return("cgam")
  else if (inherits(model, "ols")) return("ols")
  else if (inherits(model, "mixor")) return("mixor")
  else if (inherits(model, "glmx")) return("glmx")
  else if (inherits(model, "lrm")) return("lrm")
  else if (inherits(model, "lmrob")) return("lmrob")
  else if (inherits(model, "feglm")) return("feglm")
  else if (inherits(model, "glmrob")) return("glmrob")
  else if (inherits(model, "glmRob")) return("glmRob")
  else if (inherits(model, "brglm")) return("glm")
  else if (inherits(model, "bigglm")) return("glm")
  else if (inherits(model, "biglm")) return("lm")
  else if (inherits(model, "speedglm")) return("glm")
  else if (inherits(model, "speedlm")) return("lm")
  else if (inherits(model, "svyglm.nb")) return("svyglm.nb")
  else if (inherits(model, "svyglm")) return("svyglm")
  else if (inherits(model, "stanreg")) return("stanreg")
  else if (inherits(model, "brmsfit")) return("brmsfit")
  else if (inherits(model, "bayesx")) return("bayesx")
  else if (inherits(model, "gamlss")) return("gamlss")
  else if (inherits(model, "bamlss")) return("bamlss")
  else if (inherits(model, "gam")) return("gam")
  else if (inherits(model, c("tobit", "survreg"))) return("tobit")
  else if (inherits(model, "Gam")) return("Gam")
  else if (inherits(model, "MCMCglmm")) return("MCMCglmm")
  else if (inherits(model, "glmerMod")) return("glmer")
  else if (inherits(model, "glmmTMB")) return("glmmTMB")
  else if (inherits(model, "nlmerMod")) return("nlmer")
  else if (inherits(model, c("lmerMod", "merModLmerTest", "rlmerMod"))) return("lmer")
  else if (inherits(model, "lme")) return("lme")
  else if (inherits(model, "logistf")) return("logistf")
  else if (inherits(model, "ivreg")) return("ivreg")
  else if (inherits(model, "fixest")) return("fixest")
  else if (inherits(model, "gls")) return("gls")
  else if (inherits(model, "geeglm")) return("geeglm")
  else if (inherits(model, "clmm")) return("clmm")
  else if (inherits(model, "clm")) return("clm")
  else if (inherits(model, "clm2")) return("clm2")
  else if (inherits(model, "polr")) return("polr")
  else if (inherits(model, c("rq", "rqss"))) return("rq")
  else if (inherits(model, "gee")) return("gee")
  else if (inherits(model, "plm")) return("plm")
  else if (inherits(model, "negbin")) return("glm.nb")
  else if (inherits(model, "vgam")) return("vgam")
  else if (inherits(model, "vglm")) return("vglm")
  else if (inherits(model, "lm_robust")) return("lm")
  else if (inherits(model, "lmrob")) return("lm")
  else if (inherits(model, "lmRob")) return("lm")
  else if (inherits(model, "betareg")) return("betareg")
  else if (inherits(model, "truncreg")) return("truncreg")
  else if (inherits(model, "coxph")) return("coxph")
  else if (inherits(model, "brmultinom")) return("brmultinom")
  else if (inherits(model, "multinom")) return("multinom")
  else if (inherits(model, "bracl")) return("bracl")
  else if (inherits(model, "Zelig-relogit")) return("Zelig-relogit")
  else if (inherits(model, "zerotrunc")) return("zerotrunc")
  else if (inherits(model, "zeroinfl")) return("zeroinfl")
  else if (inherits(model, "hurdle")) return("hurdle")
  else if (inherits(model, "MixMod")) return("MixMod")
  else if (inherits(model, "glm")) return("glm")
  else if (inherits(model, "lm")) return("lm")
  else return("generic")
}