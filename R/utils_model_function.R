get_predict_function <- function(model) {
  if (inherits(model, c("wblm", "wbm"))) {
    "wbm"
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
  } else if (inherits(model, "lrm")) {
    "lrm"
  } else if (inherits(model, "lmrob")) {
    "lmrob"
  } else if (inherits(model, "glmrob")) {
    "glmrob"
  } else if (inherits(model, "glmRob")) {
    "glmRob"
  } else if (inherits(model, "svyglm.nb")) {
    "svyglm.nb"
  } else if (inherits(model, "stanreg")) {
    "stanreg"
  } else if (inherits(model, "brmsfit")) {
    "brmsfit"
  } else if (inherits(model, "gamlss")) {
    "gamlss"
  } else if (inherits(model, "gam")) {
    "gam"
  } else if (inherits(model, c("tobit", "survreg"))) {
    "tobit"
  } else if (inherits(model, "Gam")) {
    "Gam"
  } else if (inherits(model, "MCMCglmm")) {
    "MCMCglmm"
  } else if (inherits(model, "sdmTMB")) {
    "sdmTMB"
  } else if (inherits(model, "lme")) {
    "lme"
  } else if (inherits(model, c("logistf", "flic", "flac"))) {
    "logistf"
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
  } else if (inherits(model, "rqs")) {
    "rqs"
  } else if (inherits(model, c("rq", "rqss"))) {
    "rq"
  } else if (inherits(model, "gee")) {
    "gee"
  } else if (inherits(model, "plm")) {
    "plm"
  } else if (inherits(model, "vglm")) {
    "vglm"
  } else if (inherits(model, "coxph")) {
    "coxph"
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
  } else {
    "generic"
  }
}
