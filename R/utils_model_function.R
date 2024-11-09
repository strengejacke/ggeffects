get_predict_function <- function(model) {
  if (inherits(model, "glmgee")) {
    "glmgee"
  } else if (inherits(model, "logitr")) {
    "logitr"
  } else if (inherits(model, "glimML")) {
    "glimML"
  } else if (inherits(model, "lmrob")) {
    "lmrob"
  } else if (inherits(model, "glmrob")) {
    "glmrob"
  } else if (inherits(model, "glmRob")) {
    "glmRob"
  } else if (inherits(model, "gamlss")) {
    "gamlss"
  } else if (inherits(model, "sdmTMB")) {
    "sdmTMB"
  } else if (inherits(model, "geeglm")) {
    "geeglm"
  } else if (inherits(model, "clmm")) {
    "clmm"
  } else if (inherits(model, "clm2")) {
    "clm2"
  } else if (inherits(model, "rqs")) {
    "rqs"
  } else if (inherits(model, "gee")) {
    "gee"
  } else if (inherits(model, "vglm")) {
    "vglm"
  } else if (inherits(model, "Zelig-relogit")) {
    "Zelig-relogit"
  } else {
    "generic"
  }
}
