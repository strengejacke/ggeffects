.sanitize_type_argument <- function(model, type = NULL, verbose = TRUE) {
  # do nothing here...
  if (is.null(type) || is.null(model)) {
    return(NULL)
  }

  # do nothing for unrecognized model classes
  model_class <- class(model)[1]
  if (!model_class %in% .typedic$class) {
    return(type)
  }

  # if "type" is no valid type, return most common valid type
  valid_types <- .typedic$type[.typedic$class == model_class]
  if (!type %in% valid_types) {
    if (verbose) {
      insight::format_alert(
        paste0(
          "\"", type, "\" is no valid option for the `scale` argument.",
          " Changing to the supported \"", valid_types[1], "\"-type now."
        )
      )
    }
    return(valid_types[1])
  }

  # we have a valid type here
  return(type)
}


.typedic <- data.frame(class = c(
  "other", "other", "other",
  "bam", "bam", "bart", "bart", "betareg", "betareg", "betareg",
  "betareg", "betareg", "bife", "bife", "bracl", "brglmFit", "brglmFit",
  "brmsfit", "brmsfit", "brmsfit", "brmsfit", "brmultinom", "brmultinom",
  "clm", "clm", "clm", "clogit", "clogit", "clogit", "clogit",
  "coxph", "coxph", "coxph", "coxph", "crch", "crch", "crch", "crch",
  "hetprob", "hetprob", "hxlr", "hxlr", "hxlr", "hxlr", "ivpml",
  "ivpml", "fixest", "fixest", "fixest", "hurdle", "hurdle", "hurdle",
  "hurdle", "iv_robust", "lm", "gam", "gam", "Gam", "Gam", "Gam",
  "geeglm", "geeglm", "Gls", "glimML", "glimML", "glm", "glm",
  "glm", "glmerMod", "glmerMod", "glmrob", "glmrob", "glmmTMB",
  "glmmTMB", "glmmTMB", "glmmTMB", "glmmTMB", "glmmTMB", "glmmPQL",
  "glmmPQL", "glmx", "ivreg", "lmerMod", "lmerModLmerTest", "lmrob",
  "lm_robust", "lrm", "lrm", "lrm", "mblogit", "mblogit", "mblogit",
  "mclogit", "mclogit", "mclogit", "MCMCglmm", "model_fit", "model_fit",
  "model_fit", "workflow", "workflow", "workflow", "multinom",
  "multinom", "mhurdle", "mhurdle", "mhurdle", "mlogit", "negbin",
  "negbin", "negbin", "ols", "\"oohbchoice\"", "\"oohbchoice\"",
  "orm", "orm", "orm", "polr", "rlm", "selection", "selection",
  "selection", "speedlm", "speedglm", "speedglm", "stanreg", "stanreg",
  "survreg", "survreg", "survreg", "svyglm", "svyglm", "svyolr",
  "tobit", "tobit1", "tobit1", "tobit1", "zeroinfl", "zeroinfl",
  "zeroinfl", "zeroinfl"
), type = c(
  "response", "class",
  "link", "response", "link", "ev", "ppd", "response", "link",
  "precision", "quantile", "variance", "response", "link", "probs",
  "response", "link", "response", "link", "prediction", "average",
  "probs", "class", "prob", "cum.prob", "linear.predictor", "expected",
  "lp", "risk", "survival", "expected", "lp", "risk", "survival",
  "response", "location", "scale", "density", "pr", "xb", "location",
  "cumprob", "scale", "density", "pr", "xb", "invlink(link)", "response",
  "link", "response", "prob", "count", "zero", "response", "response",
  "response", "link", "invlink(link)", "response", "link", "response",
  "link", "lp", "response", "link", "invlink(link)", "response",
  "link", "response", "link", "response", "link", "response", "link",
  "conditional", "zprob", "zlink", "disp", "response", "link",
  "response", "response", "response", "response", "response", "response",
  "fitted", "lp", "mean", "response", "latent", "link", "response",
  "latent", "link", "response", "numeric", "prob", "class", "numeric",
  "prob", "class", "probs", "latent", "E", "Ep", "p", "response",
  "invlink(link)", "response", "link", "lp", " \"probability\"",
  " \"utility\"", "fitted", "mean", "lp", "probs", "response",
  "response", "link", "unconditional", "response", "response",
  "link", "response", "link", "response", "link", "quantile", "response",
  "link", "probs", "response", "expvalue", "linpred", "prob", "response",
  "prob", "count", "zero"
), stringsAsFactors = FALSE)
