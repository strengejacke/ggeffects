# all valid "type" arguments for each model class.
# Run "marginaleffects:::type_dictionary_build()" to update this list
.typedic <- data.frame(
  class = c(
    "bam", "bam", "bart", "bart", "betareg", "betareg", "betareg",
    "betareg", "betareg", "bife", "bife", "bracl", "brglmFit", "brglmFit",
    "brmsfit", "brmsfit", "brmsfit", "brmsfit", "brmultinom", "brmultinom",
    "clm", "clm", "clm", "clogit", "clogit", "clogit", "clogit",
    "coxph", "coxph", "coxph", "coxph", "coxph_weightit", "coxph_weightit",
    "coxph_weightit", "coxph_weightit", "crch", "crch", "crch", "crch",
    "hetprob", "hetprob", "hxlr", "hxlr", "hxlr", "hxlr", "ivpml",
    "ivpml", "flexsurvreg", "flexsurvreg", "flexsurvreg", "flexsurvreg",
    "flexsurvreg", "flexsurvreg", "flexsurvreg", "flexsurvreg", "flexsurvreg",
    "fixest", "fixest", "fixest", "hurdle", "hurdle", "hurdle", "hurdle",
    "iv_robust", "lm", "gam", "gam", "Gam", "Gam", "Gam", "geeglm",
    "geeglm", "Gls", "glimML", "glimML", "glm", "glm", "glm", "glmerMod",
    "glmerMod", "glmgee", "glmrob", "glmrob", "glmmTMB", "glmmTMB",
    "glmmTMB", "glmmTMB", "glmmTMB", "glmmTMB", "glmmPQL", "glmmPQL",
    "glmx", "glm_weightit", "glm_weightit", "glm_weightit", "glm_weightit",
    "glm_weightit", "ivreg", "lmerMod", "lmerModLmerTest", "lmrob",
    "lm_robust", "lrm", "lrm", "lrm", "mblogit", "mblogit", "mblogit",
    "mclogit", "mclogit", "mclogit", "MCMCglmm", "model_fit", "model_fit",
    "model_fit", "workflow", "workflow", "workflow", "multinom",
    "multinom", "multinom_weightit", "multinom_weightit", "multinom_weightit",
    "mhurdle", "mhurdle", "mhurdle", "mlogit", "mvgam", "mvgam",
    "mvgam", "mvgam", "mvgam", "negbin", "negbin", "negbin", "ols",
    "oohbchoice", "oohbchoice", "orm", "orm", "orm", "ordinal_weightit",
    "ordinal_weightit", "ordinal_weightit", "ordinal_weightit", "ordinal_weightit",
    "polr", "rendo.base", "rendo.base", "rlm", "selection", "selection",
    "selection", "speedlm", "speedglm", "speedglm", "stanreg", "stanreg",
    "survreg", "survreg", "survreg", "svyglm", "svyglm", "svyolr",
    "tobit", "tobit1", "tobit1", "tobit1", "zeroinfl", "zeroinfl",
    "zeroinfl", "zeroinfl"
  ),
  type = c(
    "response", "link", "ev", "ppd", "response", "link", "precision",
    "quantile", "variance", "response", "link", "probs", "response",
    "link", "response", "link", "prediction", "average", "probs",
    "class", "prob", "cum.prob", "linear.predictor", "expected",
    "lp", "risk", "survival", "survival", "expected", "lp", "risk",
    "survival", "expected", "lp", "risk", "response", "location",
    "scale", "density", "pr", "xb", "location", "cumprob", "scale",
    "density", "pr", "xb", "survival", "response", "mean", "link",
    "lp", "linear", "rmst", "hazard", "cumhaz", "invlink(link)",
    "response", "link", "response", "prob", "count", "zero", "response",
    "response", "response", "link", "invlink(link)", "response",
    "link", "response", "link", "lp", "response", "link", "invlink(link)",
    "response", "link", "response", "link", "response", "response",
    "link", "response", "link", "conditional", "zprob", "zlink",
    "disp", "response", "link", "response", "invlink(link)", "probs",
    "response", "lp", "link", "response", "response", "response",
    "response", "response", "fitted", "lp", "mean", "response", "latent",
    "link", "response", "latent", "link", "response", "numeric",
    "prob", "class", "numeric", "prob", "class", "probs", "latent",
    "probs", "response", "mean", "E", "Ep", "p", "response", "response",
    "link", "expected", "detection", "latent_N", "invlink(link)",
    "response", "link", "lp", "probability", "utility", "fitted",
    "mean", "lp", "probs", "response", "link", "lp", "mean", "probs",
    "response", "link", "response", "response", "link", "unconditional",
    "response", "response", "link", "response", "link", "response",
    "link", "quantile", "response", "link", "probs", "response",
    "expvalue", "linpred", "prob", "response", "prob", "count", "zero"
  ),
  stringsAsFactors = FALSE
)


# the default "type" arguments for each model class. Used to set the
# default type in "ggaverage()"
# Run following code to update this list:
# x <- marginaleffects:::type_dictionary_build()
# x[!duplicated(x$class), ]
# Finally, add "other" as first element to "class" and "response" to "type"
.default_type <- data.frame(
  class = c(
    "other",
    "bam", "bart", "betareg", "bife", "bracl",
    "brglmFit", "brmsfit", "brmultinom", "clm", "clogit", "coxph",
    "coxph_weightit", "crch", "hetprob", "hxlr", "ivpml", "flexsurvreg",
    "fixest", "hurdle", "iv_robust", "lm", "gam", "Gam", "geeglm",
    "Gls", "glimML", "glm", "glmerMod", "glmgee", "glmrob", "glmmTMB",
    "glmmPQL", "glmx", "glm_weightit", "ivreg", "lmerMod", "lmerModLmerTest",
    "lmrob", "lm_robust", "lrm", "mblogit", "mclogit", "MCMCglmm",
    "model_fit", "workflow", "multinom", "multinom_weightit", "mhurdle",
    "mlogit", "mvgam", "negbin", "ols", "oohbchoice", "orm", "ordinal_weightit",
    "polr", "rendo.base", "rlm", "selection", "speedlm", "speedglm",
    "stanreg", "survreg", "svyglm", "svyolr", "tobit", "tobit1",
    "zeroinfl"
  ),
  type = c(
    "response",
    "response", "ev", "response", "response",
    "probs", "response", "response", "probs", "prob", "expected",
    "survival", "survival", "response", "pr", "location", "pr", "survival",
    "invlink(link)", "response", "response", "response", "response",
    "invlink(link)", "response", "lp", "response", "invlink(link)",
    "response", "response", "response", "response", "response", "response",
    "invlink(link)", "response", "response", "response", "response",
    "response", "fitted", "response", "response", "response", "numeric",
    "numeric", "probs", "probs", "E", "response", "response", "invlink(link)",
    "lp", "probability", "fitted", "probs", "probs", "response",
    "response", "response", "response", "response", "response", "response",
    "response", "probs", "response", "expvalue", "response"
  ),
  stringsAsFactors = FALSE
)


.validate_type_argument <- function(model,
                                    type,
                                    marginaleffects = FALSE,
                                    emmeans_call = FALSE) {
  # marginaleffects supports the predict-method types
  # we need a different approach to validation here
  if (marginaleffects) {
    # for zero-inflation models, we need to find the correct name
    # for the type argument...
    is_zero_inflated <- insight::model_info(model)$is_zero_inflated
    if (is_zero_inflated) {
      if (inherits(model, "glmmTMB")) {
        types <- c("conditional", "zprob")
      } else {
        types <- c("count", "zero")
      }
    }
    # first, we overwrite the "default"
    if (type == "fixed") {
      if (is_zero_inflated) {
        type <- types[1]
      } else if (class(model)[1] %in% .default_type$class) {
        type <- .default_type$type[.default_type$class == class(model)[1]]
      } else {
        type <- "response"
      }
    } else if (type %in% c("zi", "zero_inflated", "fe.zi")) {
      type <- "response"
    } else if (type %in% c("zi.prob", "zi_prob")) {
      type <- types[2]
    }
    # check which types are supported by the model's predict-method
    type_options <- .typedic$type[.typedic$class == class(model)[1]]
    if (!type %in% c("response", type_options)) {
      insight::format_error(sprintf(
        "`type = \"%s\"` is not supported. Please use %s%s.",
        type,
        if (length(type_options) > 1) "one of " else "",
        toString(paste0("`", type_options, "`"))
      ))
    }
    return(type)
  }

  # if we call "predict()" or "emmeans()", we have these different options
  if (emmeans_call) {
    type_choices <- c("fixed", "count", "zero_inflated", "zi_prob")
  } else {
    type_choices <- c(
      "fixed", "count", "random", "zero_inflated", "zi_random",
      "zero_inflated_random", "zi_prob", "simulate", "survival",
      "cumulative_hazard", "simulate_random", "debug", "quantile" # for survreg
    )
  }
  type <- insight::validate_argument(type, type_choices)

  switch(type,
    count = "fixed",
    zi_random = "zero_inflated_random",
    type
  )
}


.retrieve_type_option <- function(model) {
  # retrieve model object's predict-method prediction-types (if any)
  predict_method <- .safe(lapply(
    class(model), function(i) {
      utils::getS3method("predict", i)
    }
  ))
  # check whether model class has a predict method
  if (!is.null(predict_method)) {
    predict_method <- predict_method[!vapply(predict_method, is.null, TRUE)][[1]]
  }
  # retrieve model object's predict-method prediction-types (if any)
  .safe(suppressWarnings(eval(formals(predict_method)$type)))
}
