#' @importFrom dplyr case_when
get_model_function <- function(model) {
  # check class of fitted model
  dplyr::case_when(
    inherits(model, "lrm") ~ "glm",
    inherits(model, "glmRob") ~ "glm",
    inherits(model, "brglm") ~ "glm",
    inherits(model, "svyglm.nb") ~ "glm",
    inherits(model, "svyglm") ~ "glm",
    inherits(model, "glmmTMB") ~ "glm",
    inherits(model, "negbin") ~ "glm",
    inherits(model, "gam") ~ "glm",
    inherits(model, "polr") ~ "glm",
    inherits(model, "vgam") ~ "glm",
    inherits(model, "vglm") ~ "glm",
    inherits(model, c("logistf", "glm")) ~ "glm",
    inherits(model, "gls") ~ "lm",
    inherits(model, "gee") ~ "lm",
    inherits(model, "plm") ~ "lm",
    inherits(model, "lm") ~ "lm",
    inherits(model, "lmRob") ~ "lm",
    inherits(model, "lme") ~ "lm",
    inherits(model, "truncreg") ~ "lm",
    inherits(model, "glmerMod") ~ "glm",
    inherits(model, "betareg") ~ "betareg",
    inherits(model, "coxph") ~ "coxph",
    inherits(model, "nlmerMod") ~ "lm",
    inherits(model, c("zeroinfl", "hurdle")) ~ "glm",
    inherits(model, c("lmerMod", "merModLmerTest")) ~ "lm",
    TRUE ~ "glm"
  )
}

#' @importFrom dplyr case_when
get_predict_function <- function(model) {
  # check class of fitted model
  dplyr::case_when(
    inherits(model, "lrm") ~ "lrm",
    inherits(model, "glmRob") ~ "glmRob",
    inherits(model, "brglm") ~ "glm",
    inherits(model, "svyglm.nb") ~ "svyglm.nb",
    inherits(model, "svyglm") ~ "svyglm",
    inherits(model, "stanreg") ~ "stanreg",
    inherits(model, "brmsfit") ~ "brmsfit",
    inherits(model, "gam") ~ "gam",
    inherits(model, "glmerMod") ~ "glmer",
    inherits(model, "glmmTMB") ~ "glmmTMB",
    inherits(model, "nlmerMod") ~ "nlmer",
    inherits(model, c("lmerMod", "merModLmerTest")) ~ "lmer",
    inherits(model, "lme") ~ "lme",
    inherits(model, "gls") ~ "gls",
    inherits(model, "clm") ~ "clm",
    inherits(model, "polr") ~ "polr",
    inherits(model, "gee") ~ "gee",
    inherits(model, "plm") ~ "plm",
    inherits(model, "negbin") ~ "glm.nb",
    inherits(model, "vgam") ~ "vgam",
    inherits(model, "vglm") ~ "vglm",
    inherits(model, "logistf") ~ "logistf",
    inherits(model, "glm") ~ "glm",
    inherits(model, "lmRob") ~ "lm",
    inherits(model, "lm") ~ "lm",
    inherits(model, "betareg") ~ "betareg",
    inherits(model, "truncreg") ~ "truncreg",
    inherits(model, "coxph") ~ "coxph",
    inherits(model, "multinom") ~ "multinom",
    inherits(model, "Zelig-relogit") ~ "Zelig-relogit",
    inherits(model, "zeroinfl") ~ "zeroinfl",
    inherits(model, "hurdle") ~ "hurdle",
    TRUE ~ "generic"
  )
}
