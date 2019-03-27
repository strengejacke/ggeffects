.ggemmeans_glmmTMB <- function(model, expanded_frame, cleaned.terms, ...) {
  if (!requireNamespace("emmeans")) {
    stop("Package `emmeans` required to compute marginal effects for clmm-models.", call. = FALSE)
  }

  x1 <- suppressWarnings(emmeans::emmeans(
    model,
    specs = cleaned.terms,
    at = expanded_frame,
    component = "cond",
    ...
  )) %>%
    as.data.frame()

  x2 <- suppressWarnings(emmeans::emmeans(
    model,
    specs = cleaned.terms,
    at = expanded_frame,
    component = "zi",
    ...
  )) %>%
    as.data.frame()

  list(x1 = x1, x2 = x2)
}
