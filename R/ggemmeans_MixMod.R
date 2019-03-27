#' @importFrom stats formula
.ggemmeans_MixMod <- function(model, expanded_frame, cleaned.terms, ...) {
  if (!requireNamespace("emmeans")) {
    stop("Package `emmeans` required to compute marginal effects for clmm-models.", call. = FALSE)
  }

  x1 <- suppressWarnings(emmeans::emmeans(
    model,
    specs = cleaned.terms,
    at = expanded_frame,
    ...
  )) %>%
    as.data.frame()

  x2 <- suppressWarnings(emmeans::emmeans(
    model,
    specs = all.vars(stats::formula(model, type = "zi_fixed")),
    at = expanded_frame,
    mode = "zero_part",
    ...
  )) %>%
    as.data.frame()

  list(x1 = x1, x2 = x2)
}
