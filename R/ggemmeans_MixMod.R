#' @importFrom stats formula
.ggemmeans_MixMod <- function(model, data_grid, cleaned_terms, ...) {
  if (!requireNamespace("emmeans")) {
    stop("Package `emmeans` required to compute marginal effects for clmm-models.", call. = FALSE)
  }

  x1 <- suppressWarnings(emmeans::emmeans(
    model,
    specs = cleaned_terms,
    at = data_grid,
    ...
  )) %>%
    as.data.frame()

  x2 <- suppressWarnings(emmeans::emmeans(
    model,
    specs = all.vars(stats::formula(model, type = "zi_fixed")),
    at = data_grid,
    mode = "zero_part",
    ...
  )) %>%
    as.data.frame()

  list(x1 = x1, x2 = x2)
}
