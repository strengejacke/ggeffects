#' @importFrom stats confint
#' @importFrom sjmisc var_rename
get_predictions_clmm <- function(model, terms, typical, condition, ci.lvl, linv, ...) {

  if (!requireNamespace("emmeans")) {
    stop("Package `emmeans` required to compute marginal effects for clmm-models.", call. = FALSE)
  }

  values.at <- get_expanded_data(
    model = model,
    mf = insight::get_data(model),
    terms = terms,
    typ.fun = typical,
    condition = condition,
    pretty.message = FALSE,
    emmeans.only = TRUE
  )

  fitfram <- emmeans::emmeans(
    object = model,
    spec = c(insight::find_response(model, combine = FALSE), get_clear_vars(terms)),
    at = values.at,
    mode = "prob"
  ) %>%
    stats::confint(level = ci.lvl) %>%
    as.data.frame() %>%
    sjmisc::var_rename(
      prob = "predicted",
      SE = "std.error",
      asymp.LCL = "conf.low",
      asymp.UCL = "conf.high"
    )

  colnames(fitfram)[1] <- "response.level"

  # copy standard errors
  attr(fitfram, "std.error") <- fitfram$std.error

  fitfram
}
