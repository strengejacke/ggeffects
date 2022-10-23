get_predictions_clmm <- function(model, terms, value_adjustment, condition, ci.lvl, linv, ...) {
  insight::check_if_installed("emmeans", "to compute estimated marginal means for clmm-models")

  values.at <- .data_grid(
    model = model,
    model_frame = insight::get_data(model),
    terms = terms,
    value_adjustment = value_adjustment,
    condition = condition,
    show_pretty_message = FALSE,
    emmeans.only = TRUE
  )

  # no predicted values at random terms allowed
  re.terms <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)
  fe.terms <- insight::find_predictors(model, flatten = TRUE)

  if (any(re.terms %in% names(values.at)) && !any(re.terms %in% fe.terms)) {
    insight::format_warning(
      "Predicted values can't be computed for levels of random effects from 'clmm' models.",
      sprintf(
        "Please remove following variables from 'terms': %s",
        toString(re.terms[which(re.terms %in% names(values.at))])
    ))
    return(NULL)
  }

  emmpred <- emmeans::emmeans(
    object = model,
    spec = c(insight::find_response(model, combine = FALSE), .clean_terms(terms)),
    at = values.at,
    mode = "prob"
  )
  fitfram <- as.data.frame(stats::confint(emmpred, level = ci.lvl))
  fitfram <- .var_rename(
    fitfram,
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
