#' @export
get_predictions.clmm <- function(model,
                                 data_grid = NULL,
                                 terms = NULL,
                                 ci_level = 0.95,
                                 type = NULL,
                                 typical = NULL,
                                 vcov = NULL,
                                 vcov_args = NULL,
                                 condition = NULL,
                                 interval = "confidence",
                                 bias_correction = FALSE,
                                 link_inverse = insight::link_inverse(model),
                                 model_info = NULL,
                                 verbose = TRUE,
                                 ...) {
  insight::check_if_installed("emmeans", "to compute estimated marginal means for clmm-models")

  values.at <- .data_grid(
    model = model,
    model_frame = insight::get_data(model, source = "frame", verbose = FALSE),
    terms = terms,
    typical = typical,
    condition = condition,
    show_pretty_message = FALSE,
    emmeans_only = TRUE
  )

  # no predicted values at random terms allowed
  re.terms <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)
  fe.terms <- insight::find_predictors(model, flatten = TRUE)

  if (any(re.terms %in% names(values.at)) && !any(re.terms %in% fe.terms)) {
    insight::format_alert(
      "Predicted values can't be computed for levels of random effects from 'clmm' models.",
      sprintf(
        "Please remove following variables from 'terms': %s",
        toString(re.terms[which(re.terms %in% names(values.at))])
      )
    )
    return(NULL)
  }

  emmpred <- emmeans::emmeans(
    object = model,
    spec = c(insight::find_response(model, combine = FALSE), .clean_terms(terms)),
    at = values.at,
    mode = "prob"
  )
  data_grid <- as.data.frame(stats::confint(emmpred, level = ci_level))
  data_grid <- .var_rename(
    data_grid,
    prob = "predicted",
    SE = "std.error",
    asymp.LCL = "conf.low",
    asymp.UCL = "conf.high"
  )

  colnames(data_grid)[1] <- "response.level"

  # copy standard errors
  attr(data_grid, "std.error") <- data_grid$std.error

  data_grid
}
