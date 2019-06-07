.ggemmeans_predict_ordinal <- function(model, expanded_frame, cleaned.terms, ci.lvl, ...) {
  tmp <- emmeans::emmeans(
    model,
    specs = c(insight::find_response(model, combine = FALSE), cleaned.terms),
    at = expanded_frame,
    mode = "prob",
    ...
  )

  .ggemmeans_add_confint(tmp, ci.lvl)
}


.ggemmeans_predict_MCMCglmm <- function(model, expanded_frame, cleaned.terms, ci.lvl, pmode, ...) {
  tmp <- emmeans::emmeans(
    model,
    specs = cleaned.terms,
    at = expanded_frame,
    mode = pmode,
    data = insight::get_data(model),
    ...
  )

  .ggemmeans_add_confint(tmp, ci.lvl)
}


.ggemmeans_predict_generic <- function(model, expanded_frame, cleaned.terms, ci.lvl, pmode, ...) {

  tmp <- tryCatch(
    {
      suppressWarnings(
        emmeans::emmeans(
          model,
          specs = cleaned.terms,
          at = expanded_frame,
          mode = pmode,
          ...
        )
      )
    },
    error = function(e) {
      insight::print_color("Can't compute marginal effects, 'emmeans::emmeans()' returned an error.\n\n", "red")
      cat(sprintf("Reason: %s\n", e$message))
      cat("You may try 'ggpredict()' or 'ggeffect()'.\n\n")
      NULL
    }
  )

  if (!is.null(tmp))
    .ggemmeans_add_confint(tmp, ci.lvl)
  else
    NULL
}
