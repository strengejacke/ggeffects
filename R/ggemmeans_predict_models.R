.ggemmeans_predict_ordinal <- function(model, data_grid, cleaned_terms, ci.lvl, type, ...) {
  tmp <- emmeans::emmeans(
    model,
    specs = c(insight::find_response(model, combine = FALSE), cleaned_terms),
    at = data_grid,
    mode = "prob",
    ...
  )

  .ggemmeans_add_confint(model, tmp, ci.lvl, type, pmode = "prob")
}


.ggemmeans_predict_MCMCglmm <- function(model, data_grid, cleaned_terms, ci.lvl, pmode, type, ...) {
  tmp <- emmeans::emmeans(
    model,
    specs = cleaned_terms,
    at = data_grid,
    mode = pmode,
    data = insight::get_data(model),
    ...
  )

  .ggemmeans_add_confint(model, tmp, ci.lvl, type, pmode)
}


.ggemmeans_predict_generic <- function(model, data_grid, cleaned_terms, ci.lvl, pmode, type, ...) {

  tmp <- tryCatch(
    {
      suppressWarnings(
        emmeans::emmeans(
          model,
          specs = cleaned_terms,
          at = data_grid,
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
    .ggemmeans_add_confint(model, tmp, ci.lvl, type, pmode)
  else
    NULL
}
