.emmeans_mixed_zi <- function(model, data_grid, cleaned_terms, ...) {
  if (inherits(model, "glmmTMB")) {
    .ggemmeans_glmmTMB(model, data_grid, cleaned_terms, ...)
  } else {
    .ggemmeans_MixMod(model, data_grid, cleaned_terms, ...)
  }
}



.emmeans_prediction_data <- function(model, data_grid, cleaned_terms, ci.lvl, pmode, type, model_info, interval = NULL, model_data = NULL, ...) {
  if (inherits(model, "MCMCglmm")) {
    prediction_data <- .ggemmeans_predict_MCMCglmm(model, data_grid, cleaned_terms, ci.lvl, pmode, type, interval = interval, model_data = model_data, ...)
  } else if (model_info$is_ordinal | model_info$is_multinomial | model_info$is_categorical) {
    prediction_data <- .ggemmeans_predict_ordinal(model, data_grid, cleaned_terms, ci.lvl, type, interval = interval, model_data = model_data, ...)
  } else if (inherits(model, c("gls", "lme"))) {
    prediction_data <- .ggemmeans_predict_nlme(model, data_grid, cleaned_terms, ci.lvl, type, interval = interval, model_data = model_data, ...)
  } else {
    prediction_data <- .ggemmeans_predict_generic(model, data_grid, cleaned_terms, ci.lvl, pmode, type, interval = interval, model_data = model_data, ...)
  }
}








#' @importFrom stats formula
.ggemmeans_MixMod <- function(model, data_grid, cleaned_terms, ...) {
  if (!requireNamespace("emmeans")) {
    stop("Package `emmeans` required to compute estimated marginal means for MixMod-models.", call. = FALSE)
  }

  x1 <- as.data.frame(suppressWarnings(emmeans::emmeans(
    model,
    specs = cleaned_terms,
    at = data_grid,
    ...
  )))

  x2 <- as.data.frame(suppressWarnings(emmeans::emmeans(
    model,
    specs = all.vars(stats::formula(model, type = "zi_fixed")),
    at = data_grid,
    mode = "zero_part",
    ...
  )))

  list(x1 = x1, x2 = x2)
}






.ggemmeans_glmmTMB <- function(model, data_grid, cleaned_terms, ...) {
  if (!requireNamespace("emmeans")) {
    stop("Package `emmeans` required to estimated marginal means effects for glmmTMB-models.", call. = FALSE)
  }

  x1 <- as.data.frame(suppressWarnings(emmeans::emmeans(
    model,
    specs = cleaned_terms,
    at = data_grid,
    component = "cond",
    ...
  )))

  x2 <- as.data.frame(suppressWarnings(emmeans::emmeans(
    model,
    specs = cleaned_terms,
    at = data_grid,
    component = "zi",
    ...
  )))

  list(x1 = x1, x2 = x2)
}






.ggemmeans_predict_ordinal <- function(model, data_grid, cleaned_terms, ci.lvl, type, interval = NULL, model_data = NULL, ...) {
  tmp <- emmeans::emmeans(
    model,
    specs = c(insight::find_response(model, combine = FALSE), cleaned_terms),
    at = data_grid,
    mode = "prob",
    ...
  )

  .ggemmeans_add_confint(model, tmp, ci.lvl, type, pmode = "prob", interval)
}






.ggemmeans_predict_MCMCglmm <- function(model, data_grid, cleaned_terms, ci.lvl, pmode, type, interval = NULL, model_data = NULL, ...) {
  tmp <- emmeans::emmeans(
    model,
    specs = cleaned_terms,
    at = data_grid,
    pmode = pmode,
    data = insight::get_data(model),
    ...
  )

  .ggemmeans_add_confint(model, tmp, ci.lvl, type, pmode, interval)
}





.ggemmeans_predict_generic <- function(model, data_grid, cleaned_terms, ci.lvl, pmode, type, interval = NULL, model_data = NULL, ...) {

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
      NULL
    }
  )

  if (is.null(tmp)) {
    tmp <- tryCatch(
      {
        suppressWarnings(
          emmeans::emmeans(
            model,
            specs = cleaned_terms,
            at = data_grid,
            mode = pmode,
            data = insight::get_data(model),
            ...
          )
        )
      },
      error = function(e) {
        insight::print_color("Can't compute estimated marginal means, 'emmeans::emmeans()' returned an error.\n\n", "red")
        cat(sprintf("Reason: %s\n", e$message))
        cat("You may try 'ggpredict()' or 'ggeffect()'.\n\n")
        NULL
      }
    )
  }


  if (!is.null(tmp))
    .ggemmeans_add_confint(model, tmp, ci.lvl, type, pmode, interval)
  else
    NULL
}





.ggemmeans_predict_nlme <- function(model, data_grid, cleaned_terms, ci.lvl, type, interval = NULL, model_data = NULL, ...) {

  tmp <- tryCatch(
    {
      suppressWarnings(
        emmeans::emmeans(
          model,
          specs = cleaned_terms,
          at = data_grid,
          ...
        )
      )
    },
    error = function(e) {
      insight::print_color("Can't compute estimated marginal means, 'emmeans::emmeans()' returned an error.\n\n", "red")
      cat(sprintf("Reason: %s\n", e$message))
      cat("You may try 'ggpredict()' or 'ggeffect()'.\n\n")
      NULL
    }
  )

  if (!is.null(tmp))
    .ggemmeans_add_confint(model, tmp, ci.lvl, type)
  else
    NULL
}
