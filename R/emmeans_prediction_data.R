.emmeans_mixed_zi <- function(model, data_grid, cleaned_terms, ci.lvl = NULL, ...) {
  if (inherits(model, "glmmTMB")) {
    .ggemmeans_glmmTMB(model, data_grid, cleaned_terms, ci.lvl, ...)
  } else {
    .ggemmeans_MixMod(model, data_grid, cleaned_terms, ci.lvl, ...)
  }
}


.emmeans_prediction_data <- function(model,
                                     data_grid,
                                     cleaned_terms,
                                     ci.lvl,
                                     pmode,
                                     type,
                                     model_info,
                                     interval = NULL,
                                     model_data = NULL,
                                     vcov_info = NULL,
                                     verbose = TRUE,
                                     ...) {
  if (inherits(model, "MCMCglmm")) {
    prediction_data <- .ggemmeans_predict_MCMCglmm(
      model, data_grid, cleaned_terms, ci.lvl, pmode, type,
      interval = interval, model_data = model_data, ...
    )
  } else if (!is.null(model_info) && (model_info$is_ordinal || model_info$is_multinomial || model_info$is_categorical)) { # nolint
    prediction_data <- .ggemmeans_predict_ordinal(
      model, data_grid, cleaned_terms, ci.lvl, type,
      interval = interval, model_data = model_data, ...
    )
  } else if (inherits(model, c("gls", "lme"))) {
    prediction_data <- .ggemmeans_predict_nlme(
      model, data_grid, cleaned_terms, ci.lvl, type,
      interval = interval, model_data = model_data, ...
    )
  } else {
    prediction_data <- .ggemmeans_predict_generic(
      model, data_grid, cleaned_terms, ci.lvl, pmode, type,
      interval = interval, model_data = model_data, vcov_info = vcov_info,
      verbose = verbose, ...
    )
  }
}


.ggemmeans_MixMod <- function(model, data_grid, cleaned_terms, ci.lvl = NULL, ...) {
  insight::check_if_installed("emmeans", "to compute estimated marginal means for MixMod-models")

  x1 <- as.data.frame(suppressWarnings(emmeans::emmeans(
    model,
    specs = cleaned_terms,
    at = data_grid,
    level = ci.lvl,
    ...
  )))

  x2 <- as.data.frame(suppressWarnings(emmeans::emmeans(
    model,
    specs = all.vars(stats::formula(model, type = "zi_fixed")),
    at = data_grid,
    mode = "zero_part",
    level = ci.lvl,
    ...
  )))

  list(x1 = x1, x2 = x2)
}


.ggemmeans_glmmTMB <- function(model, data_grid, cleaned_terms, ci.lvl = NULL, ...) {
  insight::check_if_installed("emmeans", "to compute estimated marginal means for glmmTMB-models")
  # all model variables
  vars <- insight::find_variables(model)
  # match variables for conditional model
  cleaned_terms_cond <- cleaned_terms[cleaned_terms %in% vars$conditional]
  # match variables for zi model
  cleaned_terms_zi <- cleaned_terms[cleaned_terms %in% vars$zero_inflated]

  x1 <- as.data.frame(suppressWarnings(emmeans::emmeans(
    model,
    specs = cleaned_terms_cond,
    at = data_grid[cleaned_terms_cond],
    component = "cond",
    level = ci.lvl,
    ...
  )))

  x2 <- as.data.frame(suppressWarnings(emmeans::emmeans(
    model,
    specs = cleaned_terms_zi,
    at = data_grid[cleaned_terms_zi],
    component = "zi",
    level = ci.lvl,
    ...
  )))

  list(x1 = x1, x2 = x2)
}


.ggemmeans_predict_ordinal <- function(model,
                                       data_grid,
                                       cleaned_terms,
                                       ci.lvl,
                                       type,
                                       interval = NULL,
                                       model_data = NULL,
                                       ...) {
  tmp <- suppressMessages(emmeans::emmeans(
    model,
    specs = c(insight::find_response(model, combine = FALSE), cleaned_terms),
    at = data_grid,
    mode = "prob",
    ...
  ))

  .ggemmeans_add_confint(model, tmp, ci.lvl, type, pmode = "prob", interval)
}


.ggemmeans_predict_MCMCglmm <- function(model,
                                        data_grid,
                                        cleaned_terms,
                                        ci.lvl,
                                        pmode,
                                        type,
                                        interval = NULL,
                                        model_data = NULL,
                                        ...) {
  tmp <- emmeans::emmeans(
    model,
    specs = cleaned_terms,
    at = data_grid,
    pmode = pmode,
    data = insight::get_data(model, source = "frame", verbose = FALSE),
    ...
  )

  .ggemmeans_add_confint(model, tmp, ci.lvl, type, pmode, interval)
}


.ggemmeans_predict_generic <- function(model,
                                       data_grid,
                                       cleaned_terms,
                                       ci.lvl,
                                       pmode,
                                       type,
                                       interval = NULL,
                                       model_data = NULL,
                                       vcov_info = NULL,
                                       verbose = TRUE,
                                       ...) {
  # setup arguments
  emmeans_args <- list(
    model,
    specs = cleaned_terms,
    at = data_grid,
    mode = pmode
  )
  # add vcov-information if available
  if (!is.null(vcov_info)) {
    emmeans_args <- c(emmeans_args, vcov = vcov_info$vcov.fun, vcov_info$vcov.args)
  }
  emmeans_args <- c(emmeans_args, list(...))

  # first attempt
  tmp <- tryCatch(
    suppressWarnings(do.call(emmeans::emmeans, emmeans_args)),
    error = function(e) NULL
  )

  # if data could not be recovered, tmp is NULL. Try again, this time
  # providing the data directly
  if (is.null(tmp)) {
    emmeans_args$data <- insight::get_data(model, source = "frame", verbose = FALSE)
    tmp <- tryCatch(
      suppressWarnings(do.call(emmeans::emmeans, emmeans_args)),
      error = function(e) {
        if (verbose) {
          insight::print_color("Can't compute estimated marginal means, 'emmeans::emmeans()' returned an error.\n\n", "red") # nolint
          cat(sprintf("Reason: %s\n", e$message))
          cat("You may try 'ggpredict()' or 'ggeffect()'.\n\n")
        }
        NULL
      }
    )
  }

  # if we succeded, add confidence intervals
  if (!is.null(tmp)) {
    .ggemmeans_add_confint(model, tmp, ci.lvl, type, pmode, interval)
  } else {
    NULL
  }
}


.ggemmeans_predict_nlme <- function(model,
                                    data_grid,
                                    cleaned_terms,
                                    ci.lvl,
                                    type,
                                    interval = NULL,
                                    model_data = NULL,
                                    ...) {
  tmp <- tryCatch(
    suppressWarnings(
      emmeans::emmeans(
        model,
        specs = cleaned_terms,
        at = data_grid,
        ...
      )
    ),
    error = function(e) {
      insight::print_color("Can't compute estimated marginal means, 'emmeans::emmeans()' returned an error.\n\n", "red")
      cat(sprintf("Reason: %s\n", e$message))
      cat("You may try 'ggpredict()' or 'ggeffect()'.\n\n")
      NULL
    }
  )

  if (!is.null(tmp)) {
    .ggemmeans_add_confint(model, tmp, ci.lvl, type)
  } else {
    NULL
  }
}
