.emmeans_mixed_zi <- function(model,
                              data_grid,
                              cleaned_terms,
                              ci_level = NULL,
                              bias_correction = FALSE,
                              residual_variance = NULL,
                              weights = NULL,
                              ...) {
  if (inherits(model, "glmmTMB")) {
    .ggemmeans_glmmTMB(
      model,
      data_grid,
      cleaned_terms,
      ci_level,
      bias_correction,
      residual_variance,
      weights = weights,
      ...
    )
  } else {
    .ggemmeans_MixMod(
      model,
      data_grid,
      cleaned_terms,
      ci_level,
      bias_correction,
      residual_variance,
      weights = weights,
      ...
    )
  }
}


.emmeans_prediction_data <- function(model,
                                     data_grid,
                                     cleaned_terms,
                                     ci_level,
                                     pmode,
                                     model_info,
                                     interval = NULL,
                                     model_data = NULL,
                                     vcov_info = NULL,
                                     bias_correction = FALSE,
                                     residual_variance = NULL,
                                     weights = NULL,
                                     verbose = TRUE,
                                     ...) {
  if (inherits(model, "MCMCglmm")) {
    .ggemmeans_predict_MCMCglmm(
      model, data_grid, cleaned_terms, ci_level, pmode,
      interval = interval, model_data = model_data, weights = weights, ...
    )
  } else if (!is.null(model_info) && (model_info$is_ordinal || model_info$is_multinomial || model_info$is_categorical)) { # nolint
    .ggemmeans_predict_ordinal(
      model, data_grid, cleaned_terms, ci_level,
      interval = interval, model_data = model_data, weights = weights, ...
    )
  } else if (inherits(model, c("gls", "lme"))) {
    .ggemmeans_predict_nlme(
      model, data_grid, cleaned_terms, ci_level,
      interval = interval, model_data = model_data,
      bias_correction = bias_correction,
      residual_variance = residual_variance, weights = weights, ...
    )
  } else {
    .ggemmeans_predict_generic(
      model, data_grid, cleaned_terms, ci_level, pmode,
      interval = interval, model_data = model_data, vcov_info = vcov_info,
      verbose = verbose, bias_correction = bias_correction,
      residual_variance = residual_variance, weights = weights, ...
    )
  }
}


.ggemmeans_MixMod <- function(model,
                              data_grid,
                              cleaned_terms,
                              ci_level = NULL,
                              bias_correction = FALSE,
                              residual_variance = NULL,
                              weights = NULL,
                              ...) {
  insight::check_if_installed("emmeans", "to compute estimated marginal means for MixMod-models")

  # get additional arguments
  dot_args <- list(...)
  # modify sigma, if necessary
  if (!is.null(residual_variance)) {
    dot_args$sigma <- sqrt(residual_variance)
  }

  arg_list <- list(
    model,
    specs = cleaned_terms,
    at = data_grid,
    level = ci_level,
    bias.adjust = bias_correction,
    weights = weights
  )

  x1 <- as.data.frame(suppressWarnings(do.call(emmeans::emmeans, c(arg_list, dot_args))))

  arg_list <- list(
    model,
    specs = all.vars(stats::formula(model, type = "zi_fixed")),
    at = data_grid,
    mode = "zero_part",
    level = ci_level,
    bias.adjust = bias_correction
  )

  x2 <- as.data.frame(suppressWarnings(do.call(emmeans::emmeans, c(arg_list, dot_args))))

  list(x1 = x1, x2 = x2)
}


.ggemmeans_glmmTMB <- function(model,
                               data_grid,
                               cleaned_terms,
                               ci_level = NULL,
                               bias_correction = FALSE,
                               residual_variance = NULL,
                               weights = NULL,
                               ...) {
  insight::check_if_installed("emmeans", "to compute estimated marginal means for glmmTMB-models")
  # all model variables
  vars <- insight::find_variables(model)
  # match variables for conditional model
  cleaned_terms_cond <- cleaned_terms[cleaned_terms %in% vars$conditional]
  # match variables for zi model
  cleaned_terms_zi <- cleaned_terms[cleaned_terms %in% vars$zero_inflated]

  # get additional arguments
  dot_args <- list(...)
  # modify sigma, if necessary
  if (!is.null(residual_variance)) {
    dot_args$sigma <- sqrt(residual_variance)
  }

  arg_list <- list(
    model,
    specs = cleaned_terms_cond,
    at = data_grid[cleaned_terms_cond],
    component = "cond",
    level = ci_level,
    bias.adjust = bias_correction,
    weights = weights
  )

  x1 <- as.data.frame(suppressWarnings(do.call(emmeans::emmeans, c(arg_list, dot_args))))

  arg_list <- list(
    model,
    specs = cleaned_terms_zi,
    at = data_grid[cleaned_terms_zi],
    component = "zi",
    level = ci_level,
    bias.adjust = bias_correction
  )

  x2 <- as.data.frame(suppressWarnings(do.call(emmeans::emmeans, c(arg_list, dot_args))))

  list(x1 = x1, x2 = x2)
}


.ggemmeans_predict_ordinal <- function(model,
                                       data_grid,
                                       cleaned_terms,
                                       ci_level,
                                       interval = NULL,
                                       model_data = NULL,
                                       weights = NULL,
                                       ...) {
  tmp <- suppressMessages(emmeans::emmeans(
    model,
    specs = c(insight::find_response(model, combine = FALSE), cleaned_terms),
    at = data_grid,
    mode = "prob",
    weights = weights,
    ...
  ))

  .ggemmeans_add_confint(model, tmp, ci_level, pmode = "prob", interval)
}


.ggemmeans_predict_MCMCglmm <- function(model,
                                        data_grid,
                                        cleaned_terms,
                                        ci_level,
                                        pmode,
                                        interval = NULL,
                                        model_data = NULL,
                                        weights = NULL,
                                        ...) {
  tmp <- emmeans::emmeans(
    model,
    specs = cleaned_terms,
    at = data_grid,
    pmode = pmode,
    weights = weights,
    data = insight::get_data(model, source = "frame", verbose = FALSE),
    ...
  )

  .ggemmeans_add_confint(model, tmp, ci_level, pmode, interval)
}


.ggemmeans_predict_generic <- function(model,
                                       data_grid,
                                       cleaned_terms,
                                       ci_level,
                                       pmode,
                                       interval = NULL,
                                       model_data = NULL,
                                       vcov_info = NULL,
                                       bias_correction = FALSE,
                                       residual_variance = NULL,
                                       weights = NULL,
                                       verbose = TRUE,
                                       ...) {
  # get additional arguments
  dot_args <- list(...)
  # modify sigma, if necessary
  if (!is.null(residual_variance)) {
    dot_args$sigma <- sqrt(residual_variance)
  }

  # setup arguments
  emmeans_args <- list(
    model,
    specs = cleaned_terms,
    at = data_grid,
    mode = pmode,
    bias.adjust = bias_correction,
    weights = weights
  )
  # add vcov-information if available
  if (!is.null(vcov_info)) {
    emmeans_args <- insight::compact_list(
      c(emmeans_args, list(vcov = vcov_info$vcov), vcov_info$vcov_args)
    )
  }
  emmeans_args <- c(emmeans_args, dot_args)

  # first attempt
  tmp <- .safe(suppressWarnings(do.call(emmeans::emmeans, emmeans_args)))

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
    .ggemmeans_add_confint(model, tmp, ci_level, pmode, interval)
  } else {
    NULL
  }
}


.ggemmeans_predict_nlme <- function(model,
                                    data_grid,
                                    cleaned_terms,
                                    ci_level,
                                    interval = NULL,
                                    model_data = NULL,
                                    bias_correction = FALSE,
                                    residual_variance = NULL,
                                    weights = NULL,
                                    ...) {
  # get additional arguments
  dot_args <- list(...)
  # modify sigma, if necessary
  if (!is.null(residual_variance)) {
    dot_args$sigma <- sqrt(residual_variance)
  }

  # setup arguments
  emmeans_args <- list(
    model,
    specs = cleaned_terms,
    at = data_grid,
    bias.adjust = bias_correction,
    weights = weights
  )

  tmp <- tryCatch(
    suppressWarnings(do.call(emmeans::emmeans, c(emmeans_args, dot_args))),
    error = function(e) {
      insight::print_color("Can't compute estimated marginal means, 'emmeans::emmeans()' returned an error.\n\n", "red")
      cat(sprintf("Reason: %s\n", e$message))
      cat("You may try 'ggpredict()' or 'ggeffect()'.\n\n")
      NULL
    }
  )

  if (!is.null(tmp)) {
    .ggemmeans_add_confint(model, tmp, ci_level)
  } else {
    NULL
  }
}
