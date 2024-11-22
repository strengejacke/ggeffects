# "factor_adjustment" indicates if factors should be held constant or not
# need to be false for computing std.error for merMod objects

# typical is the function to calculate at which value non-focal
# terms are held constant (mean, median, ...)

.data_grid <- function(model,
                       model_frame,
                       terms,
                       typical,
                       factor_adjustment = TRUE,
                       show_pretty_message = TRUE,
                       condition = NULL,
                       emmeans_only = FALSE,
                       include_random = FALSE,
                       verbose = TRUE) {
  # special handling for coxph
  if (inherits(model, c("coxph", "coxme"))) {
    surv.var <- which(colnames(model_frame) == insight::find_response(model))
    model_frame <- .remove_column(model_frame, surv.var)
  }

  model_info <- .get_model_info(model)

  # make sure we don't have arrays as variables
  model_frame[] <- lapply(model_frame, function(i) if (is.array(i)) as.data.frame(i) else i)

  # we may have factors converted on the fly in the formula, which are,
  # however, numeric in the original data. We need to coerce to factor here,
  # and back to numeric later...
  on_the_fly_factors <- attributes(model_frame)$factors
  model_frame[] <- lapply(model_frame, function(i) {
    if (isTRUE(attributes(i)$factor)) as.factor(i) else i
  })

  # special handling for fixest, when "cluster" is no factor
  if (inherits(model, "fixest")) {
    fixest_cluster <- insight::find_variables(model)$cluster
    if (!is.null(fixest_cluster)) {
      for (fc in fixest_cluster) {
        if (!is.factor(model_frame[[fc]])) {
          model_frame[[fc]] <- as.factor(model_frame[[fc]])
        }
      }
    }
  }

  # check for logical variables, might not work
  logicals <- vapply(model_frame, is.logical, logical(1))
  if (any(logicals)) {
    insight::format_error(
      "Variables of type `logical` do not work, please coerce to factor and fit the model again.",
      paste0(
        "The following variables in are of type logical: ",
        toString(paste0("`", colnames(model_frame)[logicals], "`"))
      )
    )
  }

  # any weights?
  w <- insight::get_weights(model)
  if (is.null(w) || all(w == 1)) {
    w <- NULL
  }

  # get random effects (grouping factor)
  random_effect_terms <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)

  # clean variable names
  colnames(model_frame) <- insight::clean_names(colnames(model_frame))

  # get specific levels. "focal_terms" is now a names list, with the variable
  # names as names, and the specified values (in []) as list elements.
  focal_terms <- .get_representative_values(terms, model_frame)
  # and all specified variables
  all_terms <- .clean_terms(terms)

  # check for character variables, might not work
  characters <- vapply(model_frame[all_terms], is.character, logical(1))
  if (any(characters) && verbose) {
    insight::format_alert(
      "Some of the focal terms are of type `character`. This may lead to unexpected results. It is recommended to convert these variables to factors before fitting the model.", # nolint
      paste0(
        "The following variables are of type character: ",
        toString(paste0("`", all_terms[characters], "`"))
      )
    )
  }

  # check if user has any predictors with log-transformation inside
  # model formula, but *not* used back-transformation "exp". Tell user
  # so she's aware of the problem

  offset_log_term <- tryCatch(
    {
      olt <- NULL
      if (!inherits(model, "brmsfit") && show_pretty_message && .has_log(model)) {
        any_offset_log_term <- .get_offset_log_terms(model)
        # check if we have offset() in formula, with transformed variable
        if (any(any_offset_log_term)) {
          clean.term <- insight::find_predictors(
            model,
            effects = "all",
            component = "all",
            flatten = FALSE,
            verbose = FALSE
          )
          clean.term <- unlist(clean.term[c("conditional", "random", "instruments")])[any_offset_log_term]

          # try to back-transform
          offset_function <- .get_offset_transformation(model)
          # check whether offset is transformed and *not* fixed (conditioned) at a specific value
          if (identical(offset_function, "log") && !clean.term %in% names(condition)) {
            if (verbose) {
              insight::format_alert(
                "Model uses a transformed offset term. Predictions may not be correct.",
                sprintf("It is recommended to fix the offset term using the `condition` argument, e.g. `condition = c(%s = 1)`.", clean.term), # nolint
                sprintf("You could also transform the offset variable before fitting the model and use `offset(%s)` in the model formula.", clean.term) # nolint
              )
            }
            olt <- clean.term
          }
        }
      }
      olt
    },
    error = function(x) NULL,
    warning = function(x) NULL,
    finally = function(x) NULL
  )


  # Check if model has splines, and if so, tell user that he may use
  # all values - except for gam and vgam models. "predict()" seems
  # stable even for large data frame for gam/vgam. Especially for
  # mixed models, computing SE and CI is very memory consuming, leading
  # to memory allocation errors. That's why by default values for continuous
  # variables are "prettified" to a smaller set of unique values.

  use_all_values <- FALSE

  # for these models, always all values are used
  all_values_models <- c("Gam", "gam", "vgam", "glm", "lm", "nestedLogit",
                         "brmsfit", "bamlss", "gamlss", "glmx", "feglm")

  # for these models, all values are used if we have splines or polynomial terms
  if (.has_spline_or_poly(model) && !.uses_all_tag(terms)) {
    use_all_values <- inherits(model, all_values_models)
  }

  # for remaining models, if not "[all]" tag is used, tell user about
  # considerung using the "[all]" tag
  if (verbose && !use_all_values && !.uses_all_tag(terms)) {
    if (.has_splines(model)) {
      insight::format_alert(sprintf(
        "Model contains splines or polynomial terms. Consider using `terms=\"%s [all]\"` to get smooth plots. See also package-vignette 'Adjusted Predictions at Specific Values'.", all_terms[1] # nolint
      ))
      show_pretty_message <- FALSE
    }
    if (.has_poly(model) && show_pretty_message) {
      insight::format_alert(sprintf(
        "Model contains polynomial or cubic / quadratic terms. Consider using `terms=\"%s [all]\"` to get smooth plots. See also package-vignette 'Adjusted Predictions at Specific Values'.", all_terms[1] # nolint
      ))
      show_pretty_message <- FALSE
    }
    if (.has_trigonometry(model) && show_pretty_message) {
      insight::format_alert(sprintf(
        "Model contains trigonometric terms (sinus, cosinus, ...). Consider using `terms=\"%s [all]\"` to get smooth plots. See also package-vignette 'Adjusted Predictions at Specific Values'.", all_terms[1] # nolint
      ))
      show_pretty_message <- FALSE
    }
  }

  # find terms for which no specific at-values are given
  conditional_terms <- which(!(all_terms %in% names(focal_terms)))

  # prettify numeric vectors, get representative values
  constant_levels <- .prettify_data(
    conditional_terms = conditional_terms,
    original_model_frame = model_frame,
    terms = all_terms,
    use_all_values = use_all_values,
    show_pretty_message = show_pretty_message && model_info$is_binomial && verbose
  )
  names(constant_levels) <- all_terms[conditional_terms]
  focal_terms <- c(focal_terms, constant_levels)


  # check for monotonic terms and valid values. In case 'mo()' is used,
  # and predictor is numeric, prettyfied values in the data grid are based
  # on the range of the numeric variable, although only those values are allowed
  # in the data grid that actually appear in the data

  if (inherits(model, "brmsfit")) {
    model_terms <- insight::find_terms(model, flatten = TRUE, verbose = FALSE)
    monotonics <- grepl("mo\\((.*)\\)", model_terms)
    if (any(monotonics)) {
      mo_terms <- gsub("mo\\((.*)\\)", "\\1", model_terms[monotonics])
      # now check if values are valid
      invalid_levels <- unlist(lapply(mo_terms, function(mt) {
        if (mt %in% names(focal_terms) && mt %in% colnames(model_frame)) {
          !all(model_frame[[mt]] %in% focal_terms[[mt]])
        } else {
          FALSE
        }
      }))
      if (any(invalid_levels)) {
        insight::format_error(
          sprintf("Variable(s) %s are used as monotonic effects, however, only values that are also present in the data are allowed for predictions.", toString(mo_terms)), # nolint
          "Consider converting variables used in `mo()` into (ordered) factors before fitting the model."
        )
      }
      # we also need to remove monotonic terms from "on-the-fly" factors,
      # because we need the levels from the monotonic term
      on_the_fly_factors <- setdiff(on_the_fly_factors, mo_terms)
      if (!length(on_the_fly_factors)) {
        on_the_fly_factors <- NULL
      }
    }
  }


  ## TODO check for other panelr models

  offset_term <- .offset_term(model, condition, show_pretty_message)
  model_predictors <- c(
    insight::find_predictors(
      model,
      effects = "all",
      component = "all",
      flatten = TRUE,
      verbose = FALSE
    ),
    offset_term
  )
  if (inherits(model, "wbm")) {
    model_predictors <- unique(c(
      insight::find_response(model),
      model_predictors,
      model@call_info$id,
      model@call_info$wave
    ))
  }

  # check if offset term is in model frame
  if (!is.null(offset_term) && !(offset_term %in% colnames(model_frame))) {
    model_frame <- .add_offset_to_mf(model, model_frame, offset_term)
  }

  # get count of terms, and number of columns
  n_predictors <- length(model_predictors)


  # remove NA from values, so we don't have expanded data grid
  # with missing values. this causes an error with predict()

  if (any(vapply(focal_terms, anyNA, TRUE))) {
    focal_terms <- lapply(focal_terms, function(.x) as.vector(stats::na.omit(.x)))
  }


  ## TODO check, it should actually no longer happen that the values of
  # "model_predictors" are not in the column names of the model frame "model_frame"

  # names of predictor variables may vary, e.g. if log(x) or poly(x) etc. is used.
  # so check if we have correct predictor names that also appear in model frame

  ## TODO brms does currently not support "terms()" generic

  if (inherits(model, "wbm")) {
    model_predictors <- model_predictors[model_predictors %in% colnames(model_frame)]
  } else {
    if (sum(!(model_predictors %in% colnames(model_frame))) > 0 && !inherits(model, c("brmsfit", "MCMCglmm"))) {
      # get terms from model directly
      model_predictors <- .safe(attr(stats::terms(model), "term.labels", exact = TRUE))
    }

    # 2nd check
    if (is.null(model_predictors) || sum(!(model_predictors %in% colnames(model_frame))) > 0) {
      # get terms from model frame column names
      model_predictors <- colnames(model_frame)
      # we may have more terms now, e.g. intercept. remove those now
      if (length(model_predictors) > n_predictors) {
        model_predictors <- model_predictors[2:(n_predictors + 1)]
      }
    }
  }

  # keep those, which we did not process yet
  model_predictors <- model_predictors[!(model_predictors %in% names(focal_terms))]

  # if we have weights, and typical value is mean, use weighted means
  # as function for the typical values

  if (!.is_empty(w) && length(w) == nrow(model_frame)) {
    if (identical(typical, "mean")) {
      typical <- "weighted.mean"
    } else if (is.list(typical)) {
      typical <- lapply(typical, function(x) {
        if (identical(x, "mean")) {
          "weighted.mean"
        } else {
          x
        }
      })
    }
  }

  if (.is_empty(w)) {
    if (identical(typical, "weighted.mean")) {
      typical <- "mean"
    } else if (is.list(typical)) {
      typical <- lapply(typical, function(x) {
        if (identical(x, "weighted.mean")) {
          "mean"
        } else {
          x
        }
      })
    }
  }

  # at this point, we have the "terms" in focal terms, but possibly in different
  # order as in "terms" (because we first have terms with [] in focal terms, then
  # the remaining). Here we make sure we restore the correct order

  if (all(all_terms %in% names(focal_terms))) {
    focal_terms <- focal_terms[all_terms]
  }


  # do we have variables that should be held constant at a specific value?

  if (!is.null(condition) && !is.null(names(condition))) {
    focal_terms <- c(focal_terms, as.list(condition))
    model_predictors <- model_predictors[!(model_predictors %in% names(condition))]
  }


  # add all constant values to list. For those predictors that have to be
  # held constant, use "typical" values - mean/median for numeric values,
  # reference level for factors and most common element for character vectors

  # special handling for emmeans
  if (isTRUE(emmeans_only)) {
    # check for log-terms, and if in focal terms, remove "0" from values
    log_terms <- .which_log_terms(model)
    if (!is.null(log_terms) && any(log_terms %in% names(focal_terms))) {
      for (lt in log_terms) {
        if (is.numeric(focal_terms[[lt]]) && any(focal_terms[[lt]] <= 0)) {
          focal_terms[[lt]] <- focal_terms[[lt]][-which(focal_terms[[lt]] <= 0)]
        }
      }
    }
    # adjust constant values, special handling for emmeans only
    constant_values <- lapply(model_predictors, function(x) {
      pred <- model_frame[[x]]
      if (!is.factor(pred) && !is.character(pred) && !x %in% random_effect_terms) {
        .typical_value(
          pred,
          fun = typical,
          weights = w,
          predictor = x,
          log_terms = .which_log_terms(model)
        )
      }
    })
    names(constant_values) <- model_predictors
    constant_values <- .compact_list(constant_values)

  } else if (factor_adjustment) {
    # adjust constant values, factors set to reference level
    constant_values <- lapply(model_predictors, function(x) {
      pred <- model_frame[[x]]
      if (is.factor(pred)) {
        pred <- droplevels(pred)
      }
      .typical_value(
        pred,
        fun = typical,
        weights = w,
        predictor = x,
        log_terms = .which_log_terms(model)
      )
    })
    names(constant_values) <- model_predictors

  } else {
    # adjust constant values, use all factor levels
    re.grp <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)
    # if factors should not be held constant (needed when computing
    # std.error for merMod objects), we need all factor levels,
    # and not just the typical value
    constant_values <- lapply(model_predictors, function(.x) {
      # get group factors from random effects
      is.re.grp <- !is.null(re.grp) && .x %in% re.grp
      x <- model_frame[[.x]]
      # only get levels if not random effect
      if (is.factor(x) && !is.re.grp) {
        levels(droplevels(x))
      } else {
        if (is.factor(x)) x <- droplevels(x)
        .typical_value(x, fun = typical, weights = w)
      }
    })
    names(constant_values) <- model_predictors
  }

  # for brms-models with additional response information, we need
  # also the number of trials to calculate predictions

  n.trials <- NULL

  if (!is.null(model_info) && model_info$is_trial && inherits(model, "brmsfit")) {
    tryCatch(
      {
        rv <- insight::find_response(model, combine = FALSE)
        # check if trials-variable is held constant at another value already
        if (!(rv[2] %in% names(condition))) {
          n.trials <- as.integer(stats::median(model_frame[[rv[2]]]))
          if (!.is_empty(n.trials)) {
            constant_values <- c(constant_values, list(n.trials))
            names(constant_values)[length(constant_values)] <- rv[2]
          }
        }
      },
      error = function(x) NULL
    )
  }

  # for MixMod, we need mean value of response as well...
  if (inherits(model, c("MixMod", "MCMCglmm"))) {
    constant_values <- c(constant_values, .typical_value(insight::get_response(model, verbose = FALSE)))
    names(constant_values)[length(constant_values)] <- insight::find_response(model, combine = FALSE)
  }

  # add constant values.
  focal_terms <- c(focal_terms, constant_values)


  # stop here for emmeans-objects

  if (isTRUE(emmeans_only)) {

    # remove grouping factor of RE from constant values
    # only applicable for MixMod objects
    if (inherits(model, "MixMod") && !is.null(random_effect_terms) &&
          !.is_empty(constant_values) && any(random_effect_terms %in% names(constant_values))) {
      constant_values <- constant_values[!(names(constant_values) %in% random_effect_terms)]
    }

    # save names
    focal_term_names <- names(focal_terms)

    # restore original type
    focal_terms <- lapply(focal_term_names, function(x) {
      # check for consistent vector type: on-the-fly conversion of factors
      if (!is.null(on_the_fly_factors) && x %in% on_the_fly_factors) {
        return(.factor_to_numeric(focal_terms[[x]]))
      }

      # check for consistent vector type: numeric
      if (is.numeric(model_frame[[x]]) && !is.numeric(focal_terms[[x]])) {
        return(.factor_to_numeric(focal_terms[[x]]))
      }

      # check for consistent vector type: factor
      if (is.factor(model_frame[[x]]) && !is.factor(focal_terms[[x]])) {
        return(as.character(focal_terms[[x]]))
      }

      # else return original vector
      focal_terms[[x]]
    })

    # add back names
    names(focal_terms) <- focal_term_names

    # save constant values as attribute
    attr(focal_terms, "constant.values") <- constant_values
    attr(focal_terms, "n.trials") <- n.trials

    # remember if grouping "factor" is numeric. this is possibly required
    # later when plotting data points for continuous predictors that are
    # held constant at their mean/sd values or similar.
    if (length(terms) > 1) {
      attr(focal_terms, "continuous.group") <- is.numeric(focal_terms[[2]])
    } else {
      attr(focal_terms, "continuous.group") <- FALSE
    }

    return(focal_terms)
  }


  # create data frame with all unqiue combinations
  dat <- as.data.frame(expand.grid(focal_terms, stringsAsFactors = TRUE))


  # we have to check type consistency. If user specified certain value
  # (e.g. "education [1,3]"), these are returned as string and coerced
  # to factor, even if original vector was numeric. In this case, we have
  # to coerce back these variables. Else, predict() complains that model
  # was fitted with numeric, but newdata has factor (or vice versa).

  datlist <- lapply(colnames(dat), function(x) {

    # check for consistent vector type: numeric
    if (is.numeric(model_frame[[x]]) && !is.numeric(dat[[x]])) {
      return(.factor_to_numeric(dat[[x]]))
    }

    # check for consistent vector type: factor
    if (is.factor(model_frame[[x]]) && !is.factor(dat[[x]])) {
      return(as.factor(dat[[x]]))
    }

    # else return original vector
    dat[[x]]
  })

  # if we want to average across random effects, we set include_random to TRUE
  # in this case, we create a data grid for all levels of the random effects
  # and average across groups later...
  if (include_random) {
    focal_terms <- c(focal_terms, random_effect_terms)
  }

  # get list names. we need to remove patterns like "log()" etc.
  names(datlist) <- names(focal_terms)
  datlist <- as.data.frame(datlist)

  # in case we have variable names with white space, fix here
  if (any(names(focal_terms) != colnames(datlist))) {
    colnames(datlist) <- names(focal_terms)
  }

  if (inherits(model, "wbm")) {
    colnames(datlist) <- names(focal_terms)
  }


  # check if predictions should be conditioned on random effects,
  # but not on each group level. If so, set random effect to NA
  # which will return predictions on a population level.
  # See ?glmmTMB::predict

  if (!include_random && inherits(model, c("glmmTMB", "merMod", "rlmerMod", "MixMod", "brmsfit", "lme"))) {
    cleaned_terms <- .clean_terms(terms)

    # check if we have fixed effects as grouping factor in random effects as well...
    cleaned_terms <- unique(c(
      cleaned_terms,
      insight::find_predictors(model, effects = "fixed", flatten = TRUE, verbose = FALSE)
    ))
    # if so, remove from random-effects here
    random_effect_terms <- random_effect_terms[!(random_effect_terms %in% cleaned_terms)]

    if (!.is_empty(random_effect_terms) && !.is_empty(constant_values)) {

      # need to check if predictions are conditioned on specific
      # value if random effect

      if (inherits(model, c("glmmTMB", "brmsfit", "MixMod"))) {
        for (i in random_effect_terms) {
          if (i %in% names(constant_values)) {
            datlist[[i]] <- NA
            constant_values[i] <- "NA (population-level)"
          }
        }
      } else if (inherits(model, c("merMod", "rlmerMod", "lme"))) {
        for (i in random_effect_terms) {
          if (i %in% names(constant_values)) {
            datlist[[i]] <- 0
            constant_values[i] <- "0 (population-level)"
          }
        }
      }

      if (inherits(model, "rlmerMod")) {
        datlist[] <- lapply(colnames(datlist), function(x) {
          if (x %in% names(constant_values) && !(x %in% random_effect_terms) && is.factor(datlist[[x]])) {
            levels(datlist[[x]]) <- levels(model_frame[[x]])
          }
          datlist[[x]]
        })
      }
    }
  }

  # for numeric variables, that are coerced to factors on-the-fly via formula,
  # convert back to numeric, so types match with original data. some models,
  # like brms, would fail for type mismatch
  if (!is.null(on_the_fly_factors)) {
    on_the_fly_factors <- on_the_fly_factors[on_the_fly_factors %in% colnames(datlist)]
    if (length(on_the_fly_factors)) {
      for (i in on_the_fly_factors) {
        datlist[[i]] <- .factor_to_numeric(datlist[[i]])
      }
    }
  }

  # save constant values as attribute
  attr(datlist, "constant.values") <- constant_values
  attr(datlist, "n.trials") <- n.trials

  # remember if grouping "factor" is numeric. this is possibly required
  # later when plotting data points for continuous predictors that are
  # held constant at their mean/sd values or similar.
  if (length(terms) > 1) {
    attr(datlist, "continuous.group") <- is.numeric(datlist[[2]])
  } else {
    attr(datlist, "continuous.group") <- FALSE
  }

  w <- insight::find_weights(model)
  if (!is.null(w) && !inherits(model, "brmsfit")) {
    # don't overwrite variables
    w <- setdiff(w, colnames(datlist))
    # for lme, can't be NA
    if (inherits(model, c("lme", "gls"))) {
      datlist[w] <- 1
    } else {
      datlist[w] <- NA_real_
    }
  }

  datlist
}



.add_offset_to_mf <- function(x, model_frame, offset_term) {
  # first try, parent frame
  dat <- .safe(eval(x$call$data, envir = parent.frame()))

  if (is.null(dat)) {
    # second try, global env
    dat <- .safe(eval(x$call$data, envir = globalenv()))
  }

  if (!is.null(dat) && .obj_has_name(x$call, "subset")) {
    dat <- subset(dat, subset = eval(x$call$subset))
  }

  tryCatch(
    {
      dat <- stats::na.omit(dat[c(intersect(colnames(model_frame), colnames(dat)), offset_term)])
      cbind(model_frame, dat[offset_term])
    },
    error = function(e) {
      model_frame
    }
  )
}


.get_model_data <- function(model) {
  # get model frame
  model_frame <- insight::get_data(model, source = "frame", verbose = FALSE)

  # sanity check - could data be extracted from model frame?
  if (is.null(model_frame)) {
    model_frame <- .safe(insight::get_data(model, source = "environment", verbose = FALSE))
  }

  # tibbles are not supported
  if (inherits(model_frame, c("tbl", "tbl_df"))) {
    class(model_frame) <- "data.frame"
  }

  model_frame
}
