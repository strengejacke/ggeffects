#' @importFrom sjmisc to_factor typical_value is_empty to_character
#' @importFrom stats terms median
#' @importFrom purrr map map_lgl map_df modify_if compact
#' @importFrom sjlabelled as_numeric
#' @importFrom insight find_predictors find_response find_random find_weights get_weights
# factor_adjustment indicates if factors should be held constant or not
# need to be false for computing std.error for merMod objects
.data_grid <- function(model, model_frame, terms, value_adjustment, factor_adjustment = TRUE, show_pretty_message = TRUE, condition = NULL, emmeans.only = FALSE) {
  # special handling for coxph
  if (inherits(model, c("coxph", "coxme"))) {
    surv.var <- which(colnames(model_frame) == insight::find_response(model))
    model_frame <- .remove_column(model_frame, surv.var)
  }

  model_info <- .get_model_info(model)

  # make sure we don't have arrays as variables
  model_frame <- purrr::modify_if(model_frame, is.array, as.data.frame)

  # check for logical variables, might not work
  if (any(purrr::map_lgl(model_frame, is.logical))) {
    stop("Variables of type 'logical' do not work, please coerce to factor and fit the model again.", call. = FALSE)
  }

  # any weights?
  w <- insight::get_weights(model)
  if (is.null(w) || all(w == 1)) w <- NULL

  # get random effects (grouping factor)
  random_effect_terms <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)

  ## TODO check for other panelr models

  # clean variable names
  # if (!inherits(model, "wbm")) {
    colnames(model_frame) <- insight::clean_names(colnames(model_frame))
  # }


  # get specific levels
  focal_terms <- .get_representative_values(terms, model_frame)
  # and all specified variables
  all_terms <- .clean_terms(terms)


  # check if user has any predictors with log-transformatio inside
  # model formula, but *not* used back-transformation "exp". Tell user
  # so she's aware of the problem

  tryCatch(
    {
      if (!inherits(model, "brmsfit") && show_pretty_message && .has_log(model)) {
        clean.term <- insight::find_predictors(model, effects = "all", component = "all", flatten = FALSE)
        clean.term <- unlist(clean.term[c("conditional", "random", "instruments")])[.get_log_terms(model)]
        exp.term <- string_ends_with(pattern = "[exp]", x = terms)

        if (any(sjmisc::is_empty(exp.term)) || any(.clean_terms(terms)[exp.term] != clean.term)) {
          message(sprintf("Model has log-transformed predictors. Consider using `terms=\"%s [exp]\"` to back-transform scale.", clean.term[1]))
        }
      }
    },
    error = function(x) { NULL },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )


  # Check if model has splines, and if so, tell user that he may use
  # all values - except for gam and vgam models. "predict()" seems
  # stable even for large data frame for gam/vgam. Especially for
  # mixed models, computing SE and CI is very memory consuming, leading
  # to memory allocation errors. That's why by default values for continuous
  # variables are "prettified" to a smaller set of unique values.

  use_all_values <- FALSE

  if (.has_splines(model) && !.uses_all_tag(terms)) {
    if (inherits(model, c("Gam", "gam", "vgam", "glm", "lm", "brmsfit", "bamlss", "gamlss"))) {
      use_all_values <- TRUE
    } else if (show_pretty_message) {
      message(sprintf("Model contains splines or polynomial terms. Consider using `terms=\"%s [all]\"` to get smooth plots. See also package-vignette 'Marginal Effects at Specific Values'.", all_terms[1]))
      show_pretty_message <- FALSE
    }
  }

  if (.has_poly(model) && !.uses_all_tag(terms) && !use_all_values) {
    if (inherits(model, c("Gam", "gam", "vgam", "glm", "lm", "brmsfit"))) {
      use_all_values <- TRUE
    } else if (show_pretty_message) {
      message(sprintf("Model contains polynomial or cubic / quadratic terms. Consider using `terms=\"%s [all]\"` to get smooth plots. See also package-vignette 'Marginal Effects at Specific Values'.", all_terms[1]))
      show_pretty_message <- FALSE
    }
  }


  # find terms for which no specific values are given
  conditional_terms <- which(!(all_terms %in% names(focal_terms)))

  # prettify numeric vectors, get representative values
  constant_levels <- .prettify_data(
    conditional_terms = conditional_terms,
    original_model_frame = model_frame,
    terms = all_terms,
    use_all_values = use_all_values,
    show_pretty_message = show_pretty_message && model_info$is_binomial
  )
  names(constant_levels) <- all_terms[conditional_terms]
  focal_terms <- c(focal_terms, constant_levels)


  ## TODO check for other panelr models

  # get names of all predictor variable
  # if (inherits(model, "wbm")) {
  #   model_predictors <- colnames(model_frame)
  # } else {
  #   model_predictors <- insight::find_predictors(model, effects = "all", component = "all", flatten = TRUE)
  # }

  model_predictors <- insight::find_predictors(model, effects = "all", component = "all", flatten = TRUE)
  if (inherits(model, "wbm")) {
    model_predictors <- unique(c(insight::find_response(model), model_predictors, model@call_info$id, model@call_info$wave))
  }

  # get count of terms, and number of columns
  n_predictors <- length(model_predictors)


  # remove NA from values, so we don't have expanded data grid
  # with missing values. this causes an error with predict()

  if (any(purrr::map_lgl(focal_terms, ~ anyNA(.x)))) {
    focal_terms <- purrr::map(focal_terms, ~ as.vector(stats::na.omit(.x)))
  }


  ## TODO check, it should actually no longer happen that
  # the values of "model_predictors" are not in the column names of
  # the model frame "model_frame"

  # names of predictor variables may vary, e.g. if log(x)
  # or poly(x) etc. is used. so check if we have correct
  # predictor names that also appear in model frame

  ## TODO brms does currently not support "terms()" generic

  if (!inherits(model, "wbm")) {

    if (sum(!(model_predictors %in% colnames(model_frame))) > 0 && !inherits(model, "brmsfit")) {
      # get terms from model directly
      model_predictors <- attr(stats::terms(model), "term.labels", exact = TRUE)
    }

    # 2nd check
    if (is.null(model_predictors) || sum(!(model_predictors %in% colnames(model_frame))) > 0) {
      # get terms from model frame column names
      model_predictors <- colnames(model_frame)
      # we may have more terms now, e.g. intercept. remove those now
      if (length(model_predictors) > n_predictors) model_predictors <- model_predictors[2:(n_predictors + 1)]
    }

  } else {
    model_predictors <- model_predictors[model_predictors %in% colnames(model_frame)]
  }

  # keep those, which we did not process yet
  model_predictors <- model_predictors[!(model_predictors %in% names(focal_terms))]

  # if we have weights, and typical value is mean, use weighted means
  # as function for the typical values

  if (!sjmisc::is_empty(w) && length(w) == nrow(model_frame) && value_adjustment == "mean") {
    value_adjustment <- "weighted.mean"
  }

  if (value_adjustment == "weighted.mean" && sjmisc::is_empty(w)) {
    value_adjustment <- "mean"
  }


  # do we have variables that should be held constant at a
  # specific value?

  if (!is.null(condition) && !is.null(names(condition))) {
    focal_terms <- c(focal_terms, as.list(condition))
    model_predictors <- model_predictors[!(model_predictors %in% names(condition))]
  }


  # add all constant values to list. For those predictors that have to be
  # held constant, use "typical" values - mean/median for numeric values,
  # reference level for factors and most common element for character vectors

  if (isTRUE(emmeans.only)) {
    # adjust constant values, special handling for emmeans only
    constant_values <- lapply(model_predictors, function(.x) {
      x <- model_frame[[.x]]
      if (!is.factor(x) && !.x %in% random_effect_terms) {
        sjmisc::typical_value(x, fun = value_adjustment, weights = w)
      }
    })
    names(constant_values) <- model_predictors
    constant_values <- purrr::compact(constant_values)
  } else if (factor_adjustment) {
    # adjust constant values, factors set to reference level
    constant_values <- lapply(model_frame[model_predictors], function(x) {
      if (is.factor(x)) x <- droplevels(x)
      sjmisc::typical_value(x, fun = value_adjustment, weights = w)
    })
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
        sjmisc::typical_value(x, fun = value_adjustment, weights = w)
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
        n.trials <- as.integer(stats::median(model_frame[[rv[2]]]))
        if (!sjmisc::is_empty(n.trials)) {
          constant_values <- c(constant_values, list(n.trials))
          names(constant_values)[length(constant_values)] <- rv[2]
        }
      },
      error = function(x) { NULL }
    )
  }

  # for MixMod, we need mean value of response as well...
  if (inherits(model, c("MixMod", "MCMCglmm"))) {
    constant_values <- c(constant_values, sjmisc::typical_value(insight::get_response(model)))
    names(constant_values)[length(constant_values)] <- insight::find_response(model, combine = FALSE)
  }

  # add constant values.
  focal_terms <- c(focal_terms, constant_values)


  # stop here for emmeans-objects

  if (isTRUE(emmeans.only)) {

    # remove grouping factor of RE from constant values
    # only applicable for MixMod objects
    if (inherits(model, "MixMod") && !is.null(random_effect_terms) && !sjmisc::is_empty(constant_values) && any(random_effect_terms %in% names(constant_values))) {
      constant_values <- constant_values[!(names(constant_values) %in% random_effect_terms)]
    }

    # save names
    focal_term_names <- names(focal_terms)

    # restore original type
    focal_terms <- purrr::map(focal_term_names, function(x) {
      # check for consistent vector type: numeric
      if (is.numeric(model_frame[[x]]) && !is.numeric(focal_terms[[x]]))
        return(sjlabelled::as_numeric(focal_terms[[x]]))

      # check for consistent vector type: factor
      if (is.factor(model_frame[[x]]) && !is.factor(focal_terms[[x]]))
        return(sjmisc::to_character(focal_terms[[x]]))

      # else return original vector
      return(focal_terms[[x]])
    })

    # add back names
    names(focal_terms) <- focal_term_names

    # save constant values as attribute
    attr(focal_terms, "constant.values") <- constant_values
    attr(focal_terms, "n.trials") <- n.trials

    return(focal_terms)
  }


  # create data frame with all unqiue combinations
  dat <- as.data.frame(expand.grid(focal_terms))


  # we have to check type consistency. If user specified certain value
  # (e.g. "education [1,3]"), these are returned as string and coerced
  # to factor, even if original vector was numeric. In this case, we have
  # to coerce back these variables. Else, predict() complains that model
  # was fitted with numeric, but newdata has factor (or vice versa).

  datlist <- purrr::map(colnames(dat), function(x) {

    # check for consistent vector type: numeric
    if (is.numeric(model_frame[[x]]) && !is.numeric(dat[[x]]))
      return(sjlabelled::as_numeric(dat[[x]]))

    # check for consistent vector type: factor
    if (is.factor(model_frame[[x]]) && !is.factor(dat[[x]]))
      return(sjmisc::to_factor(dat[[x]]))

    # else return original vector
    return(dat[[x]])
  })


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

  if (inherits(model, c("glmmTMB", "merMod", "rlmerMod", "MixMod", "brmsfit", "lme"))) {
    cleaned_terms <- .clean_terms(terms)

    # check if we have fixed effects as grouping factor in random effects as well...
    cleaned_terms <- unique(c(cleaned_terms, insight::find_predictors(model, effects = "fixed", flatten = TRUE)))
    # if so, remove from random-effects here
    random_effect_terms <- random_effect_terms[!(random_effect_terms %in% cleaned_terms)]

    if (!sjmisc::is_empty(random_effect_terms) && !sjmisc::is_empty(constant_values)) {

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
    }
  }


  # save constant values as attribute
  attr(datlist, "constant.values") <- constant_values
  attr(datlist, "n.trials") <- n.trials

  w <- insight::find_weights(model)
  if (!is.null(w) && !inherits(model, "brmsfit")) {
    datlist$.w <- as.numeric(NA)
    colnames(datlist)[ncol(datlist)] <- w
  }

  datlist
}
