#' @importFrom sjmisc to_factor typical_value is_empty to_character
#' @importFrom stats terms median
#' @importFrom purrr map map_lgl map_df modify_if compact
#' @importFrom sjlabelled as_numeric
# fac.typical indicates if factors should be held constant or not
# need to be false for computing std.error for merMod objects
get_expanded_data <- function(model, mf, terms, typ.fun, fac.typical = TRUE, pretty.message = TRUE, condition = NULL, emmeans.only = FALSE) {
  # special handling for coxph
  if (inherits(model, "coxph")) mf <- dplyr::select(mf, -1)

  # make sure we don't have arrays as variables
  mf <- purrr::modify_if(mf, is.array, as.data.frame)

  # check for logical variables, might not work
  if (any(purrr::map_lgl(mf, is.logical))) {
    stop("Variables of type 'logical' do not work, please coerce to factor and fit the model again.", call. = FALSE)
  }

  # # make sure we don't have arrays as variables
  # mf[, 2:ncol(mf)] <- purrr::modify_if(mf[, 2:ncol(mf)], is.array, as.vector)
  # mf <- as.data.frame(mf)

  # any weights?
  w <- get_model_weights(model)
  if (all(w == 1)) w <- NULL

  # clean variable names
  colnames(mf) <- insight::clean_names(colnames(mf))

  # get specific levels
  first <- get_xlevels_vector(terms, mf)
  # and all specified variables
  rest <- get_clear_vars(terms)


  # check if user has any predictors with log-transformatio inside
  # model formula, but *not* used back-transformation "exp". Tell user
  # so she's aware of the problem

  tryCatch(
    {
      if (!inherits(model, "brmsfit") && pretty.message) {
        log.terms <- grepl("^log\\(([^,)]*).*", x = attr(stats::terms(model), "term.labels", exact = TRUE))
        if (any(log.terms)) {
          clean.term <- insight::find_predictors(model, effects = "all", component = "all", flatten = TRUE)[which(log.terms)]
          exp.term <- string_ends_with(pattern = "[exp]", x = terms)

          if (sjmisc::is_empty(exp.term) || get_clear_vars(terms)[exp.term] != clean.term) {
            message(sprintf("Model has log-transformed predictors. Consider using `terms=\"%s [exp]\"` to back-transform scale.", clean.term[1]))
          }
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

  use.all <- FALSE
  if (has_splines(model) && !uses_all_tag(terms)) {
    if (inherits(model, c("Gam", "gam", "vgam", "glm", "lm")))
      use.all <- TRUE
    else if (pretty.message) {
      message(sprintf("Model contains splines or polynomial terms. Consider using `terms=\"%s [all]\"` to get smooth plots. See also package-vignette 'Marginal Effects at Specific Values'.", rest[1]))
      pretty.message <- FALSE
    }
  }

  if (has_poly(model) && !uses_all_tag(terms) && !use.all) {
    if (inherits(model, c("Gam", "gam", "vgam", "glm", "lm")))
      use.all <- TRUE
    else if (pretty.message) {
      message(sprintf("Model contains polynomial or cubic / quadratic terms. Consider using `terms=\"%s [all]\"` to get smooth plots. See also package-vignette 'Marginal Effects at Specific Values'.", rest[1]))
      pretty.message <- FALSE
    }
  }


  # find terms for which no specific values are given
  xl.remain <- which(!(rest %in% names(first)))

  # prettify numeric vectors, get representative values
  xl <- prettify_data(xl.remain, mf, rest, use.all = use.all)
  names(xl) <- rest[xl.remain]
  first <- c(first, xl)

  # get names of all predictor variable
  alle <- insight::find_predictors(model, effects = "all", component = "all", flatten = TRUE)

  # remove response, if necessary
  resp <- insight::find_response(model, combine = FALSE)

  if (!is.null(resp) && resp %in% alle) {
    alle <- alle[-which(alle == resp)]
  }


  # get count of terms, and number of columns
  term.cnt <- length(alle)


  # remove NA from values, so we don't have expanded data grid
  # with missing values. this causes an error with predict()

  if (any(purrr::map_lgl(first, ~ anyNA(.x)))) {
    first <- purrr::map(first, ~ as.vector(stats::na.omit(.x)))
  }


  # names of predictor variables may vary, e.g. if log(x)
  # or poly(x) etc. is used. so check if we have correct
  # predictor names that also appear in model frame

  ## TODO brms does currently not support "terms()" generic

  if (sum(!(alle %in% colnames(mf))) > 0 && !inherits(model, "brmsfit")) {
    # get terms from model directly
    alle <- attr(stats::terms(model), "term.labels", exact = TRUE)
  }

  # 2nd check
  if (is.null(alle) || sum(!(alle %in% colnames(mf))) > 0) {
    # get terms from model frame column names
    alle <- colnames(mf)
    # we may have more terms now, e.g. intercept. remove those now
    if (length(alle) > term.cnt) alle <- alle[2:(term.cnt + 1)]
  }

  # keep those, which we did not process yet
  alle <- alle[!(alle %in% names(first))]

  # if we have weights, and typical value is mean, use weighted means
  # as function for the typical values

  if (!sjmisc::is_empty(w) && length(w) == nrow(mf) && typ.fun == "mean")
    typ.fun <- "weighted.mean"

  if (typ.fun == "weighted.mean" && sjmisc::is_empty(w))
    typ.fun <- "mean"


  # do we have variables that should be held constant at a
  # specific value?

  if (!is.null(condition) && !is.null(names(condition))) {
    first <- c(first, as.list(condition))
    alle <- alle[!(alle %in% names(condition))]
  }


  # add all to list. For those predictors that have to be held constant,
  # use "typical" values - mean/median for numeric values, reference
  # level for factors and most common element for character vectors

  if (isTRUE(emmeans.only)) {
    const.values <-
      lapply(alle, function(.x) {
        x <- mf[[.x]]
        if (!is.factor(x))
          sjmisc::typical_value(x, fun = typ.fun, weights = w)
      })
    names(const.values) <- alle
    const.values <- purrr::compact(const.values)
  } else if (fac.typical) {
    const.values <- lapply(
      mf[, alle, drop = FALSE],
      function(x) {
        if (is.factor(x)) x <- droplevels(x)
        sjmisc::typical_value(x, fun = typ.fun, weights = w)
      })
  } else {
    re.grp <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)
    # if factors should not be held constant (needed when computing
    # std.error for merMod objects), we need all factor levels,
    # and not just the typical value
    const.values <-
      lapply(alle, function(.x) {
        # get group factors from random effects
        is.re.grp <- !is.null(re.grp) && .x %in% re.grp
        x <- mf[[.x]]
        # only get levels if not random effect
        if (is.factor(x) && !is.re.grp) {
          levels(droplevels(x))
        } else {
          if (is.factor(x)) x <- droplevels(x)
          sjmisc::typical_value(x, fun = typ.fun, weights = w)
        }
      })
    names(const.values) <- alle
  }

  # for brms-models with additional response information, we need
  # also the number of trials to calculate predictions

  fam.info <- insight::model_info(model)
  n.trials <- NULL

  if (!is.null(fam.info) && fam.info$is_trial && inherits(model, "brmsfit")) {
    tryCatch(
      {
        rv <- insight::find_response(model, combine = FALSE)
        n.trials <- as.integer(stats::median(mf[[rv[2]]]))
        if (!sjmisc::is_empty(n.trials)) {
          const.values <- c(const.values, list(n.trials))
          names(const.values)[length(const.values)] <- rv[2]
        }
      },
      error = function(x) { NULL }
    )
  }

  # for MixMod, we need mean value of response as well...
  if (inherits(model, c("MixMod", "MCMCglmm"))) {
    const.values <- c(const.values, sjmisc::typical_value(insight::get_response(model)))
    names(const.values)[length(const.values)] <- resp
  }

  # add constant values.
  first <- c(first, const.values)


  # stop here for emmeans-objects

  if (isTRUE(emmeans.only)) {

    # remove grouping factor of RE from constant values
    # only applicable for MixMod objects
    re.terms <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)

    if (inherits(model, "MixMod") && !is.null(re.terms) && !sjmisc::is_empty(const.values) && any(re.terms %in% names(const.values))) {
      const.values <- const.values[!(names(const.values) %in% re.terms)]
    }

    # save names
    fn <- names(first)

    # restore original type
    first <- purrr::map(fn, function(x) {
      # check for consistent vector type: numeric
      if (is.numeric(mf[[x]]) && !is.numeric(first[[x]]))
        return(sjlabelled::as_numeric(first[[x]]))

      # check for consistent vector type: factor
      if (is.factor(mf[[x]]) && !is.factor(first[[x]]))
        return(sjmisc::to_character(first[[x]]))

      # else return original vector
      return(first[[x]])
    })

    # add back names
    names(first) <- fn

    # save constant values as attribute
    attr(first, "constant.values") <- const.values
    attr(first, "n.trials") <- n.trials

    return(first)
  }


  # create data frame with all unqiue combinations
  dat <- as.data.frame(expand.grid(first))


  # we have to check type consistency. If user specified certain value
  # (e.g. "education [1,3]"), these are returned as string and coerced
  # to factor, even if original vector was numeric. In this case, we have
  # to coerce back these variables. Else, predict() complains that model
  # was fitted with numeric, but newdata has factor (or vice versa).

  datlist <- purrr::map(colnames(dat), function(x) {

    # check for consistent vector type: numeric
    if (is.numeric(mf[[x]]) && !is.numeric(dat[[x]]))
      return(sjlabelled::as_numeric(dat[[x]]))

    # check for consistent vector type: factor
    if (is.factor(mf[[x]]) && !is.factor(dat[[x]]))
      return(sjmisc::to_factor(dat[[x]]))

    # else return original vector
    return(dat[[x]])
  })


  # get list names. we need to remove patterns like "log()" etc.
  names(datlist) <- names(first)
  datlist <- as.data.frame(datlist)


  # check if predictions should be conditioned on random effects,
  # but not on each group level. If so, set random effect to NA
  # which will return predictions on a population level.
  # See ?glmmTMB::predict

  if (inherits(model, c("glmmTMB", "merMod", "MixMod", "brmsfit"))) {
    cleaned.terms <- get_clear_vars(terms)
    re.terms <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)
    re.terms <- re.terms[!(re.terms %in% cleaned.terms)]

    if (!sjmisc::is_empty(re.terms) && !sjmisc::is_empty(const.values)) {

      # need to check if predictions are conditioned on specific
      # value if random effect

      if (inherits(model, c("glmmTMB", "brmsfit", "MixMod"))) {
        for (i in re.terms) {
          if (i %in% names(const.values)) {
            datlist[[i]] <- NA
            const.values[i] <- "NA (population-level)"
          }
        }
      } else if (inherits(model, "merMod")) {
        for (i in re.terms) {
          if (i %in% names(const.values)) {
            datlist[[i]] <- 0
            const.values[i] <- "0 (population-level)"
          }
        }
      }
    }
  }


  # save constant values as attribute
  attr(datlist, "constant.values") <- const.values
  attr(datlist, "n.trials") <- n.trials

  datlist
}


#' @importFrom sjmisc is_empty
#' @importFrom dplyr slice
#' @importFrom insight clean_names
get_sliced_data <- function(fitfram, terms) {
  # check if we have specific levels in square brackets
  x.levels <- get_xlevels_vector(terms)

  # if we have any x-levels, go on and filter
  if (!sjmisc::is_empty(x.levels) && !is.null(x.levels)) {
    # get names of covariates that should be filtered
    x.lvl.names <- names(x.levels)

    # slice data, only select observations that have specified
    # levels for the grouping variables
    for (i in seq_len(length(x.levels)))
      fitfram <- dplyr::slice(fitfram, which(fitfram[[x.lvl.names[i]]] %in% x.levels[[i]]))
  }

  # clean variable names
  colnames(fitfram) <- insight::clean_names(colnames(fitfram))

  fitfram
}
