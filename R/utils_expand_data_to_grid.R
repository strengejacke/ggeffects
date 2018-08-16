#' @importFrom sjstats pred_vars typical_value var_names
#' @importFrom sjmisc to_factor is_empty
#' @importFrom stats terms
#' @importFrom purrr map map_lgl map_df modify_if
#' @importFrom sjlabelled as_numeric
# fac.typical indicates if factors should be held constant or not
# need to be false for computing std.error for merMod objects
get_expanded_data <- function(model, mf, terms, typ.fun, fac.typical = TRUE, type = "fe", pretty.message = TRUE, condition = NULL) {
  # special handling for coxph
  if (inherits(model, "coxph")) mf <- dplyr::select(mf, -1)

  # make sure we don't have arrays as variables
  mf <- suppressWarnings(as.data.frame(purrr::modify_if(mf, is.array, as.vector)))

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
  colnames(mf) <- sjstats::var_names(colnames(mf))

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
          clean.term <- sjstats::pred_vars(model)[which(log.terms)]
          exp.term <- string_ends_with(pattern = "[exp]", x = terms)

          if (sjmisc::is_empty(exp.term) || get_clear_vars(terms)[exp.term] != clean.term) {
            message(sprintf("Model has log-transformed predictors. Consider using `terms = \"%s [exp]\"` to back-transform scale.", clean.term))
          }
        }
      }
    },
    error = function(x) { NULL },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )


  # find terms for which no specific values are given
  xl.remain <- which(!(rest %in% names(first)))

  # prettify numeric vectors, get representative values
  xl <- prettify_data(xl.remain, mf, rest)
  names(xl) <- rest[xl.remain]
  first <- c(first, xl)

  # get names of all predictor variable
  alle <- sjstats::pred_vars(model)

  # get count of terms, and number of columns
  term.cnt <- length(alle)


  # remove NA from values, so we don't have expanded data grid
  # with missing values. this causes an error with predict()

  if (any(purrr::map_lgl(first, ~ anyNA(.x)))) {
    first <- purrr::map(first, ~ as.vector(na.omit(.x)))
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

  if (fac.typical) {
    const.values <- lapply(mf[, alle, drop = FALSE], function(x) sjstats::typical_value(x, typ.fun, weight.by = w))
  } else {
    # if factors should not be held constant (needed when computing
    # std.error for merMod objects), we need all factor levels,
    # and not just the typical value
    const.values <-
      lapply(mf[, alle, drop = FALSE], function(x) {
        if (is.factor(x))
          levels(x)
        else
          sjstats::typical_value(x, typ.fun, weight.by = w)
      })
  }

  # add constant values.
  first <- c(first, const.values)

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

  # save constant values as attribute
  attr(datlist, "constant.values") <- const.values

  datlist
}


#' @importFrom sjmisc is_empty
#' @importFrom dplyr slice
#' @importFrom sjstats var_names
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
  colnames(fitfram) <- sjstats::var_names(colnames(fitfram))

  fitfram
}
