#' @importFrom tibble as_tibble
#' @importFrom sjstats pred_vars
#' @importFrom sjmisc to_value to_factor
#' @importFrom stats terms
#' @importFrom purrr map map_lgl
get_expanded_data <- function(model, mf, terms, typ.fun) {
  # use tibble, no drop = FALSE
  mf <- tibble::as_tibble(mf)

  # clean variable names
  colnames(mf) <- get_cleaned_varnames(colnames(mf))

  # get specific levels
  first <- get_xlevels_vector(terms)
  # and all specified variables
  rest <- get_clear_vars(terms)

  # create unique combinations
  rest <- rest[!(rest %in% names(first))]
  first <- c(first, lapply(mf[, rest], unique, na.rm = TRUE))

  # get names of all predictor variable
  alle <- sjstats::pred_vars(model)

  # get count of terms, and number of columns
  term.cnt <- length(alle)


  # remove NA from values, so we don't have expanded data grid
  # with missing values. this causes an error with predict()
  if (any(purrr::map_lgl(first, ~ anyNA(.x)))) {
    first <- map(first, ~ as.vector(na.omit(.x)))
  }


  # names of predictor variables may vary, e.g. if log(x)
  # or poly(x) etc. is used. so check if we have correct
  # predictor names that also appear in model frame
  if (sum(!(alle %in% colnames(mf))) > 0) {
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

  # add all to list. For those predictors that have to be held constant,
  # use "typical" values - mean/median for numeric values, reference
  # level for factors and most common element for character vectors
  first <- c(first, lapply(mf[, alle], function(x) typical_value(x, typ.fun)))

  # create data frame with all unqiue combinations
  dat <- tibble::as_tibble(expand.grid(first))

  # we have to check type consistency. If user specified certain value
  # (e.g. "education [1,3]"), these are returned as string and coerced
  # to factor, even if original vector was numeric. In this case, we have
  # to coerce back these variables. Else, predict() complains that model
  # was fitted with numeric, but newdata has factor (or vice versa).
  datlist <- purrr::map(colnames(dat), function(x) {
    # check for consistent vector type: numeric
    if (is.numeric(mf[[x]]) && !is.numeric(dat[[x]]))
      return(sjmisc::to_value(dat[[x]]))
    # check for consistent vector type: factor
    if (is.factor(mf[[x]]) && !is.factor(dat[[x]]))
      return(sjmisc::to_factor(dat[[x]]))
    # else return original vector
    return(dat[[x]])
  })

  # get list names. we need to remove patterns like "log()" etc.
  # and give list elements names, so we can make a tibble
  names(datlist) <- names(first)

  tibble::as_tibble(datlist)
}


#' @importFrom sjmisc is_empty
#' @importFrom dplyr slice
#' @importFrom tibble as_tibble
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
  colnames(fitfram) <- get_cleaned_varnames(colnames(fitfram))

  tibble::as_tibble(fitfram)
}


get_cleaned_varnames <- function(x) {
  # do we have a "log()" pattern here? if yes, get capture region
  # which matches the "cleaned" variable name
  for (i in 1:length(x)) {
    pos <- regexpr(pattern = "log(.*)", text = x[i], perl = TRUE)
    start <- attr(pos, "capture.start", exact = TRUE)
    len <- attr(pos, "capture.length", exact = TRUE)

    # return substring or original variable
    if (start != -1 && len != -1)
      x[i] <- substr(x[i], start = start + 1, stop = start + len - 2)
    else
      x[i]
  }

  x
}


typical_value <- function(x, fun = c("mean", "median")) {
  fun <- match.arg(fun)

  if (fun == "median")
    myfun <- get("median", asNamespace("stats"))
  else
    myfun <- get("mean", asNamespace("base"))

  if (is.numeric(x))
    do.call(myfun, args = list(x = x, na.rm = TRUE))
  else if (is.factor(x))
    levels(x)[1]
  else {
    counts <- table(x)
    names(counts)[max(counts) == counts]
  }
}
