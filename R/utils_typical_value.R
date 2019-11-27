#' @importFrom stats median
.typical_value <- function(x, fun = "mean", weights = NULL, ...) {

  # check if we have named vectors and find the requested function
  # for special functions for factors, convert to numeric first

  fnames <- names(fun)

  if (!is.null(fnames)) {
    if (is.integer(x)) {
      fun <- fun[which(fnames %in% c("integer", "i"))]
      x <- as.numeric(x)
    } else if (is.numeric(x)) {
      fun <- fun[which(fnames %in% c("numeric", "n"))]
    } else if (is.factor(x)) {
      fun <- fun[which(fnames %in% c("factor", "f"))]
      if (fun != "mode") x <- as.numeric(x, keep.labels = FALSE)
    }
  }

  # for weighted mean, check that weights are of same length as x

  if (fun == "weighted.mean" && !is.null(weights)) {

    # make sure weights and x have same length

    if (length(weights) != length(x)) {
      # if not, tell user and change function to mean
      warning("Vector of weights is of different length than `x`. Using `mean` as function for typical value.", call. = F)
      fun <- "mean"
    }


    # make sure weights are differen from 1

    if (all(weights == 1)) {
      # if not, tell user and change function to mean
      warning("All weight values are `1`. Using `mean` as function for typical value.", call. = F)
      fun <- "mean"
    }
  }


  # no weights, than use normal mean function

  if (fun == "weighted.mean" && is.null(weights)) fun <- "mean"


  if (fun == "median")
    myfun <- get("median", asNamespace("stats"))
  else if (fun == "weighted.mean")
    myfun <- get("weighted.mean", asNamespace("stats"))
  else if (fun == "mode")
    myfun <- get(".mode_value", asNamespace("sjmisc"))
  else if (fun == "zero")
    return(0)
  else
    myfun <- get("mean", asNamespace("base"))

  if (is.integer(x)) {
    stats::median(x, na.rm = TRUE)
  } else if (is.numeric(x)) {
    if (fun == "weighted.mean")
      do.call(myfun, args = list(x = x, na.rm = TRUE, w = weights, ...))
    else
      do.call(myfun, args = list(x = x, na.rm = TRUE, ...))
  } else if (is.factor(x)) {
    if (fun != "mode")
      levels(x)[1]
    else
      .mode_value(x)
  } else {
    .mode_value(x)
  }
}


.mode_value <- function(x, ...) {
  # create frequency table, to find most common value
  counts <- table(x)
  modus <- names(counts)[max(counts) == counts]

  # in case all values appear equally often, use first value
  if (length(modus) > 1) modus <- modus[1]

  # check if it's numeric
  if (!is.na(suppressWarnings(as.numeric(modus))))
    as.numeric(modus)
  else
    modus
}
