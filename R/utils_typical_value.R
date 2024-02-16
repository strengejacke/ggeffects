.typical_value <- function(x,
                           fun = "mean",
                           weights = NULL,
                           predictor = NULL,
                           log_terms = NULL,
                           ...) {

  # we may have factors converted on the fly in the formula, which are, however,
  # numeric in the original data. coerce to factor here
  if (isTRUE(attributes(x)$factor)) {
    x <- as.factor(x)
  }

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
      if (!fun %in% c("mode", "reference")) {
        insight::format_alert("Function for typical value for factors must be either `mode` or `reference`. Using `reference` as function for typical value.") # nolint
        fun <- "reference"
      }
    }
  }

  # for weighted mean, check that weights are of same length as x

  if (fun == "weighted.mean" && !is.null(weights)) {
    # make sure weights and x have same length
    if (length(weights) != length(x)) {
      # if not, tell user and change function to mean
      insight::format_alert("Vector of weights is of different length than `x`. Using `mean` as function for typical value.") # nolint
      fun <- "mean"
    }
    # make sure weights are different from 1
    if (all(weights == 1)) {
      # if not, tell user and change function to mean
      insight::format_alert("All weight values are `1`. Using `mean` as function for typical value.")
      fun <- "mean"
    }
  }

  # no weights, than use normal mean function
  if (fun == "weighted.mean" && is.null(weights)) fun <- "mean"

  # return 0 if "fun" is "zero"
  if (fun == "zero") return(0)

  myfun <- switch(fun,
    mean = get("mean", asNamespace("base")),
    median = get("median", asNamespace("stats")),
    weighted.mean = get("weighted.mean", asNamespace("stats")),
    mode = get(".mode_value", asNamespace("ggeffects")),
    get("mean", asNamespace("base"))
  )

  if (is.integer(x)) {
    out <- stats::median(x, na.rm = TRUE)
  } else if (is.numeric(x)) {
    if (fun == "weighted.mean") {
      out <- do.call(myfun, args = list(x = x, na.rm = TRUE, w = weights, ...))
    } else {
      out <- do.call(myfun, args = list(x = x, na.rm = TRUE, ...))
    }
  } else if (is.factor(x)) {
    if (fun != "mode") {
      out <- levels(x)[1]
    } else {
      out <- .mode_value(x)
    }
  } else {
    out <- .mode_value(x)
  }

  # if a log-transformed variable is held constant, we need to check
  # that it's not negative for its typical value - else, predict()
  # might fail due to log(<negative number>)...

  if (!is.null(log_terms) && !is.null(predictor) && predictor %in% log_terms && out <= 0) {
    out <- 0.5
  }

  out
}


.mode_value <- function(x, ...) {
  uniqv <- unique(x)
  tab <- tabulate(match(x, uniqv))
  idx <- which.max(tab)
  modus <- uniqv[idx]
  # check if it's numeric
  if (is.numeric(modus)) {
    modus
  } else {
    as.character(modus)
  }
}
