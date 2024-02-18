#' @rdname predict_response
#' @param x An object of class `ggeffects`, as returned by `predict_response()`,
#' `ggpredict()`, `ggeffect()`, `ggaverage()` or `ggemmeans()`.
#' @param terms_to_colnames Logical, if `TRUE`, standardized column names (like
#' `"x"`, `"group"` or `"facet"`) are replaced by the variable names of the focal
#' predictors specified in `terms`.
#' @inheritParams base::as.data.frame
#' @export
as.data.frame.ggeffects <- function(x,
                                    row.names = NULL,
                                    optional = FALSE,
                                    ...,
                                    stringsAsFactors = FALSE,
                                    terms_to_colnames = FALSE) {
  # get variables names
  focal <- attributes(x)$terms
  resp <- attributes(x)$response.name

  # coerce to data frame, remove attributes
  x <- as.data.frame.data.frame(x, row.names = row.names, stringsAsFactors = stringsAsFactors, ...)

  # rename columns
  if (terms_to_colnames) {
    colnames(x)[colnames(x) == "x"] <- focal[1]
    if ("group" %in% colnames(x) && length(focal) >= 2) {
      colnames(x)[colnames(x) == "group"] <- focal[2]
    }
    if ("facet" %in% colnames(x) && length(focal) >= 3) {
      colnames(x)[colnames(x) == "facet"] <- focal[3]
    }
    if ("response.level" %in% colnames(x) && !is.null(resp)) {
      colnames(x)[colnames(x) == "response.level"] <- resp
    }
  }
  x
}


# subsetting
#' @export
`[.ggeffects` <- function(x, i, j, drop = FALSE, ...) {
  att <- attributes(x)
  cl <- class(x)
  out <- as.data.frame(x)
  if (!missing(i)) {
    out <- out[i, , drop = FALSE]
  }
  if (!missing(j)) {
    out <- out[, j, drop = FALSE]
  }
  attributes(out) <- utils::modifyList(att, attributes(out))
  attr(out, "is_subset") <- TRUE
  class(out) <- cl
  out
}
