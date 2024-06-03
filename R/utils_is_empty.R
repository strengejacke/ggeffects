.is_empty <- function(x) {
  # do we have a valid vector?
  if (!is.null(x)) {
    # if it's a character, check if we have only one element in that vector
    if (is.character(x)) {
      # characters may also be of length 0
      if (length(x) == 0) return(TRUE)
      # else, check first elements of x
      zero_len <- isTRUE((!nzchar(x, keepNA = TRUE))[1])
      if (length(x) > 0) x <- x[1]
      # we have a non-character vector here. check for length
    } else if (is.list(x)) {
      x <- .compact_list(x)
      zero_len <- length(x) == 0
    } else {
      zero_len <- length(x) == 0
    }
  }

  any(is.null(x) || zero_len || all(is.na(x)))
}
