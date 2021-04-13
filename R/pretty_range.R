#' @title Create a pretty sequence over a range of a vector
#' @name pretty_range
#'
#' @description Creates an evenly spaced, pretty sequence of numbers for a
#'   range of a vector.
#'
#' @param x A numeric vector.
#' @param n Numeric value, indicating the size of how many values are used to
#'   create a pretty sequence. If \code{x} has a large value range (> 100),
#'   \code{n} could be something between 1 to 5. If \code{x} has a rather
#'   small amount of unique values, \code{n} could be something between
#'   10 to 20. If \code{n = NULL}, \code{pretty_range()} automatically
#'   tries to find a pretty sequence.
#' @param length Integer value, as alternative to \code{n}, defines the number of
#'   intervals to be returned.
#'
#' @return A numeric vector with a range corresponding to the minimum and
#'   maximum values of \code{x}. If \code{x} is missing, a function,
#'   pre-programmed with \code{n} and \code{length} is returned. See examples.
#'
#' @examples
#' data(iris)
#' # pretty range for vectors with decimal points
#' pretty_range(iris$Petal.Length)
#'
#' # pretty range for large range, increasing by 50
#' pretty_range(1:1000)
#'
#' # increasing by 20
#' pretty_range(1:1000, n = 7)
#'
#' # return 10 intervals
#' pretty_range(1:1000, length = 10)
#'
#' # same result
#' pretty_range(1:1000, n = 2.5)
#'
#' # function factory
#' range_n_5 <- pretty_range(n = 5)
#' range_n_5(1:1000)
#' @export
pretty_range <- function(x, n = NULL, length = NULL) {
  force(n)
  force(length)
  .pretty_range <- function(x) {
    ra.min <- min(x, na.rm = TRUE)
    ra.max <- max(x, na.rm = TRUE)
    ra <- seq(ra.min, ra.max, sqrt(ra.max - ra.min) / 10)

    if (!is.null(length)) {
      pretty(ra, n = length)
    } else {
      if (!is.null(n))
        pr <- n
      else if (.n_distinct(x) > 100)
        pr <- 3
      else if (.n_distinct(x) > 50)
        pr <- 5
      else
        pr <- 10

      pr <- pr^(floor(log10(length(ra))))

      p1 <- pretty(ra, n = pr)
      p2 <- pretty(ra, n = ceiling(pr * 1.5))
      p3 <- pretty(ra, n = 2 * pr)

      if (length(p1) >= .n_distinct(x))
        p1
      else if (length(p1) < 10 && length(p2) < 25)
        p2
      else if (length(p2) < 15 && length(p3) < 25)
        p3
      else
        p1
    }
  }

  if (missing(x)) {
    .pretty_range
  } else {
    .pretty_range(x)
  }
}
