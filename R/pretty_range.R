#' @title Create a pretty sequence over a range of a vector
#' @name pretty_range
#'
#' @description Creates an evenly spaced, pretty sequence of numbers for a
#'   range of a vector.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector with a range corresponding to the minimum and maximum
#'   values of \code{x}.
#'
#' @examples
#' library(sjmisc)
#' data(efc)
#'
#' x <- std(efc$c12hour)
#' x
#' # pretty range for vectors with decimal points
#' pretty_range(x)
#'
#' # pretty range for large range
#' pretty_range(1:1000)
#'
#' @export
pretty_range <- function(x) {
  ra.min <- min(x, na.rm = TRUE)
  ra.max <- max(x, na.rm = TRUE)
  ra <- seq(ra.min, ra.max, sqrt(ra.max - ra.min) / 10)
  pretty(ra, n = 10^(floor(log10(length(ra)))))
}
