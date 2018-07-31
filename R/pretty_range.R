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
#' @importFrom dplyr n_distinct
#' @export
pretty_range <- function(x) {
  ra.min <- min(x, na.rm = TRUE)
  ra.max <- max(x, na.rm = TRUE)
  ra <- seq(ra.min, ra.max, sqrt(ra.max - ra.min) / 10)

  if (dplyr::n_distinct(x, na.rm = TRUE) > 100)
    pr <- 3
  else if (dplyr::n_distinct(x, na.rm = TRUE) > 50)
    pr <- 5
  else
    pr <- 10

  pr <- pr^(floor(log10(length(ra))))

  p1 <- pretty(ra, n = pr)
  p2 <- pretty(ra, n = ceiling(pr * 1.5))
  p3 <- pretty(ra, n = 2 * pr)

  if (length(p1) >= dplyr::n_distinct(x, na.rm = TRUE))
    p1
  else if (length(p1) < 10 && length(p2) < 25)
    p2
  else if (length(p2) < 15 && length(p3) < 25)
    p3
  else
    p1
}
