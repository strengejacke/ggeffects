#' @importFrom sjmisc round_num
#' @importFrom crayon blue italic
#' @export
print.ggeffects <- function(x, n = 10, digits = 3, ...) {
  cat("\n")

  lab <- attr(x, "title", exact = TRUE)
  if (!is.null(lab)) cat(crayon::blue(sprintf("# %s", lab)), "\n")

  lab <- attr(x, "x.title", exact = TRUE)
  if (!is.null(lab)) cat(crayon::blue(sprintf("# x = %s", lab)), "\n")

  x <- sjmisc::round_num(x, digits = digits)

  if (nrow(x) > n) {
    remain <- nrow(x) - n
    x <- x[1:n, ]
  } else {
    remain <- NULL
  }

  cat("\n")
  print.data.frame(x, ..., row.names = FALSE, quote = FALSE)

  if (!is.null(remain)) {
    cat(crayon::italic(sprintf(" ... and %i more rows.", remain)))
  }

  cat("\n\n")
}
