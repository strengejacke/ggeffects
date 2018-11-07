#' @importFrom purrr map flatten_df
#' @importFrom dplyr select group_by pull n_distinct
#' @importFrom sjmisc round_num is_empty add_variables
#' @importFrom crayon blue italic red
#' @importFrom tidyr nest
#' @importFrom rlang .data
#' @export
print.ggeffects <- function(x, n = 10, digits = 3, ...) {

  # do we have groups and facets?
  has_groups <- obj_has_name(x, "group") && length(unique(x$group)) > 1
  has_facets <- obj_has_name(x, "facet") && length(unique(x$facet)) > 1
  has_se <- obj_has_name(x, "std.error")

  cat("\n")

  lab <- attr(x, "title", exact = TRUE)
  if (!is.null(lab)) cat(crayon::blue(sprintf("# %s", lab)), "\n")

  lab <- attr(x, "x.title", exact = TRUE)
  if (!is.null(lab)) cat(crayon::blue(sprintf("# x = %s", lab)), "\n")

  x <- sjmisc::round_num(x, digits = digits)

  # if we have groups, show n rows per group
  .n <- 1
  if (has_groups) .n <- dplyr::n_distinct(x$group, na.rm = T)
  if (has_facets) .n <- .n * dplyr::n_distinct(x$facet, na.rm = T)
  n <- n * .n

  if (nrow(x) > n) {
    remain <- nrow(x) - n
    x <- x[1:n, ]
  } else {
    remain <- NULL
  }

  if (!has_groups) {
    cat("\n")
    x <- dplyr::select(x, -.data$group)
    print.data.frame(x, ..., row.names = FALSE, quote = FALSE)

    if (!is.null(remain)) {
      cat(crayon::italic(sprintf(" ... and %i more rows.\n", remain)))
    }
  } else if (has_groups && !has_facets) {
    xx <- x %>%
      dplyr::group_by(.data$group) %>%
      tidyr::nest()

    for (i in 1:nrow(xx)) {
      cat(crayon::red(sprintf("\n# %s\n", dplyr::pull(xx[i, 1]))))
      print.data.frame(purrr::flatten_df(xx[i, 2]), ..., row.names = FALSE, quote = FALSE)
      if (!is.null(remain)) {
        cat(crayon::italic(sprintf(" ... and %i more rows.\n", as.integer(remain / .n))))
      }
    }
  } else {
    xx <- x %>%
      dplyr::group_by(.data$group, .data$facet) %>%
      tidyr::nest()

    for (i in 1:nrow(xx)) {
      cat(crayon::red(sprintf("\n# %s\n# %s\n", dplyr::pull(xx[i, 1]), dplyr::pull(xx[i, 2]))))
      print.data.frame(purrr::flatten_df(xx[i, 3]), ..., row.names = FALSE, quote = FALSE)
      if (!is.null(remain)) {
        cat(crayon::italic(sprintf(" ... and %i more rows.\n", as.integer(remain / .n))))
      }
    }
  }

  cv <- purrr::map(
    attr(x, "constant.values"),
    function(.x) {
      if (is.numeric(.x))
        sprintf("%.2f", .x)
      else
        as.character(.x)
    })

  if (!sjmisc::is_empty(cv)) {
    cv.names <- names(cv)
    cv.space <- max(nchar(cv.names))

    # ignore this string when determing maximum length
    poplev <- which(cv == "NA (population-level)")
    if (!sjmisc::is_empty(poplev))
      mcv <- cv[-poplev]
    else
      mcv <- cv

    cv.space2 <- max(nchar(mcv))

    cat(crayon::blue(paste0(
      "\nAdjusted for:\n",
      paste0(sprintf("* %*s = %*s", cv.space, cv.names, cv.space2, cv), collapse = "\n")
    )))
  }


  cat("\n\n")

  if (has_se) {
    message("Standard errors are on link-scale (untransformed).")
  }
}
