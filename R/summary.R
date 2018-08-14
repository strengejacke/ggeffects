#' @importFrom purrr map flatten_df
#' @importFrom dplyr select group_by pull
#' @importFrom tidyr nest
#' @importFrom rlang .data
#' @export
summary.ggeffects <- function(object, ...) {

  # do we have groups and facets?
  has_groups <- obj_has_name(object, "group") && length(unique(object$group)) > 1
  has_facets <- obj_has_name(object, "facet") && length(unique(object$facet)) > 1

  # print title
  cat(sprintf("## %s\n", get_title(object)))
  cat(sprintf("   x = %s\n", get_x_title(object)))

  if (!has_groups) {
    cat("\n")
    object <- dplyr::select(object, -.data$group)
    print(object, ...)
  } else if (has_groups && !has_facets) {
    x <- object %>%
      dplyr::group_by(.data$group) %>%
      tidyr::nest()

    for (i in 1:nrow(x)) {
      cat(sprintf("\ngroup: %s\n", dplyr::pull(x[i, 1])))
      print(purrr::flatten_df(x[i, 2]), ...)
    }
  } else {
    x <- object %>%
      dplyr::group_by(.data$group, .data$facet) %>%
      tidyr::nest()

    for (i in 1:nrow(x)) {
      cat(sprintf("\ngroups: %s; %s\n", dplyr::pull(x[i, 1]), dplyr::pull(x[i, 2])))
      print(purrr::flatten_df(x[i, 3]), ...)
    }
  }

  cv <- purrr::map(
    attr(object, "constant.values"),
    function(x) {
      if (is.numeric(x))
        sprintf("%.2f", x)
      else
        as.character(x)
    })

  cv.names <- names(cv)
  cv.space <- max(nchar(cv.names))

  cat(paste0("\nAdjusted for:\n", paste0(sprintf("* %*s = %s", cv.space, cv.names, cv), collapse = "\n")))
}
