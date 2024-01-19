#' @export
format.ggeffects <- function(x,
                             variable_labels = FALSE,
                             value_labels = FALSE,
                             group_name = FALSE,
                             row_header_separator = ", ",
                             ...) {
  insight::check_if_installed("datawizard")

  # use value labels as values for focal term
  if (isTRUE(value_labels)) {
    labs <- get_x_labels(x, case = NULL)
    vals <- x$x

    if (!is.null(labs)) {
      x$x <- format(labs, justify = "right")
      labs <- format(sprintf("[%g]", vals), justify = "left")
      x$x <- paste(labs, x$x, sep = " ")
    }
  }

  # use variable labels or variable names as column header?
  if (isTRUE(variable_labels)) {
    x_label <- attributes(x)$x.title
    predicted_label <- attributes(x)$title
  } else {
    x_label <- attributes(x)$terms[1]
    predicted_label <- "Predicted"
  }

  has_groups <- .obj_has_name(x, "group") && length(unique(x$group)) > 1
  has_facets <- .obj_has_name(x, "facet") && length(unique(x$facet)) > 1
  has_panel <- .obj_has_name(x, "panel") && length(unique(x$panel)) > 1
  has_response <- .obj_has_name(x, "response.level") && length(unique(x$response.level)) > 1

  sort_columns <- c("response.level", "group", "facet", "panel")[c(has_response, has_groups, has_facets, has_panel)]
  x <- datawizard::data_arrange(x, sort_columns)

  x$CI <- 0.95
  colnames(x)[colnames(x) == "conf.low"] <- "CI_low"
  colnames(x)[colnames(x) == "conf.high"] <- "CI_high"

  x$x <- insight::format_value(x$x, protect_integers = TRUE, ...)
  x <- insight::format_table(x, zap_small = TRUE, ci_brackets = c("(", ")"), ...)

  x$std.error <- NULL

  # add variable name to group levels?
  if (isTRUE(group_name)) {
    for (i in sort_columns) {
      x[[i]] <- paste(i, x[[i]], sep = ": ")
    }
  }

  row_header_labels <- apply(x[sort_columns], 1, paste, collapse = row_header_separator)
  x[sort_columns] <- NULL

  colnames(x)[1] <- x_label
  colnames(x)[2] <- predicted_label

  x$groups <- row_header_labels
  x
}




# xx <- format(x, row_header_separator = "\n# ", group_name = TRUE)
# captions <- paste0("# ", unique(xx$groups))
# xx <- split(xx, xx$groups)

# xx <- lapply(xx, function(i) {
#   i$groups <- NULL
#   i
# })

# insight::export_table(xx, title = as.list(captions))

