#' @export
format.ggeffects <- function(x, ...) {
  insight::check_if_installed("datawizard")
  x_label <- attributes(x)$x.title
  predicted_label <- attributes(x)$title

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

  row_header_labels <- apply(x[sort_columns], 1, toString)
  x[sort_columns] <- NULL

  colnames(x)[1] <- x_label
  colnames(x)[2] <- predicted_label

  x$groups <- row_header_labels
  x
}
