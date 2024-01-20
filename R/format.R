#' @param variable_labels Logical, if `TRUE` variable labels are used as column
#' headers. If `FALSE`, variable names are used.
#' @param value_labels Logical, if `TRUE`, value labels are used as values in
#' the table output. If `FALSE`, the numeric values or factor levels are used.
#' @param row_header_separator Character, separator between the different
#' subgroups in the table output.
#' @param n Number of rows to print per subgroup. If `NULL`, a default number
#' of rows is printed, depending on the number of subgroups.
#'
#' @rdname print
#' @export
format.ggeffects <- function(x,
                             variable_labels = FALSE,
                             value_labels = FALSE,
                             group_name = FALSE,
                             row_header_separator = ", ",
                             n,
                             ...) {
  # we need to determine how many rows to print. this requires the original
  # data frame including attributes, that's why this code comes first
  nrow_to_print <- .nrows_to_print(x, n)
  focal_terms <- attributes(x)$terms

  # fix terms for survival models
  a1 <- attr(x, "fitfun", exact = TRUE)
  a2 <- attr(x, "y.title", exact = TRUE)

  if (!is.null(a1) && !is.null(a2) && a1 == "coxph" && a2 != "Risk Score" && !"time" %in% focal_terms) {
    focal_terms <- c("time", focal_terms)
  }

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
    x_label <- focal_terms[1]
    predicted_label <- "Predicted"
  }

  # check if we have groups, facets, panels or response levels
  has_groups <- .obj_has_name(x, "group") && length(unique(x$group)) > 1
  has_facets <- .obj_has_name(x, "facet") && length(unique(x$facet)) > 1
  has_panel <- .obj_has_name(x, "panel") && length(unique(x$panel)) > 1
  has_response <- .obj_has_name(x, "response.level") && length(unique(x$response.level)) > 1

  # check which columns we have - we want to sort by "subgroups"
  sort_columns <- c("response.level", "group", "facet", "panel")[c(has_response, has_groups, has_facets, has_panel)]

  if (length(sort_columns)) {
    insight::check_if_installed("datawizard")
    x <- datawizard::data_arrange(x, sort_columns)
  }

  # format column names, to make it work with insight-formatting-functions
  x$CI <- 0.95
  colnames(x)[colnames(x) == "conf.low"] <- "CI_low"
  colnames(x)[colnames(x) == "conf.high"] <- "CI_high"

  # format data frame
  x$x <- insight::format_value(x$x, protect_integers = TRUE, ...)
  x <- insight::format_table(x, zap_small = TRUE, ci_brackets = c("(", ")"), ...)

  x$std.error <- NULL
  row_header_labels <- NULL

  # when we have multiple focal terms, we create proper subheadings here
  if (length(sort_columns)) {
    # add variable name to group levels?
    if (isTRUE(group_name)) {
      for (i in sort_columns) {
        prefix <- switch(
          i,
          response.level = "Response level",
          group = ifelse(length(focal_terms) > 1, focal_terms[2], ""),
          facet = ifelse(length(focal_terms) > 2, focal_terms[3], ""),
          panel = ifelse(length(focal_terms) > 3, focal_terms[4], ""),
          ""
        )
        prefix <- format(prefix, justify = "right", width = max(nchar(focal_terms)))
        x[[i]] <- paste(prefix, x[[i]], sep = ": ")
      }
    }

    # create labels, based on values from the different sort-columns ("subgroups")
    row_header_labels <- apply(x[sort_columns], 1, paste, collapse = row_header_separator)
    x[sort_columns] <- NULL
  }

  colnames(x)[1] <- x_label
  colnames(x)[2] <- predicted_label

  x$groups <- row_header_labels

  # split by groups
  if (!is.null(x$group) && insight::n_unique(x$group) == 1) {
    x$group <- NULL
    x <- x[.get_sample_rows(x, n = nrow_to_print), , drop = FALSE]
  } else {
    # split by groups, apply row selection (filtering), and combine data frame
    tmp <- lapply(split(x, x$group), function(i) {
      i[.get_sample_rows(i, n = nrow_to_print), , drop = FALSE]
    })
    # create data frame w/o rownames
    x <- as.data.frame(do.call(rbind, tmp))
  }

  rownames(x) <- NULL
  x
}


.nrows_to_print <- function(x, n = 10) {
  # if we have groups, show n rows per group
  .n <- 1

  # remember if we have a factor
  x_is_factor <- identical(attr(x, "x.is.factor"), "1") && is.factor(x$x)

  # we do not simply count rows, but rather the number of combinations
  # when we have facets / groups / response.levels. These are separated
  # with own heading, making the output probably too long. Thus, we
  # decide on how many rows per "subheading" to be printed also based on
  # the number of combinations of groups, facets and response level
  has_groups <- .obj_has_name(x, "group") && length(unique(x$group)) > 1
  has_facets <- .obj_has_name(x, "facet") && length(unique(x$facet)) > 1
  has_panel <- .obj_has_name(x, "panel") && length(unique(x$panel)) > 1
  has_response <- .obj_has_name(x, "response.level") && length(unique(x$response.level)) > 1

  # here comes the top-secret formula that calculates the perfect number of rows
  if (has_groups) {
    .n <- .n_distinct(x$group)
  }
  if (has_facets) {
    .n <- .n * .n_distinct(x$facet)
  }
  if (has_panel) {
    .n <- .n * .n_distinct(x$panel)
  }
  if (has_response) {
    .n <- .n * .n_distinct(x$response.level)
  }

  # make sure that by default not too many rows are printed. The larger ".n" is
  # (i.e. the more subheadings we have, see code above), the fewer rows we want
  # per subheading. For factors, however, we want to show all levels
  if (missing(n) && !x_is_factor) {
    n <- if (.n >= 6) {
      4
    } else if (.n >= 4 && .n < 6) {
      5
    } else if (.n >= 2 && .n < 4) {
      6
    } else {
      8
    }
  } else if (x_is_factor) {
    n <- Inf
  }
  n
}


# xx <- format(x, row_header_separator = "\n# ", group_name = TRUE)
# captions <- paste0("# ", unique(xx$groups))
# xx <- split(xx, xx$groups)

# xx <- lapply(xx, function(i) {
#   i$groups <- NULL
#   i
# })

# insight::export_table(xx, title = as.list(captions))
