#' @param variable_labels Logical, if `TRUE` variable labels are used as column
#' headers. If `FALSE`, variable names are used.
#' @param value_labels Logical, if `TRUE`, value labels are used as values in
#' the table output. If `FALSE`, the numeric values or factor levels are used.
#' @param row_header_separator Character, separator between the different
#' subgroups in the table output.
#' @param collapse_ci Logical, if `TRUE`, the columns with predicted values and
#' confidence intervals are collapsed into one column, e.g. `Predicted (95% CI)`.
#' @param n Number of rows to print per subgroup. If `NULL`, a default number
#' of rows is printed, depending on the number of subgroups.
#' @param collapse_tables Logical, if `TRUE`, all tables are combined into one.
#' The tables are not split by further focal terms, but rather are added as
#' columns. Only works when there is more than one focal term.
#'
#' @rdname print
#' @export
format.ggeffects <- function(x,
                             variable_labels = FALSE,
                             value_labels = FALSE,
                             group_name = FALSE,
                             row_header_separator = ", ",
                             digits = 2,
                             collapse_ci = FALSE,
                             collapse_tables = FALSE,
                             n,
                             ...) {
  # we need to determine how many rows to print. this requires the original
  # data frame including attributes, that's why this code comes first
  nrow_to_print <- .nrows_to_print(x, n)
  focal_terms <- attributes(x)$terms
  ci_level <- attributes(x)$ci.lvl
  response_name <- attributes(x)$response.name
  # default name, used later
  if (is.null(response_name)) {
    response_name <- "Response Level"
  }

  # fix terms for survival models
  a1 <- attr(x, "fitfun", exact = TRUE)
  a2 <- attr(x, "y.title", exact = TRUE)

  if (!is.null(a1) && !is.null(a2) && a1 == "coxph" && a2 != "Risk Score" && !"time" %in% focal_terms) {
    focal_terms <- c("time", focal_terms)
  }

  # format confidence intervals
  dots <- list(...)
  if (is.null(dots$ci_brackets)) {
    dots$ci_brackets <- getOption("ggeffects_ci_brackets", c("", ""))
  }

  # set default for collapse_ci and collapse_tables
  collapse_ci <- getOption("ggeffects_collapse_ci", collapse_ci)
  collapse_tables <- getOption("ggeffects_collapse_tables", collapse_tables)

  # use value labels as values for focal term
  if (isTRUE(value_labels)) {
    labs <- get_x_labels(x, case = NULL)
    vals <- x$x

    if (!is.null(labs)) {
      x$x <- format(labs)
      labs <- format(sprintf("[%g]", vals))
      x$x <- paste(labs, x$x)
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

  # response needs to be factor
  if (has_response) {
    x$response.level <- factor(x$response.level, levels = unique(x$response.level))
  }

  if (length(sort_columns)) {
    insight::check_if_installed("datawizard")
    x <- datawizard::data_arrange(x, sort_columns)
  }

  # format column names, to make it work with insight-formatting-functions
  x$CI <- ifelse(is.null(ci_level), 95, ci_level)
  colnames(x)[colnames(x) == "conf.low"] <- "CI_low"
  colnames(x)[colnames(x) == "conf.high"] <- "CI_high"

  # sanity check - if we don't have CI columns, remove CI level as well
  if (!any(c("CI_low", "CI_high") %in% colnames(x))) {
    x$CI <- NULL
  }

  # round numeric values and format data frame
  x$x <- insight::format_value(
    x$x,
    digits = digits,
    protect_integers = TRUE,
    ...
  )

  ft_args <- list(
    x,
    zap_small = TRUE,
    digits = digits,
    ci_digits = digits
  )
  x <- do.call(insight::format_table, c(ft_args, dots))

  x$std.error <- NULL
  row_header_labels <- NULL

  # when we have multiple focal terms, we create proper subheadings here
  if (length(sort_columns)) {
    # add variable name to group levels?
    if (isTRUE(group_name) && isFALSE(collapse_tables)) {
      for (i in sort_columns) {
        prefix <- switch(
          i,
          response.level = response_name,
          group = ifelse(length(focal_terms) > 1, focal_terms[2], ""),
          facet = ifelse(length(focal_terms) > 2, focal_terms[3], ""),
          panel = ifelse(length(focal_terms) > 3, focal_terms[4], ""),
          ""
        )
        x[[i]] <- paste(prefix, x[[i]], sep = ": ")
      }
    }

    # create labels, based on values from the different sort-columns ("subgroups")
    row_header_labels <- apply(x[sort_columns], 1, paste, collapse = row_header_separator)
  }

  colnames(x)[1] <- x_label
  colnames(x)[2] <- predicted_label

  x$groups <- row_header_labels

  # clean-up for subset data frames, e.g. ggeffects[c(1:2, 4:6)]
  invalid_rows <- !nzchar(x$Predicted)
  if (any(invalid_rows)) {
    x <- x[!invalid_rows, ]
  }

  # split by groups
  if (is.null(x$groups) || length(unique(x$groups)) <= 1) {
    x <- x[.get_sample_rows(x, n = nrow_to_print), , drop = FALSE]
  } else {
    # coerce to factor, so that `split()` preserves correct order
    f <- factor(x$groups, levels = unique(x$groups))
    # split by groups, apply row selection (filtering), and combine data frame
    tmp <- lapply(split(x, f), function(i) {
      i[.get_sample_rows(i, n = nrow_to_print), , drop = FALSE]
    })
    # create data frame w/o rownames
    x <- as.data.frame(do.call(rbind, tmp))
    # if user wants just one table, we need to preserve the group, facet and
    # panel columns, but rename those to the name of the respective focal terms
    if (isTRUE(collapse_tables)) {
      insight::check_if_installed("datawizard")
      # first focal term is main term, we don't want to touch it here
      focal_terms <- focal_terms[-1]
      # if we have ordinal models and alike, we have a response level, which
      # we need to include as first value of "focal_terms"
      if (has_response) {
        focal_terms <- c(response_name, focal_terms)
      }
      x <- datawizard::data_rename(x, sort_columns, focal_terms)
      x <- datawizard::data_relocate(x, focal_terms, after = 1)
      # we need to remove "groups", else table will be separated again
      x$groups <- NULL
      # remove repeating elements in focal term columns
      for (i in focal_terms) {
        for (j in nrow(x):2) {
          if (x[[i]][j] == x[[i]][j - 1]) x[[i]][j] <- ""
        }
      }
    }
  }

  # clean-up
  x[c("response.level", "group", "facet", "panel")] <- NULL

  # collapse CI?
  x <- .collapse_ci(x, collapse_ci, ci_brackets = dots$ci_brackets)

  rownames(x) <- NULL
  x
}


.collapse_ci <- function(x, collapse_ci, ci_brackets) {
  # collapse CI?
  ci_column <- grep("\\d{2}% CI", colnames(x))
  if (collapse_ci && length(ci_column)) {
    # make sure we don't replace with empty string
    if (!any(nzchar(ci_brackets))) {
      ci_brackets <- c("(", ")")
    }
    # remove brackets/parentheses
    x[, ci_column] <- gsub("(\\(|\\)|\\[|\\])", "", x[, ci_column])
    x[, ci_column] <- format(paste0(ci_brackets[1], trimws(x[, ci_column]), ci_brackets[2]), justify = "right")
    # paste CI to predicted values
    x[, ci_column - 1] <- paste0(x[, ci_column - 1], " ", x[, ci_column])
    # reassign column name
    colnames(x)[ci_column - 1] <- paste0(colnames(x)[ci_column - 1], " (", colnames(x)[ci_column], ")")
    # remove CI column
    x[, ci_column] <- NULL
  }
  x
}


.collapse_p <- function(x, collapse_p) {
  # collapse p?
  p_column <- which(colnames(x) == "p")
  if (collapse_p && length(p_column)) {
    # find CI column - we may insert p-values before that column. If we don't
    # have CI columns, e.g. because user also used "collapse_ci = TRUE", we
    # simply put it before estimated values
    ci_column <- grep("\\d{2}% CI", colnames(x))
    if (length(ci_column)) {
      insert_column <- ci_column - 1
    } else {
      insert_column <- p_column - 1
    }
    # make sure "< .001" is correctly handled
    x$p[x$p == "< .001"] <- "0.0001"
    # format p-values, we only want significance stars
    p_values <- format(insight::format_p(as.numeric(x$p), stars = TRUE, stars_only = TRUE))
    # paste po-values to predicted values
    x[, insert_column] <- paste0(x[, insert_column], p_values)
    # remove p column
    x[p_column] <- NULL
  }
  x
}


.nrows_to_print <- function(x, n) {
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
