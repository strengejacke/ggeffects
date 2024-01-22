#' @title Print and format ggeffects-objects
#' @name print
#'
#' @description A generic plot-method for `ggeffects`-objects.
#'
#' @param x An object of class `ggeffects`, as returned by the functions
#' from this package.
#' @param group_name Logical, if `TRUE`, the name of further focal terms are
#' used in the sub-headings of the table. If `FALSE`, only the values of the
#' focal terms are used.
#' @param digits Number of digits to print.
#' @param verbose Toggle messages.
#' @param ... Further arguments passed down to [`format.ggeffects()`], some of
#' them are also passed down further to [`insight::format_table()`] or
#' [`insight::format_value()`].
#'
#' @return A formatted data frame, printed to the console.
#'
#' @section Global Options to Customize Tables when Printing:
#' The `verbose` argument can be used to display or silence messages and
#' warnings. Furthermore, `options()` can be used to set defaults for the
#' `print()` method. The following options are available:
#'
#' - `ggeffects_ci_brackets`: Define a character vector of length two, indicating
#'   the opening and closing parentheses that encompass the confidence intervals
#'   values, e.g. `options(ggeffects_ci_brackets = c("[", "]"))`.
#'
#' @examplesIf requireNamespace("datawizard", quietly = TRUE)
#' data(efc, package = "ggeffects")
#' fit <- lm(barthtot ~ c12hour + e42dep, data = efc)
#'
#' # default print
#' ggpredict(fit, "e42dep")
#'
#' # surround CI values with parentheses
#' print(ggpredict(fit, "e42dep"), ci_brackets = c("(", ")"))
#'
#' # include value labels
#' print(ggpredict(fit, "e42dep"), value_labels = TRUE)
#'
#' # include variable labels in column headers
#' print(ggpredict(fit, "e42dep"), variable_labels = TRUE)
#'
#' # include value labels and variable labels
#' print(ggpredict(fit, "e42dep"), variable_labels = TRUE, value_labels = TRUE)
#'
#' data(iris)
#' m <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
#'
#' # default print with subgroups
#' ggpredict(m, c("Petal.Length", "Species"))
#'
#' # omit name of grouping variable in subgroup table headers
#' print(ggpredict(m, c("Petal.Length", "Species")), group_name = FALSE)
#'
#' # increase number of digits
#' print(ggpredict(fit, "e42dep"), digits = 5)
#'
#' @export
print.ggeffects <- function(x, group_name = TRUE, digits = 2, verbose = TRUE, ...) {
  lab <- attr(x, "title", exact = TRUE)
  if (!is.null(lab)) {
    insight::print_color(paste0(sprintf("# %s", lab), "\n\n", collapse = ""), "blue")
  }

  if (.is_numeric_character(x$x) || is.numeric(x$x)) {
    align <- "right"
  } else {
    align <- NULL
  }

  out <- format(
    x,
    row_header_separator = ifelse(isTRUE(group_name), "\n", ", "),
    group_name = group_name,
    digits = digits,
    ...
  )
  print_rows <- nrow(out)
  captions <- NULL

  # create strings of table captions for subgroups
  if (!is.null(out$groups)) {
    captions <- lapply(as.list(unique(out$groups)), c, "red")
    out <- lapply(split(out, out$groups), function(i) {
      i$groups <- NULL
      i
    })
  }

  cat(insight::export_table(
    out,
    title = captions,
    footer = .print_footnote(x),
    align = align
  ))
  cat("\n")

  # show msg?
  if (missing(verbose)) {
    verbose <- isTRUE(attr(x, "verbose", exact = TRUE))
  }

  predint <- attr(x, "prediction.interval", exact = TRUE)
  if (!is.null(predint) && isTRUE(predint) && isTRUE(verbose)) {
    insight::format_alert(
      "\nIntervals are prediction intervals. Use `interval = \"confidence\"` to return regular confidence intervals."
    )
  }

  # tell user about truncated output
  if (print_rows < nrow(x) && isTRUE(verbose)) {
    insight::format_alert(
      "\nNot all rows are shown in the output. Use `print(..., n = Inf)` to show all rows."
    )
  }
}


.print_footnote <- function(x, format = "text") {
  msg <- NULL
  consv <- attr(x, "constant.values")
  ci.lvl <- attr(x, "ci.lvl")

  cv <- lapply(consv, function(.x) {
    if (is.numeric(.x)) {
      sprintf("%.2f", .x)
    } else {
      as.character(.x)
    }
  })

  if (!.is_empty(cv)) {
    cv.names <- names(cv)
    cv.space <- max(nchar(cv.names))

    # ignore this string when determining maximum length
    poplev <- which(cv %in% c("NA (population-level)", "0 (population-level)"))
    if (.is_empty(poplev)) {
      mcv <- cv
    } else {
      mcv <- cv[-poplev]
    }

    if (.is_empty(mcv)) {
      cv.space2 <- 0
    } else {
      cv.space2 <- max(nchar(mcv))
    }

    if (identical(format, "text")) {
      msg <- c(paste0(
        "\nAdjusted for:\n",
        paste0(sprintf("* %*s = %*s", cv.space, cv.names, cv.space2, cv), collapse = "\n")
      ), "yellow")
    } else {
      msg <- paste0(
        "Adjusted for: ",
        toString(sprintf("%*s = %*s", cv.space, cv.names, cv.space2, cv))
      )
    }
  }
  msg
}


#' @importFrom insight print_html
#' @export
insight::print_html

#' @rdname print
#' @export
print_html.ggeffects <- function(x, group_name = TRUE, digits = 2, ...) {
  insight::export_table(
    format(x, digits = digits, group_name = group_name, ...),
    group_by = "groups",
    format = "html",
    footer = .print_footnote(x, format = "html"),
    ...
  )
}


# helper --------------------

.get_sample_rows <- function(x, n) {
  nr.of.rows <- seq_len(nrow(x))

  if (n < length(nr.of.rows)) {
    sample.rows <- round(c(
      min(nr.of.rows),
      stats::quantile(nr.of.rows, seq_len(n - 2) / n),
      max(nr.of.rows)
    ))
  } else {
    sample.rows <- nr.of.rows
  }

  sample.rows
}




## printing with tinytables

# x <- ggpredict(fit, terms = c("c12hour [5,15,50,80]", "c172code", "c161sex"))
# x_label <- attributes(x)$x.title
# predicted_label <- attributes(x)$title

# has_groups <- ggeffects:::.obj_has_name(x, "group") && length(unique(x$group)) > 1
# has_facets <- ggeffects:::.obj_has_name(x, "facet") && length(unique(x$facet)) > 1
# has_panel <- ggeffects:::.obj_has_name(x, "panel") && length(unique(x$panel)) > 1
# has_response <- ggeffects:::.obj_has_name(x, "response.level") && length(unique(x$response.level)) > 1

# sort_columns <- c("response.level", "group", "facet", "panel")[c(has_response, has_groups, has_facets, has_panel)]
# x <- datawizard::data_arrange(x, sort_columns)


# x$CI <- 0.95
# colnames(x)[colnames(x) == "conf.low"] <- "CI_low"
# colnames(x)[colnames(x) == "conf.high"] <- "CI_high"

# x$x <- insight::format_value(x$x, protect_integers = TRUE)
# x <- insight::format_table(x, zap_small = TRUE, ci_brackets = c("(", ")"))

# x$std.error <- NULL

# row_header_pos <- which(!duplicated(x[sort_columns]))
# row_header_labels <- apply(x[row_header_pos, sort_columns], 1, toString)
# row_header_labels <- as.list(stats::setNames(row_header_pos, as.vector(row_header_labels)))
# x[sort_columns] <- NULL

# colnames(x)[1] <- x_label
# colnames(x)[2] <- predicted_label

# tt(x) |>
#   group_tt(i = row_header_labels)
