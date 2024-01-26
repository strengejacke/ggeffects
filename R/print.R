#' @title Print and format ggeffects-objects
#' @name print
#'
#' @description A generic print-method for `ggeffects`-objects.
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
#' @return `format()` return a formatted data frame, `print()`prints a formatted
#' data frame printed to the console. `print_html()` returns a `tinytable`
#' object, which is printed as HTML, markdown or LaTeX table (depending on the
#' context from which `print_html()` is called, see [`tinytable::tt()`] for
#' details).
#'
#' @section Global Options to Customize Tables when Printing:
#' The `verbose` argument can be used to display or silence messages and
#' warnings. Furthermore, `options()` can be used to set defaults for the
#' `print()` and `print_html()` method. The following options are available,
#' which can simply be run in the console:
#'
#' - `ggeffects_ci_brackets`: Define a character vector of length two, indicating
#'   the opening and closing parentheses that encompass the confidence intervals
#'   values, e.g. `options(ggeffects_ci_brackets = c("[", "]"))`.
#'
#' - `ggeffects_collapse_ci`: Logical, if `TRUE`, the columns with predicted
#'   values and confidence intervals are collapsed into one column, e.g.
#'   `options(ggeffects_collapse_ci = TRUE)`.
#'
#' - `ggeffects_collapse_tables`: Logical, if `TRUE`, multiple tables for
#'   subgroups are combined into one table. Only works when there is more than
#'   one focal term, e.g. `options(ggeffects_collapse_tables = TRUE)`.
#'
#' - `ggeffects_output_format`: String, either `"text"` or `"html"`. Defines the
#'   default output format from `ggpredict()`. If `"html"`, a formatted HTML
#'   table is created and printed to the view pane. If `"text"` or `NULL`, a
#'   formatted table is printed to the console, e.g. `options(ggeffects_output_format = "html")`.
#'
#' Use `options(<option_name> = NULL)` to remove the option.
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
#' # you can also use `options(ggeffects_ci_brackets = c("[", "]"))`
#' # to set this globally
#'
#' # collapse CI columns into column with predicted values
#' print(ggpredict(fit, "e42dep"), collapse_ci = TRUE)
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
#' # collapse tables into one
#' print(ggpredict(m, c("Petal.Length", "Species")), collapse_tables = TRUE, n = 3)
#'
#' # increase number of digits
#' print(ggpredict(fit, "e42dep"), digits = 5)
#'
#' @export
print.ggeffects <- function(x, group_name = TRUE, digits = 2, verbose = TRUE, ...) {
  # check if default format is "html"
  if (identical(getOption("ggeffects_output_format", "text"), "html")) {
    return(print(print_html(x, ...)))
  }

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
  insight::check_if_installed("tinytable")

  out <- format(
    x,
    digits = digits,
    group_name = group_name,
    row_header_separator = ifelse(isTRUE(group_name), "<br/>", ", "),
    ...
  )
  caption <- attr(x, "title", exact = TRUE)

  # used for subgroup headers, if available
  row_header_pos <- row_header_labels <- NULL

  if (!is.null(out$groups)) {
    # find start row of each subgroup
    row_header_pos <- which(!duplicated(out$groups))
    # create named list, required for tinytables
    row_header_labels <- as.list(stats::setNames(row_header_pos, as.vector(out$groups[row_header_pos])))
    # since we have the group names in "row_header_labels" now , we can remove the column
    out$groups <- NULL
    # make sure that the row header positions are correct - each header
    # must be shifted by the number of rows above
    for (i in 2:length(row_header_pos)) {
      row_header_pos[i] <- row_header_pos[i] + (i - 1)
    }
  }

  # create and format footer
  footer <- .format_html_footer(.print_footnote(x, format = "html"))

  # base table
  out <- tinytable::tt(out, caption = caption, notes = footer)
  # add subheaders, if any
  if (!is.null(row_header_labels)) {
    out <- tinytable::group_tt(out, i = row_header_labels, indent = 2)
    out <- tinytable::style_tt(out, i = row_header_pos, italic = TRUE)
  }
  # workaround, to make sure HTML is default output
  m <- attr(out, "tinytable_meta")
  m$output <- "html"
  attr(out, "tinytable_meta") <- m

  out
}


.format_html_footer <- function(footer) {
  if (!is.null(footer)) {
    footer <- paste0("<div style=\"font-size: 0.9em; color: #666666\">", footer, "</div>")
  }
  footer
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
