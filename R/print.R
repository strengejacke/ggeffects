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
#' @param theme The theme to apply to the table. One of `"grid"`, `"striped"`,
#' `"bootstrap"`, or `"darklines"`.
#' @param engine The engine to use for printing. One of `"tt"` (default) or `"gt"`.
#' `"tt"` uses the *tinytable* package, `"gt"` uses the *gt* package.
#' @param ... Further arguments passed down to [`format.ggeffects()`], some of
#' them are also passed down further to [`insight::format_table()`] or
#' [`insight::format_value()`].
#'
#' @return `format()` return a formatted data frame, `print()` prints a formatted
#' data frame printed to the console. `print_html()` returns a `tinytable`
#' object by default (unless changed with `engine = "gt"`), which is printed as
#' HTML, markdown or LaTeX table (depending on the context from which
#' `print_html()` is called, see [`tinytable::tt()`] for details).
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
#'   values (or contrasts) and confidence intervals are collapsed into one
#'   column, e.g. `options(ggeffects_collapse_ci = TRUE)`.
#'
#' - `ggeffects_collapse_p`: Logical, if `TRUE`, the columns with predicted
#'   values (or contrasts) and p-values are collapsed into one column, e.g.
#'   `options(ggeffects_collapse_p = TRUE)`. Note that p-values are replaced
#'   by asterisk-symbols (stars) or empty strings when `ggeffects_collapse_p = TRUE`,
#'   depending on the significance level.
#'
#' - `ggeffects_collapse_tables`: Logical, if `TRUE`, multiple tables for
#'   subgroups are combined into one table. Only works when there is more than
#'   one focal term, e.g. `options(ggeffects_collapse_tables = TRUE)`.
#'
#' - `ggeffects_output_format`: String, either `"text"`, `"markdown"` or `"html"`.
#'   Defines the default output format from `predict_response()`. If `"html"`, a
#'   formatted HTML table is created and printed to the view pane. `"markdown"`
#'   creates a markdown-formatted table inside Rmarkdown documents, and prints
#'   a text-format table to the console when used interactively. If `"text"` or
#'   `NULL`, a formatted table is printed to the console, e.g.
#'   `options(ggeffects_output_format = "html")`.
#'
#' - `ggeffects_html_engine`: String, either `"tt"` or `"gt"`. Defines the default
#'   engine to use for printing HTML tables. If `"tt"`, the *tinytable* package
#'   is used, if `"gt"`, the *gt* package is used, e.g.
#'   `options(ggeffects_html_engine = "gt")`.
#'
#' Use `options(<option_name> = NULL)` to remove the option.
#'
#' @examplesIf requireNamespace("datawizard", quietly = TRUE)
#' data(efc, package = "ggeffects")
#' fit <- lm(barthtot ~ c12hour + e42dep, data = efc)
#'
#' # default print
#' predict_response(fit, "e42dep")
#'
#' # surround CI values with parentheses
#' print(predict_response(fit, "e42dep"), ci_brackets = c("(", ")"))
#' # you can also use `options(ggeffects_ci_brackets = c("[", "]"))`
#' # to set this globally
#'
#' # collapse CI columns into column with predicted values
#' print(predict_response(fit, "e42dep"), collapse_ci = TRUE)
#'
#' # include value labels
#' print(predict_response(fit, "e42dep"), value_labels = TRUE)
#'
#' # include variable labels in column headers
#' print(predict_response(fit, "e42dep"), variable_labels = TRUE)
#'
#' # include value labels and variable labels
#' print(predict_response(fit, "e42dep"), variable_labels = TRUE, value_labels = TRUE)
#'
#' data(iris)
#' m <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
#'
#' # default print with subgroups
#' predict_response(m, c("Petal.Length", "Species"))
#'
#' # omit name of grouping variable in subgroup table headers
#' print(predict_response(m, c("Petal.Length", "Species")), group_name = FALSE)
#'
#' # collapse tables into one
#' print(predict_response(m, c("Petal.Length", "Species")), collapse_tables = TRUE, n = 3)
#'
#' # increase number of digits
#' print(predict_response(fit, "e42dep"), digits = 5)
#'
#' @export
print.ggeffects <- function(x, group_name = TRUE, digits = 2, verbose = TRUE, ...) {
  # check if default format is "html" or "markdown"
  output_format <- getOption("ggeffects_output_format", "text")
  if (identical(output_format, "html")) {
    return(print(print_html(x, group_name = group_name, digits = digits, ...)))
  }
  if (identical(output_format, "markdown")) {
    return(print(print_md(x, group_name = group_name, digits = digits, ...)))
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
    # make "groups" a factor, for split and correct order
    out$groups <- factor(out$groups, levels = unique(out$groups))
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
  ci_level <- attr(x, "ci_level")

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

#' @importFrom insight print_md
#' @export
insight::print_md


#' @rdname print
#' @export
print_md.ggeffects <- function(x, group_name = TRUE, digits = 2, ...) {
  .print_html_tt(x, group_name = group_name, digits = digits, theme = NULL, output = "markdown", ...)
}


#' @rdname print
#' @export
print_html.ggeffects <- function(x,
                                 group_name = TRUE,
                                 digits = 2,
                                 theme = NULL,
                                 engine = c("tt", "gt"),
                                 ...) {
  engine <- getOption("ggeffects_html_engine", engine)
  engine <- match.arg(engine)

  if (engine == "tt") {
    .print_html_tt(x, group_name = group_name, digits = digits, theme = theme, ...)
  } else {
    .print_html_gt(x, group_name = group_name, digits = digits, ...)
  }
}


# print using tiny table
.print_html_tt <- function(x, group_name = TRUE, digits = 2, theme = NULL, output = "html", ...) {
  insight::check_if_installed("tinytable", minimum_version = "0.1.0")

  out <- format(
    x,
    digits = digits,
    group_name = group_name,
    row_header_separator = ifelse(isTRUE(group_name) && identical(output, "html"), "<br/>", ", "),
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
  footer <- .print_footnote(x, format = output)
  if (identical(output, "html")) {
    footer <- .format_html_footer(footer)
  }

  # base table
  out <- tinytable::tt(out, caption = caption, notes = footer)
  # add subheaders, if any
  if (!is.null(row_header_labels)) {
    out <- tinytable::group_tt(out, i = row_header_labels, indent = 2)
    out <- tinytable::style_tt(out, i = row_header_pos, italic = TRUE)
  }
  # apply theme, if any
  if (identical(output, "html")) {
    out <- insight::apply_table_theme(out, x, theme = theme, sub_header_positions = row_header_pos)
  }
  # workaround, to make sure HTML is default output
  out@output <- output
  out
}

# print using gt
.print_html_gt <- function(x, group_name = TRUE, digits = 2, theme = NULL, ...) {
  out <- format(
    x,
    digits = digits,
    group_name = group_name,
    row_header_separator = "; ",
    ...
  )
  caption <- attr(x, "title", exact = TRUE)
  footer <- .format_html_footer(.print_footnote(x, format = "html"))
  insight::export_table(
    out,
    format = "html",
    group_by = "groups",
    footer = footer,
    caption = caption
  )
}



# helper --------------------

.format_html_footer <- function(footer) {
  if (!is.null(footer)) {
    footer <- paste0("<div style=\"font-size: 0.9em; color: #666666\">", footer, "</div>")
  }
  footer
}

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


# methods for ggcomparisons --------------------------------------------


#' @rdname print
#' @export
print.ggcomparisons <- function(x, collapse_tables = FALSE, ...) {
  # check if default format is "html" or "markdown"
  output_format <- getOption("ggeffects_output_format", "text")
  if (identical(output_format, "html")) {
    return(print(print_html(x, ...)))
  }
  if (identical(output_format, "markdown")) {
    return(print(print_md(x, ...)))
  }

  test_pairwise <- identical(attributes(x)$test, "pairwise")
  test_consecutive <- identical(attributes(x)$test, "consecutive")
  test_interaction <- identical(attributes(x)$test, "interaction")
  test_custom <- identical(attributes(x)$test, "custom")
  estimate_name <- attributes(x)$estimate_name
  by_factor <- attributes(x)$by_factor
  rope_range <- attributes(x)$rope_range
  msg_intervals <- isTRUE(attributes(x)$msg_intervals)
  verbose <- isTRUE(attributes(x)$verbose)
  scale_outcome <- attributes(x)$scale
  scale_label <- attributes(x)$scale_label
  is_linear <- isTRUE(attributes(x)$linear_model)

  # if we want to collapse table, we don't need "by" variable
  if (isTRUE(collapse_tables)) {
    by_factor <- NULL
  }

  # get header and footer, then print table
  x <- format(x, ...)
  slopes <- vapply(x, function(i) all(i == "slope"), TRUE)
  if (!is.null(rope_range)) {
    caption <- c("# TOST-test for Practical Equivalence", "blue")
  } else if (any(slopes)) {
    x[slopes] <- NULL
    caption <- c(paste0("# (Average) Linear trend for ", names(slopes)[slopes]), "blue")
  } else if (test_pairwise) {
    caption <- c("# Pairwise comparisons", "blue")
  } else if (test_consecutive) {
    caption <- c("# Consecutive contrasts", "blue")
  } else if (test_interaction) {
    caption <- c("# Interaction contrasts", "blue")
  } else if (test_custom) {
    caption <- c("# Custom contrasts", "blue")
  } else {
    caption <- NULL
  }
  footer <- attributes(x)$hypothesis_label
  if (!is.null(footer)) {
    footer <- insight::format_message(paste0("Tested hypothesis: ", footer))
    footer <- paste0("\n", footer, "\n")
  }

  # split tables by response levels?
  if ("Response_Level" %in% colnames(x)) {
    x$Response_Level <- factor(x$Response_Level, levels = unique(x$Response_Level))
    out <- split(x, x$Response_Level)
    for (response_table in seq_along(out)) {
      insight::print_color(paste0("\n# Response Level: ", names(out)[response_table], "\n\n"), "red")
      tab <- out[[response_table]]
      tab$Response_Level <- NULL
      if (response_table == 1) {
        cat(insight::export_table(tab, title = caption, footer = NULL, ...))
      } else if (response_table == length(out)) {
        cat(insight::export_table(tab, title = NULL, footer = footer, ...))
      } else {
        cat(insight::export_table(tab, title = NULL, footer = NULL, ...))
      }
    }
    # check if we have at least three rows by column, else splitting by "by"
    # is not useful
  } else if (!is.null(by_factor) && all(by_factor %in% colnames(x)) && (prod(lengths(lapply(x[by_factor], unique))) * 3) <= nrow(x)) { # nolint
    # split tables by "by" variable? Need a different handling for captions here
    out <- split(x, x[by_factor])
    if (!is.null(caption)) {
      insight::print_color(caption[1], caption[2])
      cat("\n")
    }
    for (by_table in seq_along(out)) {
      insight::print_color(paste0("\n", paste0(by_factor, collapse = "/"), " = ", names(out)[by_table], "\n\n"), "red")
      tab <- out[[by_table]]
      tab[by_factor] <- NULL
      if (by_table == length(out)) {
        cat(insight::export_table(tab, title = NULL, footer = footer, ...))
      } else {
        cat(insight::export_table(tab, title = NULL, footer = NULL, ...))
      }
    }
  } else {
    cat(insight::export_table(x, title = caption, footer = footer, ...))
  }

  # what type of estimates do we have?
  type <- switch(estimate_name,
    Predicted = "Predictions",
    Contrast = "Contrasts",
    Slope = "Slopes",
    "Estimates"
  )

  # tell user about scale of estimate type
  if (verbose && !(is_linear && identical(scale_outcome, "response"))) {
    if (is.null(scale_label)) {
      scale_label <- switch(scale_outcome,
        response = "response",
        probs = ,
        probability = if (type == "Contrasts") {
          "probability (in %-points)"
        } else {
          "probability"
        },
        exp = "exponentiated",
        log = "log",
        link = "link",
        oddsratios = "odds ratio",
        irr = "incident rate ratio",
        count = ,
        conditional = "conditional means",
        "unknown"
      )
      msg <- paste0("\n", type, " are presented on the ", scale_label, " scale.")
    } else {
      # for proportions and probabilities, contrasts are differences in %-points
      if (type == "Contrasts" && scale_label %in% c("probabilities", "proportions")) {
        scale_label <- paste(scale_label, "(in %-points)")
      }
      msg <- paste0("\n", type, " are presented as ", scale_label, ".")
    }
    insight::format_alert(msg)
  }

  # tell user about possible discrepancies between prediction intervals of
  # predictions and confidence intervals of contrasts/comparisons
  if (msg_intervals && verbose) {
    insight::format_alert(paste(
      "\nIntervals used for contrasts and comparisons are regular confidence intervals, not prediction intervals.",
      "To obtain the same type of intervals for your predictions, use `predict_response(..., interval = \"confidence\")`." # nolint
    ))
  }
}


#' @rdname print
#' @export
print_html.ggcomparisons <- function(x,
                                     collapse_ci = FALSE,
                                     collapse_p = FALSE,
                                     theme = NULL,
                                     engine = c("tt", "gt"),
                                     ...) {
  engine <- getOption("ggeffects_html_engine", engine)
  engine <- match.arg(engine)
  .print_html_ggcomparisons(
    x,
    collapse_ci = collapse_ci,
    collapse_p = collapse_p,
    theme = theme,
    engine = engine,
    ...
  )
}


#' @rdname print
#' @export
print_md.ggcomparisons <- function(x, collapse_ci = FALSE, collapse_p = FALSE, theme = NULL, ...) {
  .print_html_ggcomparisons(
    x,
    collapse_ci = collapse_ci,
    collapse_p = collapse_p,
    theme = theme,
    engine = "tt",
    output = "markdown",
    ...
  )
}


.print_html_ggcomparisons <- function(x,
                                      collapse_ci = FALSE,
                                      collapse_p = FALSE,
                                      theme = NULL,
                                      engine = c("tt", "gt"),
                                      output = "html",
                                      ...) {
  test_pairwise <- identical(attributes(x)$test, "pairwise")
  test_interaction <- identical(attributes(x)$test, "interaction")
  test_consecutive <- identical(attributes(x)$test, "consecutive")
  test_custom <- identical(attributes(x)$test, "custom")
  estimate_name <- attributes(x)$estimate_name
  rope_range <- attributes(x)$rope_range
  msg_intervals <- isTRUE(attributes(x)$msg_intervals)
  verbose <- isTRUE(attributes(x)$verbose)
  by_factor <- attributes(x)$by_factor
  scale_outcome <- attributes(x)$scale
  scale_label <- attributes(x)$scale_label
  is_linear <- isTRUE(attributes(x)$linear_model)

  # get header and footer, then print table
  x <- format(x, collapse_ci = collapse_ci, collapse_p = collapse_p, ...)
  slopes <- vapply(x, function(i) all(i == "slope"), TRUE)
  if (!is.null(rope_range)) {
    caption <- "TOST-test for Practical Equivalence"
  } else if (any(slopes)) {
    x[slopes] <- NULL
    caption <- paste0("(Average) Linear trend for ", names(slopes)[slopes])
  } else if (test_pairwise) {
    caption <- "Pairwise comparisons"
  } else if (test_interaction) {
    caption <- "Interaction contrasts"
  } else if (test_consecutive) {
    caption <- "Consecutive contrasts"
  } else if (test_custom) {
    caption <- "Custom contrasts"
  } else {
    caption <- NULL
  }

  footer <- attributes(x)$hypothesis_label
  if (!is.null(footer)) {
    footer <- paste0("Tested hypothesis: ", footer)
  }

  if (verbose) {
    # what type of estimates do we have?
    type <- switch(estimate_name,
      Predicted = "Predictions",
      Contrast = "Contrasts",
      Slope = "Slopes",
      "Estimates"
    )

    # line separator
    line_sep <- ifelse(identical(output, "html"), "<br/>", ", ")

    # tell user about scale of estimate type
    if (!(is_linear && identical(scale_outcome, "response"))) {
      if (is.null(scale_label)) {
        scale_label <- switch(scale_outcome,
          response = "response",
          probs = ,
          probability = "probability",
          exp = "exponentiated",
          log = "log",
          link = "link",
          oddsratios = "odds ratio",
          irr = "incident rate ratio",
          "unknown"
        )
        footer <- paste0(
          footer,
          ifelse(is.null(footer), "", line_sep),
          type,
          " are presented on the ",
          scale_label,
          " scale."
        )
      } else {
        footer <- paste0(
          footer,
          ifelse(is.null(footer), "", line_sep),
          type,
          " are presented as ",
          scale_label,
          "."
        )
      }
    }
  }

  # format footer, make it a bit smaller
  if (identical(output, "html")) {
    footer <- .format_html_footer(footer)
  }

  # split by "by"? But only, if we have enough rows for each group
  # else, inserting table headings for each row is not useful
  split_by <- !is.null(by_factor) &&
    all(by_factor %in% colnames(x)) &&
    (prod(lengths(lapply(x[by_factor], unique))) * 3) <= nrow(x)

  # start here for using tinytables
  if (engine == "tt") {
    insight::check_if_installed("tinytable", minimum_version = "0.1.0")
    # used for subgroup headers, if available
    row_header_pos <- row_header_labels <- NULL

    # do we have groups?
    if (split_by) { # nolint
      # if we have more than one group variable, we unite them into one
      if (length(by_factor) > 1) {
        group_by <- datawizard::data_unite(x, "group_by", by_factor, separator = ", ")$group_by
      } else {
        group_by <- x[[by_factor]]
      }
      x[by_factor] <- NULL
    } else if ("Response_Level" %in% colnames(x)) {
      group_by <- x$Response_Level
      x$Response_Level <- NULL
    } else {
      group_by <- NULL
    }

    # split tables by response levels?
    if (!is.null(group_by)) {
      # make sure group_by is ordered
      if (is.unsorted(group_by)) {
        new_row_order <- order(group_by)
        # re-order group_by and data frame
        group_by <- group_by[new_row_order]
        x <- x[new_row_order, ]
      }
      # find start row of each subgroup
      row_header_pos <- which(!duplicated(group_by))
      # create named list, required for tinytables
      row_header_labels <- as.list(stats::setNames(row_header_pos, as.vector(group_by[row_header_pos])))
      # make sure that the row header positions are correct - each header
      # must be shifted by the number of rows above
      for (i in 2:length(row_header_pos)) {
        row_header_pos[i] <- row_header_pos[i] + (i - 1)
      }
    }

    # base table
    out <- tinytable::tt(x, caption = caption, notes = footer)
    # add subheaders, if any
    if (!is.null(row_header_labels)) {
      out <- tinytable::group_tt(out, i = row_header_labels, indent = 2)
      out <- tinytable::style_tt(out, i = row_header_pos, italic = TRUE)
    }
    # apply theme, if any
    if (identical(output, "html")) {
      out <- insight::apply_table_theme(out, x, theme = theme, sub_header_positions = row_header_pos)
    }
    # workaround, to make sure HTML is default output
    out@output <- output
    out
  } else {
    # here we go with gt
    if ("Response_Level" %in% colnames(x)) {
      group_by <- c("Response_Level", "groups")
    } else if (split_by) {
      groups <- c(by_factor, "groups")
    } else {
      group_by <- "groups"
    }
    insight::export_table(
      x,
      format = "html",
      group_by = "groups",
      footer = footer,
      caption = caption
    )
  }
}
