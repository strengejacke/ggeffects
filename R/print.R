#' @export
print.ggeffects <- function(x, n = 10, digits = 2, use_labels = FALSE, verbose = TRUE, ...) {

  # remember if we have a factor
  x_is_factor <- identical(attr(x, "x.is.factor"), "1") && is.factor(x$x)

  # use value labels as values in print
  if (isTRUE(use_labels)) {
    labs <- get_x_labels(x, case = NULL)
    vals <- x$x

    if (!is.null(labs)) {
      x$x <- format(labs, justify = "right")
      labs <- format(sprintf("[%g]", vals), justify = "left")
      x$x <- paste(labs, x$x, sep = " ")
    }
  }

  # remove std.error for printint
  x$std.error <- NULL


  # do we have groups and facets?
  has_groups <- .obj_has_name(x, "group") && length(unique(x$group)) > 1
  has_facets <- .obj_has_name(x, "facet") && length(unique(x$facet)) > 1
  has_panel <- .obj_has_name(x, "panel") && length(unique(x$panel)) > 1
  has_response <- .obj_has_name(x, "response.level") && length(unique(x$response.level)) > 1
  has_se <- .obj_has_name(x, "std.error")

  lab <- attr(x, "title", exact = TRUE)
  if (!is.null(lab)) insight::print_color(paste0(sprintf("# %s", lab), "\n", collapse = ""), "blue")

  # lab <- attr(x, "x.title", exact = TRUE)
  # if (!is.null(lab)) insight::print_color(paste0(sprintf("# x = %s", lab), "\n", collapse = ""), "blue")

  consv <- attr(x, "constant.values")
  terms_arg <- attr(x, "terms")
  ci.lvl <- attr(x, "ci.lvl")

  # fix terms for survival models
  a1 <- attr(x, "fitfun", exact = TRUE)
  a2 <- attr(x, "y.title", exact = TRUE)

  if (!is.null(a1) && !is.null(a2) && a1 == "coxph" && a2 != "Risk Score" && !"time" %in% terms_arg) {
    terms_arg <- c("time", terms_arg)
  }

  # use focal term as column name
  focal_term <- terms_arg[1]
  colnames(x)[1] <- focal_term

  x <- .round_numeric(x, digits = digits)

  # justify terms
  tl <- length(terms_arg)
  if (tl > 2) {
    terms_arg[2:tl] <- format(terms_arg[2:tl], justify = "right")
  }

  # if we have groups, show n rows per group
  .n <- 1

  # we do not simply count rows, but rather the number of combinations
  # when we have facets / groups / response.levels. These are separated
  # with own heading, making the output probably too long. Thus, we
  # decide on how many rows per "subheading" to be printed also based on
  # the number of combinations of groups, facets and response level

  if (has_groups) {
    .n <-  .n_distinct(x$group)
    if (!is.null(terms_arg) && length(terms_arg) >= 2) {
      vals <- sprintf("%s = %s", terms_arg[2], as.character(x$group))
      lvls <- unique(vals)
      x$group <- factor(vals, levels = lvls)
    }
  }

  if (has_facets) {
    .n <- .n * .n_distinct(x$facet)
    if (!is.null(terms_arg) && length(terms_arg) >= 3) {
      x$facet <- sprintf("%s = %s", terms_arg[3], as.character(x$facet))
    }
  }

  if (has_panel) {
    .n <- .n * .n_distinct(x$panel)
    if (!is.null(terms_arg) && length(terms_arg) >= 4) {
      x$panel <- sprintf("%s = %s", terms_arg[4], as.character(x$panel))
    }
  }

  if (has_response) {
    .n <- .n * .n_distinct(x$response.level)
    vals <- sprintf("Response Level = %s", as.character(x$response.level))
    lvls <- unique(vals)
    x$response.level <- ordered(vals, levels = lvls)
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

  if (!has_groups) {

    if (has_response) {
      x$.nest <- tapply(x$predicted, list(x$response.level), NULL)
      xx <- split(x, x$.nest)
      for (i in xx) {
        insight::print_color(sprintf("\n# %s\n\n", i$response.level[1]), "red")
        .print_block(i, n, digits, ci.lvl, ...)
      }
    } else {
      cat("\n")
      if (.obj_has_name(x, "group")) x <- .remove_column(x, "group")
      .print_block(x, n, digits, ci.lvl, ...)
    }

  } else if (has_groups && !has_facets) {

    if (has_response) {
      x$.nest <- tapply(x$predicted, list(x$response.level, x$group), NULL)
      xx <- split(x, x$.nest)
      for (i in xx) {
        insight::print_color(sprintf("\n# %s\n# %s\n\n", i$response.level[1], i$group[1]), "red")
        .print_block(i, n, digits, ci.lvl, ...)
      }
    } else {
      x$.nest <- tapply(x$predicted, list(x$group), NULL)
      xx <- split(x, x$.nest)
      for (i in xx) {
        insight::print_color(sprintf("\n# %s\n\n", i$group[1]), "red")
        .print_block(i, n, digits, ci.lvl, ...)
      }
    }

  } else if (has_groups && has_facets && !has_panel) {

    if (has_response) {
      x$.nest <- tapply(x$predicted, list(x$response.level, x$group, x$facet), NULL)
      xx <- split(x, x$.nest)
      for (i in xx) {
        insight::print_color(sprintf("\n# %s\n# %s\n# %s\n\n", i$response.level[1], i$group[1], i$facet[1]), "red")
        .print_block(i, n, digits, ci.lvl, ...)
      }
    } else {
      x$.nest <- tapply(x$predicted, list(x$group, x$facet), NULL)
      xx <- split(x, x$.nest)
      for (i in xx) {
        insight::print_color(sprintf("\n# %s\n# %s\n\n", i$group[1], i$facet[1]), "red")
        .print_block(i, n, digits, ci.lvl, ...)
      }
    }

  } else if (has_response) {
    x$.nest <- tapply(x$predicted, list(x$response.level, x$group, x$facet, x$panel), NULL)
    xx <- split(x, x$.nest)
    for (i in xx) {
      insight::print_color(sprintf(
        "\n# %s\n# %s\n# %s\n# %s\n\n",
        i$response.level[1],
        i$group[1],
        i$facet[1],
        i$panel[1]
      ), "red")
      .print_block(i, n, digits, ci.lvl, ...)
    }
  } else {
    x$.nest <- tapply(x$predicted, list(x$group, x$facet, x$panel), NULL)
    xx <- split(x, x$.nest)
    for (i in xx) {
      insight::print_color(sprintf("\n# %s\n# %s\n# %s\n\n", i$group[1], i$facet[1], i$panel[1]), "red")
      .print_block(i, n, digits, ci.lvl, ...)
    }
  }

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

    insight::print_color(paste0(
      "\nAdjusted for:\n",
      paste0(sprintf("* %*s = %*s", cv.space, cv.names, cv.space2, cv), collapse = "\n")
    ), "blue")

    cat("\n")
  }

  # show msg?
  if (missing(verbose)) {
    verbose <- isTRUE(attr(x, "verbose", exact = TRUE))
  }

  fitfun <- attr(x, "fitfun", exact = TRUE)
  if (has_se && !is.null(fitfun) && fitfun != "lm" && isTRUE(verbose)) {
    insight::format_alert("\nStandard errors are on the link-scale (untransformed).")
  }

  predint <- attr(x, "prediction.interval", exact = TRUE)
  if (!is.null(predint) && isTRUE(predint) && isTRUE(verbose)) {
    insight::format_alert(
      "\nIntervals are prediction intervals. Use `interval = \"confidence\"` to return regular confidence intervals."
    )
  }

  # tell user about truncated output
  if ((.n * n) < nrow(x) && isTRUE(verbose)) {
    insight::format_alert(
      "\nNot all rows are shown in the output. Use `print(..., n = Inf)` to show all rows."
    )
  }
}


#' @importFrom insight print_html
#' @export
insight::print_html

#' @export
print_html.ggeffects <- function(x, ...) {
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
  insight::export_table(x, group_by = "groups", format = "html", align = "llcc", ...)
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


.print_block <- function(i, n, digits, ci.lvl, ...) {
  i <- as.data.frame(i)
  i <- i[setdiff(colnames(i), c("group", "facet", "panel", "response.level", ".nest"))]
  dd <- i[.get_sample_rows(i, n), , drop = FALSE]

  if ("conf.low" %in% colnames(dd) && "conf.high" %in% colnames(dd)) {
    dd$CI <- insight::format_ci(dd$conf.low, dd$conf.high, digits = digits, width = "auto")
    dd$CI <- gsub("95% CI ", "", dd$CI, fixed = TRUE)

    if (is.null(ci.lvl)) {
      ci.lvl <- 0.95
    }
    colnames(dd)[which(colnames(dd) == "CI")] <- sprintf("%g%% CI", 100 * ci.lvl)

    dd$conf.low <- NULL
    dd$conf.high <- NULL
  }

  if ("std.error" %in% colnames(dd)) {
    colnames(dd)[which(colnames(dd) == "std.error")] <- "SE"
  }

  colnames(dd)[which(colnames(dd) == "predicted")] <- "Predicted"
  cat(insight::export_table(dd, digits = digits, protect_integers = TRUE))
}
