


#' @export
format.ggeffects <- function(x, n = 10, digits = 2, x.lab = FALSE, format = NULL, ci_width = "auto", ci_brackets = TRUE, ...) {
  # convert to factor
  if (isTRUE(x.lab)) {
    labs <- sjlabelled::get_labels(
      x$x,
      attr.only = TRUE,
      values = "n",
      non.labelled = FALSE,
      drop.na = TRUE
    )

    vals <- x$x
    x$x <- format(sjlabelled::as_label(x$x), justify = "right")

    if (!is.null(labs) && !is.null(names(labs))) {
      labs <- labs[match(vals, names(labs))]
      labs <- format(sprintf("[%s]", names(labs)), justify = "left")
      x$x <- paste(labs, x$x, sep = " ")
    }
  }


  # remove std.error for print
  x$std.error <- NULL


  # do we have groups and facets?
  has_groups <- .obj_has_name(x, "group") && length(unique(x$group)) > 1
  has_facets <- .obj_has_name(x, "facet") && length(unique(x$facet)) > 1
  has_panel <- .obj_has_name(x, "panel") && length(unique(x$panel)) > 1
  has_response <- .obj_has_name(x, "response.level") && length(unique(x$response.level)) > 1
  has_se <- .obj_has_name(x, "std.error")

  table_caption <- NULL

  if (is.null(format) || format == "text") {
    lab <- attr(x, "title", exact = TRUE)
    if (!is.null(lab)) table_caption <- sprintf("# %s", lab)
    lab <- attr(x, "x.title", exact = TRUE)
    if (!is.null(lab)) table_caption <- paste0(table_caption, "\n", sprintf("# x = %s", lab))
    table_caption <- c(table_caption, "blue")
  } else {
    lab <- attr(x, "title", exact = TRUE)
    if (!is.null(lab)) table_caption <- sprintf("%s", lab)
    lab <- attr(x, "x.title", exact = TRUE)
    if (!is.null(lab)) table_caption <- sprintf("x = %s", lab)
  }

  consv <- attr(x, "constant.values")
  terms <- attr(x, "terms")
  ci.lvl <- attr(x, "ci.lvl")

  # fix terms for survival models
  a1 <- attr(x, "fitfun", exact = TRUE)
  a2 <- attr(x, "y.title", exact = TRUE)

  if (!is.null(a1) && !is.null(a2) && a1 == "coxph" && !(a2 == "Risk Score"))
    terms <- c("time", terms)

  x <- .round_numeric(x, digits = digits)

  # if we have groups, show n rows per group

  .n <- 1

  # justify terms

  tl <- length(terms)
  if (tl > 2) terms[2:tl] <- format(terms[2:tl], justify = "right")

  if (has_groups) {
    .n <-  .n_distinct(x$group)
    if (!is.null(terms) && length(terms) >= 2) {
      vals <- sprintf("%s = %s", terms[2], as.character(x$group))
      lvls <- unique(vals)
      x$group <- factor(vals, levels = lvls)
    }
  }

  if (has_facets) {
    .n <- .n * .n_distinct(x$facet)
    if (!is.null(terms) && length(terms) >= 3) {
      x$facet <- sprintf("%s = %s", terms[3], as.character(x$facet))
    }
  }

  if (has_panel) {
    .n <- .n * .n_distinct(x$panel)
    if (!is.null(terms) && length(terms) >= 4) {
      x$panel <- sprintf("%s = %s", terms[4], as.character(x$panel))
    }
  }

  if (has_response) {
    .n <- .n * .n_distinct(x$response.level)
    vals <- sprintf("Response Level = %s", as.character(x$response.level))
    lvls <- unique(vals)
    x$response.level <- ordered(vals, levels = lvls)
  }

  # make sure that by default not too many rows are printed
  if (missing(n)) {
    n <- if (.n >= 6)
      4
    else if (.n >= 4 & .n < 6)
      5
    else if (.n >= 2 & .n < 4)
      6
    else
      8
  }

  formatted_text <- c()
  final_table <- list()

  if (!has_groups) {

    if (!has_response) {
      if (.obj_has_name(x, "group")) x <- .remove_column(x, "group")
      formatted_text <- .format_block(x, n, digits, ci.lvl, ...)
      attr(formatted_text, "table_caption") <- table_caption
      final_table <- list(formatted_text)
    } else {
      x$.nest <- tapply(x$predicted, list(x$response.level), NULL)
      xx <- split(x, x$.nest)
      for (i in xx) {
        if (is.null(format) || format == "text") {
          table_caption <- c(sprintf("# %s", i$response.level[1]), "red")
        } else {
          table_caption <- sprintf("%s", i$response.level[1])
        }
        formatted_text <- .format_block(i, n, digits, ci.lvl, ...)
        attr(formatted_text, "table_caption") <- table_caption
        final_table <- c(final_table, list(formatted_text))
      }
    }

  } else if (has_groups && !has_facets) {

    if (!has_response) {
      x$.nest <- tapply(x$predicted, list(x$group), NULL)
      xx <- split(x, x$.nest)

      for (i in xx) {
        insight::print_color(sprintf("\n# %s\n\n", i$group[1]), "red")
        .format_block(i, n, digits, ci.lvl, ...)
      }
    } else {
      x$.nest <- tapply(x$predicted, list(x$response.level, x$group), NULL)
      xx <- split(x, x$.nest)

      for (i in xx) {
        insight::print_color(sprintf("\n# %s\n# %s\n\n", i$response.level[1], i$group[1]), "red")
        .format_block(i, n, digits, ci.lvl, ...)
      }
    }

  } else if (has_groups && has_facets && !has_panel) {

    if (!has_response) {
      x$.nest <- tapply(x$predicted, list(x$group, x$facet), NULL)
      xx <- split(x, x$.nest)

      for (i in xx) {
        insight::print_color(sprintf("\n# %s\n# %s\n\n", i$group[1], i$facet[1]), "red")
        .format_block(i, n, digits, ci.lvl, ...)
      }
    } else {
      x$.nest <- tapply(x$predicted, list(x$response.level, x$group, x$facet), NULL)
      xx <- split(x, x$.nest)

      for (i in xx) {
        insight::print_color(sprintf("\n# %s\n# %s\n# %s\n\n", i$response.level[1], i$group[1], i$facet[1]), "red")
        .format_block(i, n, digits, ci.lvl, ...)
      }
    }

  } else {

    if (!has_response) {
      x$.nest <- tapply(x$predicted, list(x$group, x$facet, x$panel), NULL)
      xx <- split(x, x$.nest)

      for (i in xx) {
        insight::print_color(sprintf("\n# %s\n# %s\n# %s\n\n", i$group[1], i$facet[1], i$panel[1]), "red")
        .format_block(i, n, digits, ci.lvl, ...)
      }
    } else {
      x$.nest <- tapply(x$predicted, list(x$response.level, x$group, x$facet, x$panel), NULL)
      xx <- split(x, x$.nest)

      for (i in xx) {
        insight::print_color(sprintf("\n# %s\n# %s\n# %s\n# %s\n\n", i$response.level[1], i$group[1], i$facet[1], i$panel[1]), "red")
        .format_block(i, n, digits, ci.lvl, ...)
      }
    }
  }

  cv <- lapply(
    consv,
    function(.x) {
      if (is.numeric(.x))
        sprintf("%.2f", .x)
      else
        as.character(.x)
    })

  if (!.is_empty(cv)) {
    cv.names <- names(cv)
    cv.space <- max(nchar(cv.names))

    # ignore this string when determining maximum length
    poplev <- which(cv %in% c("NA (population-level)", "0 (population-level)"))
    if (!.is_empty(poplev))
      mcv <- cv[-poplev]
    else
      mcv <- cv

    if (!.is_empty(mcv))
      cv.space2 <- max(nchar(mcv))
    else
      cv.space2 <- 0

    insight::print_color(paste0(
      "\nAdjusted for:\n",
      paste0(sprintf("* %*s = %*s", cv.space, cv.names, cv.space2, cv), collapse = "\n")
    ), "blue")

    cat("\n")
  }


  fitfun <- attr(x, "fitfun", exact = TRUE)
  if (has_se && !is.null(fitfun) && fitfun != "lm") {
    message("\nStandard errors are on the link-scale (untransformed).")
  }

  predint <- attr(x, "prediction.interval", exact = TRUE)
  if (!is.null(predint) && isTRUE(predint)) {
    message("\nIntervals are prediction intervals.")
  }
}



#' @importFrom insight format_ci
.format_block <- function(i, n, digits, ci.lvl, ci_width = "auto", ci_brackets = TRUE, ...) {
  i <- i[setdiff(colnames(i), c("group", "facet", "panel", "response.level", ".nest"))]
  # print.data.frame(, ..., row.names = FALSE, quote = FALSE)
  dd <- i[.get_sample_rows(i, n), , drop = FALSE]

  if ("conf.low" %in% colnames(dd) && "conf.high" %in% colnames(dd)) {
    dd$CI <- insight::format_ci(dd$conf.low, dd$conf.high, digits = digits, width = ci_width, brackets = ci_brackets)
    dd$CI <- gsub("95% CI ", "", dd$CI, fixed = TRUE)

    if (is.null(ci.lvl)) ci.lvl <- .95
    colnames(dd)[which(colnames(dd) == "CI")] <- sprintf("%g%% CI", 100 * ci.lvl)

    dd$conf.low <- NULL
    dd$conf.high <- NULL
  }

  if ("std.error" %in% colnames(dd)) {
    colnames(dd)[which(colnames(dd) == "std.error")] <- "SE"
  }

  colnames(dd)[which(colnames(dd) == "predicted")] <- "Predicted"
  dd
}
