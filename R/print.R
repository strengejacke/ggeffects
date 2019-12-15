#' @importFrom purrr flatten_df
#' @importFrom sjmisc round_num add_variables seq_row is_num_fac
#' @importFrom stats quantile
#' @importFrom sjlabelled as_label get_labels
#' @export
print.ggeffects <- function(x, n = 10, digits = 2, x.lab = FALSE, ...) {

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


  # do we have groups and facets?
  has_groups <- .obj_has_name(x, "group") && length(unique(x$group)) > 1
  has_facets <- .obj_has_name(x, "facet") && length(unique(x$facet)) > 1
  has_panel <- .obj_has_name(x, "panel") && length(unique(x$panel)) > 1
  has_response <- .obj_has_name(x, "response.level") && length(unique(x$response.level)) > 1
  has_se <- .obj_has_name(x, "std.error")

  cat("\n")

  lab <- attr(x, "title", exact = TRUE)
  if (!is.null(lab)) insight::print_color(paste0(sprintf("# %s", lab), "\n", collapse = ""), "blue")

  lab <- attr(x, "x.title", exact = TRUE)
  if (!is.null(lab)) insight::print_color(paste0(sprintf("# x = %s", lab), "\n", collapse = ""), "blue")

  consv <- attr(x, "constant.values")
  terms <- attr(x, "terms")
  ci.lvl <- attr(x, "ci.lvl")

  # fix terms for survival models
  a1 <- attr(x, "fitfun", exact = TRUE)
  a2 <- attr(x, "y.title", exact = TRUE)

  if (!is.null(a1) && !is.null(a2) && a1 == "coxph" && !(a2 == "Risk Score"))
    terms <- c("time", terms)

  x <- sjmisc::round_num(x, digits = digits)

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

  if (!has_groups) {

    if (!has_response) {
      cat("\n")
      if (.obj_has_name(x, "group")) x <- .remove_column(x, "group")
      # print.data.frame(x[.get_sample_rows(x, n), ], ..., row.names = FALSE, quote = FALSE)
      .print_block(x, n, digits, ci.lvl, ...)
    } else {
      x$.nest <- tapply(x$predicted, list(x$response.level), NULL)
      xx <- split(x, x$.nest)

      for (i in xx) {
        insight::print_color(sprintf("\n# %s\n\n", i$response.level[1]), "red")
        .print_block(i, n, digits, ci.lvl, ...)
      }
    }

  } else if (has_groups && !has_facets) {

    if (!has_response) {
      x$.nest <- tapply(x$predicted, list(x$group), NULL)
      xx <- split(x, x$.nest)

      for (i in xx) {
        insight::print_color(sprintf("\n# %s\n\n", i$group[1]), "red")
        .print_block(i, n, digits, ci.lvl, ...)
      }
    } else {
      x$.nest <- tapply(x$predicted, list(x$response.level, x$group), NULL)
      xx <- split(x, x$.nest)

      for (i in xx) {
        insight::print_color(sprintf("\n# %s\n# %s\n\n", i$response.level[1], i$group[1]), "red")
        .print_block(i, n, digits, ci.lvl, ...)
      }
    }

  } else if (has_groups && has_facets && !has_panel) {

    if (!has_response) {
      x$.nest <- tapply(x$predicted, list(x$group, x$facet), NULL)
      xx <- split(x, x$.nest)

      for (i in xx) {
        insight::print_color(sprintf("\n# %s\n# %s\n\n", i$group[1], i$facet[1]), "red")
        .print_block(i, n, digits, ci.lvl, ...)
      }
    } else {
      x$.nest <- tapply(x$predicted, list(x$response.level, x$group, x$facet), NULL)
      xx <- split(x, x$.nest)

      for (i in xx) {
        insight::print_color(sprintf("\n# %s\n# %s\n# %s\n\n", i$response.level[1], i$group[1], i$facet[1]), "red")
        .print_block(i, n, digits, ci.lvl, ...)
      }
    }

  } else {

    if (!has_response) {
      x$.nest <- tapply(x$predicted, list(x$group, x$facet, x$panel), NULL)
      xx <- split(x, x$.nest)

      for (i in xx) {
        insight::print_color(sprintf("\n# %s\n# %s\n# %s\n\n", i$group[1], i$facet[1], i$panel[1]), "red")
        .print_block(i, n, digits, ci.lvl, ...)
      }
    } else {
      x$.nest <- tapply(x$predicted, list(x$response.level, x$group, x$facet, x$panel), NULL)
      xx <- split(x, x$.nest)

      for (i in xx) {
        insight::print_color(sprintf("\n# %s\n# %s\n# %s\n# %s\n\n", i$response.level[1], i$group[1], i$facet[1], i$panel[1]), "red")
        .print_block(i, n, digits, ci.lvl, ...)
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

    # ignore this string when determing maximum length
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


  cat("\n")

  fitfun <- attr(x, "fitfun", exact = TRUE)
  if (has_se && !is.null(fitfun) && fitfun != "lm") {
    message("Standard errors are on link-scale (untransformed).")
  }

  predint <- attr(x, "prediction.interval", exact = TRUE)
  if (!is.null(predint) && isTRUE(predint)) {
    message("Intervals are prediction intervals.")
  }
}



.get_sample_rows <- function(x, n) {
  nr.of.rows <- sjmisc::seq_row(x)

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



#' @importFrom insight format_table
.print_block <- function(i, n, digits, ci.lvl, ...) {
  i <- i[setdiff(colnames(i), c("group", "facet", "panel", "response.level", ".nest"))]
  # print.data.frame(, ..., row.names = FALSE, quote = FALSE)
  dd <- i[.get_sample_rows(i, n), ]

  if ("conf.low" %in% colnames(dd) && "conf.high" %in% colnames(dd)) {
    max_len_low <- max(unlist(lapply(stats::na.omit(round(dd$conf.low, digits)), function(.i) nchar(as.character(.i)))))
    max_len_high <- max(unlist(lapply(stats::na.omit(round(dd$conf.high,digits)), function(.i) nchar(as.character(.i)))))

    dd$CI <- .format_confint(dd$conf.low, dd$conf.high, digits = digits, width_low = max_len_low, width_high = max_len_high)
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
  cat(insight::format_table(dd, digits = digits, protect_integers = TRUE))
  # print.data.frame(dd, ..., quote = FALSE, row.names = FALSE)
}




.format_confint <- function(CI_low, CI_high, ci = 0.95, digits = 2, brackets = TRUE, width = NULL, width_low = width, width_high = width) {
  if (!is.null(ci)) {
    ifelse(is.na(CI_low) & is.na(CI_high), "", paste0(ci * 100, "% CI ", .format_ci(
      CI_low,
      CI_high,
      digits = digits,
      brackets = brackets,
      width_low = width_low,
      width_high = width_high
    )))
  } else {
    ifelse(is.na(CI_low) & is.na(CI_high), "", .format_ci(
      CI_low,
      CI_high,
      digits = digits,
      brackets = brackets,
      width_low = width_low,
      width_high = width_high
    ))
  }
}



#' @importFrom insight format_value
.format_ci <- function(CI_low, CI_high, digits = 2, brackets = TRUE, width_low = NULL, width_high = NULL) {
  paste0(ifelse(isTRUE(brackets), "[", ""), insight::format_value(CI_low, digits = digits, missing = "missing", width = width_low), ", ", insight::format_value(CI_high, digits = digits, missing = "missing", width = width_high), ifelse(isTRUE(brackets), "]", ""))
}
