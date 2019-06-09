#' @importFrom purrr map flatten_df
#' @importFrom dplyr select group_by n_distinct case_when
#' @importFrom sjmisc round_num is_empty add_variables seq_row is_num_fac
#' @importFrom stats quantile
#' @importFrom rlang .data
#' @importFrom sjlabelled as_label get_labels
#' @export
print.ggeffects <- function(x, n = 10, digits = 3, x.lab = FALSE, ...) {

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
  has_groups <- obj_has_name(x, "group") && length(unique(x$group)) > 1
  has_facets <- obj_has_name(x, "facet") && length(unique(x$facet)) > 1
  has_panel <- obj_has_name(x, "panel") && length(unique(x$panel)) > 1
  has_se <- obj_has_name(x, "std.error")

  cat("\n")

  lab <- attr(x, "title", exact = TRUE)
  if (!is.null(lab)) insight::print_color(paste0(sprintf("# %s", lab), "\n", collapse = ""), "blue")

  lab <- attr(x, "x.title", exact = TRUE)
  if (!is.null(lab)) insight::print_color(paste0(sprintf("# x = %s", lab), "\n", collapse = ""), "blue")

  consv <- attr(x, "constant.values")
  terms <- attr(x, "terms")

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
    .n <- dplyr::n_distinct(x$group, na.rm = T)
    if (!is.null(terms) && length(terms) >= 2) {
      x$group <- sprintf("%s = %s", terms[2], as.character(x$group))
    }
  }

  if (has_facets) {
    .n <- .n * dplyr::n_distinct(x$facet, na.rm = T)
    if (!is.null(terms) && length(terms) >= 3) {
      x$facet <- sprintf("%s = %s", terms[3], as.character(x$facet))
    }
  }

  if (has_panel) {
    .n <- .n * dplyr::n_distinct(x$panel, na.rm = T)
    if (!is.null(terms) && length(terms) >= 4) {
      x$panel <- sprintf("%s = %s", terms[4], as.character(x$panel))
    }
  }

  # make sure that by default not too many rows are printed
  if (missing(n)) {
    n <- dplyr::case_when(
      .n >= 6 ~ 4,
      .n >= 4 & .n < 6 ~ 5,
      .n >= 2 & .n < 4 ~ 6,
      TRUE ~ 8
    )
  }

  if (!has_groups) {
    cat("\n")
    if (obj_has_name(x, "group"))
      x <- dplyr::select(x, -.data$group)
    print.data.frame(x[get_sample_rows(x, n), ], ..., row.names = FALSE, quote = FALSE)
  } else if (has_groups && !has_facets) {
    xx <- x %>%
      dplyr::group_by(.data$group) %>%
      .nest()

    for (i in 1:nrow(xx)) {
      insight::print_color(sprintf("\n# %s\n", xx[i, 1]), "red")
      tmp <- purrr::flatten_df(xx[i, 2])
      print.data.frame(tmp[get_sample_rows(tmp, n), ], ..., row.names = FALSE, quote = FALSE)
    }
  } else if (has_groups && has_facets && !has_panel) {
    xx <- x %>%
      dplyr::group_by(.data$group, .data$facet) %>%
      .nest()

    for (i in 1:nrow(xx)) {
      insight::print_color(sprintf("\n# %s\n# %s\n", xx[i, 1], xx[i, 2]), "red")
      tmp <- purrr::flatten_df(xx[i, 3])
      print.data.frame(tmp[get_sample_rows(tmp, n), ], ..., row.names = FALSE, quote = FALSE)
    }
  } else {
    xx <- x %>%
      dplyr::group_by(.data$group, .data$facet, .data$panel) %>%
      .nest()

    for (i in 1:nrow(xx)) {
      insight::print_color(sprintf("\n# %s\n# %s\n# %s\n", xx[i, 1], xx[i, 2], xx[i, 3]), "red")
      tmp <- purrr::flatten_df(xx[i, 4])
      print.data.frame(tmp[get_sample_rows(tmp, n), ], ..., row.names = FALSE, quote = FALSE)
    }
  }

  cv <- purrr::map(
    consv,
    function(.x) {
      if (is.numeric(.x))
        sprintf("%.2f", .x)
      else
        as.character(.x)
    })

  if (!sjmisc::is_empty(cv)) {
    cv.names <- names(cv)
    cv.space <- max(nchar(cv.names))

    # ignore this string when determing maximum length
    poplev <- which(cv %in% c("NA (population-level)", "0 (population-level)"))
    if (!sjmisc::is_empty(poplev))
      mcv <- cv[-poplev]
    else
      mcv <- cv

    if (!sjmisc::is_empty(mcv))
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


get_sample_rows <- function(x, n) {
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
