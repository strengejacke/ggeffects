#' @title Get titles and labels from data
#' @name get_title
#'
#' @description
#' Get variable and value labels from `ggeffects`-objects. Functions like
#' `ggpredict()` or `ggeffect()` save information on variable names and value
#' labels as additional attributes in the returned data frame. This is especially
#' helpful for labelled data (see **sjlabelled**), since these labels can be used
#' to set axis labels and titles.
#'
#' @param x An object of class `ggeffects`, as returned by any ggeffects-function;
#'   for `get_complete_df()`, must be a list of `ggeffects`-objects.
#' @param case Desired target case. Labels will automatically converted into the
#'   specified character case. See `?sjlabelled::convert_case` for more details
#'   on this argument.
#'
#' @return The titles or labels as character string, or `NULL`, if variables
#'         had no labels; `get_complete_df()` returns the input list `x`
#'         as single data frame, where the grouping variable indicates the
#'         predicted values for each term.
#'
#' @examplesIf require("sjmisc", quietly = TRUE) && require("ggplot2", quietly = TRUE) && require("effects", quietly = TRUE)
#' data(efc)
#' efc$c172code <- to_factor(efc$c172code)
#' fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
#'
#' mydf <- ggpredict(fit, terms = c("c12hour", "c161sex", "c172code"))
#'
#' ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
#'   stat_smooth(method = "lm") +
#'   facet_wrap(~facet, ncol = 2) +
#'   labs(
#'     x = get_x_title(mydf),
#'     y = get_y_title(mydf),
#'     colour = get_legend_title(mydf)
#'   )
#'
#' # adjusted predictions, a list of data frames (one data frame per term)
#' eff <- ggeffect(fit)
#' eff
#' get_complete_df(eff)
#'
#' # adjusted predictions for education only, and get x-axis-labels
#' mydat <- eff[["c172code"]]
#' ggplot(mydat, aes(x = x, y = predicted, group = group)) +
#'   stat_summary(fun = sum, geom = "line") +
#'   scale_x_discrete(labels = get_x_labels(mydat))
#' @export
get_title <- function(x, case = NULL) {
  if (.is_empty(x)) return(NULL)
  .convert_case(x, "title", case)
}


#' @rdname get_title
#' @export
get_x_title <- function(x, case = NULL) {
  if (.is_empty(x)) return(NULL)
  .convert_case(x, "x.title", case)
}


#' @rdname get_title
#' @export
get_y_title <- function(x, case = NULL) {
  if (.is_empty(x)) return(NULL)
  .convert_case(x, "y.title", case)
}


#' @rdname get_title
#' @export
get_legend_title <- function(x, case = NULL) {
  if (.is_empty(x)) return(NULL)
  .convert_case(x, "legend.title", case)
}


#' @rdname get_title
#' @export
get_legend_labels <- function(x, case = NULL) {
  if (.is_empty(x)) return(NULL)
  .convert_case(x, "legend.labels", case)
}


#' @rdname get_title
#' @export
get_x_labels <- function(x, case = NULL) {
  if (.is_empty(x)) return(NULL)

  labs <- attr(x, which = "x.axis.labels", exact = TRUE)

  if (!is.null(labs) && !is.numeric(labs)) {
    .convert_case(x, "x.axis.labels", case)
  } else {
    labs
  }
}


#' @rdname get_title
#' @export
get_complete_df <- function(x, case = NULL) {
  suppressWarnings(do.call(rbind, lapply(x, function(df) {
    df$x <- .factor_to_numeric(df$x)
    df
  })))
}


get_sub_title <- function(x, case = NULL) {
  if (.is_empty(x)) return(NULL)

  st <- attr(x, which = "n.trials", exact = TRUE)
  panel <- attr(x, which = "panel.title", exact = TRUE)

  if (!is.null(panel))
    .convert_case(x, "panel.title", case)
  else if (!is.null(st))
    sprintf("(for %s trials)", st)
  else
    NULL
}


# helper ---------------------------

.convert_case <- function(x, name, case = NULL) {
  if (!inherits(x, "ggeffects")) {
    insight::format_error("`x` must be of class `ggeffects`.")
  }

  label <- attr(x, which = name, exact = TRUE)
  if (isTRUE(insight::check_if_installed("sjlabelled", quietly = TRUE))) {
    sjlabelled::convert_case(label, case)
  } else {
    label
  }
}


.get_labels <- function(x, char_values = NULL, ...) {
  if (!is.null(attributes(x)$labels) && isTRUE(insight::check_if_installed("sjlabelled", quietly = TRUE))) {
    args <- list(x, ...)
    out <- do.call(sjlabelled::get_labels, args)
  } else {
    if (is.factor(x)) {
      out <- levels(x)
    } else if (is.character(x)) {
      if (!is.null(char_values)) {
        out <- char_values
      } else {
        out <- unique(x)
      }
    } else {
      out <- NULL
    }
  }
  out
}


.get_label <- function(x, default = NULL) {
  out <- attr(x, "label", exact = TRUE)
  if (is.null(out)) {
    out <- default
  }
  out
}


.as_label <- function(x, ...) {
  if (isTRUE(insight::check_if_installed("sjlabelled", quietly = TRUE))) {
    args <- list(x, ...)
    out <- do.call(sjlabelled::as_label, args)
  } else {
    out <- x
  }
  out
}
