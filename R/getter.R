#' @title Get titles and labels from data
#' @name get_title
#'
#' @description Get variable and value labels from \code{ggeffects}-objects. Functions
#'              like \code{ggpredict()} or \code{ggeffect()} save
#'              information on variable names and value labels as additional attributes
#'              in the returned data frame. This is especially helpful for labelled
#'              data (see \CRANpkg{sjlabelled}), since these labels can be used to
#'              set axis labels and titles.
#'
#' @param x An object of class \code{ggeffects}, as returned by any ggeffects-function;
#'          for \code{get_complete_df()}, must be a list of \code{ggeffects}-objects.
#' @param case Desired target case. Labels will automatically converted into the
#'          specified character case. See \code{?sjlabelled::convert_case} for
#'          more details on this argument.
#'
#' @return The titles or labels as character string, or \code{NULL}, if variables
#'         had no labels; \code{get_complete_df()} returns the input list \code{x}
#'         as single data frame, where the grouping variable indicates the
#'         marginal effects for each term.
#'
#' @examples
#' library(sjmisc)
#' data(efc)
#' efc$c172code <- to_factor(efc$c172code)
#' fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
#'
#' mydf <- ggpredict(fit, terms = c("c12hour", "c161sex", "c172code"))
#'
#' library(ggplot2)
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
#'
#' @export
get_title <- function(x, case = NULL) {
  if (.is_empty(x)) return(NULL)

  if (!inherits(x, "ggeffects"))
    stop("`x` must be of class `ggeffects`.", call. = FALSE)

  sjlabelled::convert_case(attr(x, which = "title", exact = TRUE), case)
}


#' @rdname get_title
#' @export
get_x_title <- function(x, case = NULL) {
  if (.is_empty(x)) return(NULL)

  if (!inherits(x, "ggeffects"))
    stop("`x` must be of class `ggeffects`.", call. = FALSE)

  sjlabelled::convert_case(attr(x, which = "x.title", exact = TRUE), case)
}


#' @rdname get_title
#' @export
get_y_title <- function(x, case = NULL) {
  if (.is_empty(x)) return(NULL)

  if (!inherits(x, "ggeffects"))
    stop("`x` must be of class `ggeffects`.", call. = FALSE)

  sjlabelled::convert_case(attr(x, which = "y.title", exact = TRUE), case)
}


#' @rdname get_title
#' @export
get_legend_title <- function(x, case = NULL) {
  if (.is_empty(x)) return(NULL)

  if (!inherits(x, "ggeffects"))
    stop("`x` must be of class `ggeffects`.", call. = FALSE)

  sjlabelled::convert_case(attr(x, which = "legend.title", exact = TRUE), case)
}


#' @rdname get_title
#' @export
get_legend_labels <- function(x, case = NULL) {
  if (.is_empty(x)) return(NULL)

  if (!inherits(x, "ggeffects"))
    stop("`x` must be of class `ggeffects`.", call. = FALSE)

  sjlabelled::convert_case(attr(x, which = "legend.labels", exact = TRUE), case)
}


#' @rdname get_title
#' @export
get_x_labels <- function(x, case = NULL) {
  if (.is_empty(x)) return(NULL)

  if (!inherits(x, "ggeffects"))
    stop("`x` must be of class `ggeffects`.", call. = FALSE)

  labs <- attr(x, which = "x.axis.labels", exact = TRUE)

  if (!is.numeric(labs)) {
    sjlabelled::convert_case(attr(x, which = "x.axis.labels", exact = TRUE), case)
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

  if (!inherits(x, "ggeffects"))
    stop("`x` must be of class `ggeffects`.", call. = FALSE)

  st <- attr(x, which = "n.trials", exact = TRUE)
  panel <- attr(x, which = "panel.title", exact = TRUE)

  if (!is.null(panel))
    sjlabelled::convert_case(panel, case)
  else if (!is.null(st))
    sprintf("(for %s trials)", st)
  else
    NULL
}


