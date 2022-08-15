#' @title Calculate representative values of a vector
#' @name values_at
#'
#' @description This function calculates representative values of a vector,
#'   like minimum/maximum values or lower, median and upper quartile etc.,
#'   which can be used for numeric vectors to plot marginal effects at these
#'   representative values.
#'
#' @param x A numeric vector.
#' @param values Character vector, naming a pattern for which representative values
#'   should be calculcated.
#'   describe{
#'   \item{\code{"minmax"}}{(default) minimum and maximum values (lower and upper bounds) of the moderator are used to plot the interaction between independent variable and moderator.}
#'   \item{\code{"meansd"}}{uses the mean value of the moderator as well as one standard deviation below and above mean value to plot the effect of the moderator on the independent variable.}
#'   \item{\code{"zeromax"}}{is similar to the \code{"minmax"} option, however, \code{0} is always used as minimum value for the moderator. This may be useful for predictors that don't have an empirical zero-value, but absence of moderation should be simulated by using 0 as minimum.}
#'   \item{\code{"fivenum"}}{calculates and uses the Tukey's five number summary (minimum, lower-hinge, median, upper-hinge, maximum) of the moderator value.}
#'   \item{\code{"quart"}}{calculates and uses the quartiles (lower, median and upper) of the moderator value, \emph{including} minimum and maximum value.}
#'   \item{\code{"quart2"}}{calculates and uses the quartiles (lower, median and upper) of the moderator value, \emph{excluding} minimum and maximum value.}
#'   \item{\code{"terciles"}}{calculates and uses the terciles (lower and upper third) of the moderator value, \emph{including} minimum and maximum value.}
#'   \item{\code{"terciles2"}}{calculates and uses the terciles (lower and upper third) of the moderator value, \emph{excluding} minimum and maximum value.}
#'   \item{\code{"all"}}{uses all values of the moderator variable. Note that this option only applies to \code{type = "eff"}, for numeric moderator values.}
#'   }
#'
#' @return A numeric vector of length two or three, representing the required
#'   values from \code{x}, like minimum/maximum value or mean and +/- 1 SD. If
#'   \code{x} is missing, a function, pre-programmed with \code{n} and
#'   \code{length} is returned. See examples.
#'
#' @examples
#' data(efc)
#' values_at(efc$c12hour)
#' values_at(efc$c12hour, "quart2")
#'
#' mean_sd <- values_at(values = "meansd")
#' mean_sd(efc$c12hour)
#' @export
values_at <- function(x, values = "meansd") {

  force(values)
  .values_at <- function(x) {
    # check if representative value is possible to compute
    # e.g. for quantiles, if we have at least three values
    values <- check_rv(values, x)

    # we have more than two values, so re-calculate effects, just using
    # min and max value of moderator.
    if (values == "minmax") {
      # retrieve min and max values
      mv.min <- min(x, na.rm = TRUE)
      mv.max <- max(x, na.rm = TRUE)
      # re-compute effects, prepare xlevels
      xl <- c(mv.min, mv.max)
      # we have more than two values, so re-calculate effects, just using
      # 0 and max value of moderator.
    } else if (values == "zeromax") {
      # retrieve max values
      mv.max <- max(x, na.rm = TRUE)
      # re-compute effects, prepare xlevels
      xl <- c(0, mv.max)
      # compute mean +/- sd
    } else if (values == "meansd") {
      # retrieve mean and sd
      mv.mean <- mean(x, na.rm = TRUE)
      mv.sd <- stats::sd(x, na.rm = TRUE)
      # re-compute effects, prepare xlevels
      xl <- c(mv.mean - mv.sd, mv.mean, mv.mean + mv.sd)
    } else if (values == "all") {
      # re-compute effects, prepare xlevels
      xl <- as.vector(unique(sort(x, na.last = NA)))
    } else if (values == "fivenum") {
      # re-compute effects, prepare xlevels
      xl <- as.vector(stats::fivenum(x, na.rm = TRUE))
    } else if (values == "quart") {
      # re-compute effects, prepare xlevels
      xl <- as.vector(stats::quantile(x, na.rm = TRUE))
    } else if (values == "quart2") {
      # re-compute effects, prepare xlevels
      xl <- as.vector(stats::quantile(x, na.rm = TRUE))[2:4]
    } else if (values == "terciles") {
      # re-compute effects, prepare xlevels
      xl <- as.vector(stats::quantile(x, probs = (0:3) / 3, na.rm = TRUE))
    } else if (values == "terciles2") {
      # re-compute effects, prepare xlevels
      xl <- as.vector(stats::quantile(x, probs = (1:2) / 3, na.rm = TRUE))
    }

    if (is.numeric(x)) {
      if (is.whole(x)) {
        rv <- round(xl, 1)
        if (length(unique(rv)) < length(rv))
          rv <- unique(round(xl, 2))
      } else {
        rv <- round(xl, 2)
      }

      if (length(unique(rv)) < length(rv)) {
        rv <- unique(round(xl, 3))
        if (length(unique(rv)) < length(rv)) {
          rv <- unique(round(xl, 4))
        }
      }
    } else {
      rv <- xl
    }

    rv
  }

  if (missing(x)) {
    .values_at
  } else {
    .values_at(x)
  }
}

check_rv <- function(values, x) {
  if ((is.factor(x) || is.character(x)) && values != "all") {
    # tell user that quart won't work
    message(paste0("Cannot use '", values, "' for factors or character vectors. Defaulting `values` to `all`."))
    values <- "all"
  }

  mvc <- length(unique(as.vector(stats::quantile(x, na.rm = TRUE))))
  if (is.numeric(x) &&
    values %in% c("quart", "quart2", "quartiles", "quartiles2", "terciles", "terciles2") &&
    mvc < 3) {
    # tell user that quart won't work
    message("Could not compute quartiles, too small range of variable. Defaulting `values` to `minmax`.")
    values <- "minmax"
  }

  values
}


#' @rdname values_at
#' @export
representative_values <- values_at
