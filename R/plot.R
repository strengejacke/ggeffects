#' @title Plot ggeffects-objects
#' @name plot
#'
#' @description A generic plot-method for \code{ggeffects}-objects.
#'
#' @param x An object of class \code{ggeffects}, as returned by the functions
#'   from this package.
#' @param ci Logical, if \code{TRUE}, confidence bands (for continuous variables
#'   at x-axis) resp. error bars (for factors at x-axis) are plotted.For
#'   \code{ggeffects}-objects from \code{ggpredict()} with argument
#'   \code{full.data = TRUE}, \code{ci} is automatically set to \code{FALSE}.
#' @param ci.style Character vector, indicating the style of the confidence
#'   bands. May be either \code{"ribbon"}, \code{"errorbar"}, \code{"dash"} or
#'   \code{"dot"}, to plot a ribbon, error bars, or dashed or dotted lines as
#'   confidence bands.
#' @param facets,grid Logical, defaults to \code{TRUE}, if \code{x} has a column named
#'   \code{facet}, and defaults to \code{FALSE}, if \code{x} has no such
#'   column. Set \code{facets = TRUE} to wrap the plot into facets even
#'   for grouping variables (see 'Examples'). \code{grid} is an alias for
#'   \code{facets}.
#' @param rawdata Logical, if \code{TRUE}, a layer with raw data from response by
#'   predictor on the x-axis, plotted as point-geoms, is added to the plot.
#' @param colors Character vector with color values in hex-format, valid
#'   color value names (see \code{demo("colors")}) or a name of a
#'   \href{http://colorbrewer2.org}{color brewer} palette.
#'   Following options are valid for \code{colors}:
#'   \itemize{
#'     \item If not specified, the color brewer palette "Set1" will be used.
#'     \item If \code{"gs"}, a greyscale will be used.
#'     \item If \code{"bw"}, the plot is black/white and uses different line types to distinguish groups.
#'     \item If \code{colors} is any valid color brewer palette name, the related palette will be used. Use \code{\link[RColorBrewer]{display.brewer.all}} to view all available palette names.
#'     \item Else specify own color values or names as vector (e.g. \code{colors = c("#f00000", "#00ff00")}).
#'   }
#' @param alpha Alpha value for the confidence bands.
#' @param line.size Numeric, size of the line geoms.
#' @param dot.size Numeric, size of the point geoms.
#' @param dodge Value for offsetting or shifting error bars, to avoid overlapping.
#'   Only applies, if a factor is plotted at the x-axis (in such cases, the
#'   confidence bands are replaced by error bars automatically), or if
#'   \code{ci.style = "errorbars"}.
#' @param use.theme Logical, if \code{TRUE}, a slightly tweaked version of ggplot's
#'   minimal-theme, \code{theme_ggeffects()}, is applied to the plot. If
#'   \code{FALSE}, no theme-modifications are applied.
#' @param dot.alpha Alpha value for data points, when \code{rawdata = TRUE}.
#' @param jitter Numeric, between 0 and 1. If not \code{NULL} and
#'   \code{rawdata = TRUE}, adds a small amount of random variation to
#'   the location of data points dots, to avoid overplotting. Hence the
#'   points don't reflect exact values in the data. For binary outcomes,
#'   raw data is never jittered to avoid that data points exceed the axis
#'   limits.
#' @param log.y Logical, if \code{TRUE}, the y-axis scale is log-transformed.
#'   This might be useful for binomial models with predicted probabilities on
#'   the y-axis.
#' @param show.legend Logical, shows or hides the plot legend.
#' @param show.title Logical, shows or hides the plot title-
#' @param show.x.title Logical, shows or hides the plot title for the x-axis.
#' @param show.y.title Logical, shows or hides the plot title for the y-axis.
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @param ... Further arguments passed down to \code{ggplot::scale_y*()}, to
#'    control the appearance of the y-axis.
#'
#' @inheritParams get_title
#'
#' @return A ggplot2-object.
#'
#' @note Load \code{library(ggplot2)} and use \code{theme_set(theme_ggeffects())}
#'   to set the \pkg{ggeffects}-theme as default plotting theme. You can then use
#'   further plot-modifiers from \pkg{sjPlot}, like \code{legend_style()} or
#'   \code{font_size()} without losing the theme-modifications.
#'
#' @details \code{ggpredict()} with argument \code{full.data = FALSE} computes
#'          marginal effects at the mean, where covariates are held constant. In
#'          this case, the slope between groups does not vary and the standard
#'          errors and confidence intervals have the same "trend" as the predicted
#'          values. Hence, plotting confidence bands or error bars is possible.
#'          However, \code{ggpredict()} with argument \code{full.data = TRUE},
#'          covariates and standard errors vary between groups, so plotting
#'          confidence bands and error bars would follow a "winding" shape,
#'          while the predicted values are smoothened by \code{\link[ggplot2]{geom_smooth}}.
#'          Predicted values and confidence bands or error bars would no
#'          longer match, thus, \code{ci} is automatically set to \code{FALSE}
#'          in such cases. You still may want to plot objects returned by
#'          \code{ggpredict()} with argument \code{full.data = TRUE} to additionally
#'          plot the raw data points, which is automatically done.
#'          \cr \cr
#'          For \code{ggaverage()}, which computes average marginal effects,
#'          the same problem with standard errors and confidence bands would
#'          apply. However, the standard errors for the average marginal effects
#'          are taken from the marginal effects at the mean, and the predicted
#'          values from the average marginal effects are used to compute
#'          another regression on these values, to get the "smoothened" values
#'          that are used to compute standard errors and confidence intervals
#'          that match the predicted values of the average marginal effects
#'          (maybe, at this point, it is helpful to inspect the code to better
#'          understand what is happening...).
#'          \cr \cr
#'          For proportional odds logistic regression (see \code{\link[MASS]{polr}})
#'          or cumulative link models in general, plots are automatically facetted
#'          by \code{response.level}, which indicates the grouping of predictions
#'          based on the level of the model's response.
#'
#' @examples
#' library(sjlabelled)
#' data(efc)
#' efc$c172code <- as_label(efc$c172code)
#' fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
#'
#' dat <- ggpredict(fit, terms = "c12hour")
#' plot(dat)
#'
#' dat <- ggpredict(fit, terms = "c12hour", full.data = TRUE)
#' plot(dat)
#'
#' dat <- ggaverage(fit, terms = "neg_c_7")
#' plot(dat)
#' plot(dat, ci = "dash")
#'
#' # facet by group
#' dat <- ggpredict(fit, terms = c("c12hour", "c172code"))
#' plot(dat, facet = TRUE)
#'
#' # don't use facets, b/w figure, w/o confidence bands
#' dat <- ggaverage(fit, terms = c("c12hour", "c172code"))
#' plot(dat, colors = "bw", ci = FALSE)
#'
#' # factor at x axis, plot exact data points and error bars
#' dat <- ggpredict(fit, terms = c("c172code", "c161sex"))
#' plot(dat)
#'
#' # for three variables, automatic facetting
#' dat <- ggpredict(fit, terms = c("c12hour", "c172code", "c161sex"))
#' plot(dat)
#'
#'
#' @importFrom ggplot2 ggplot aes_string geom_smooth facet_wrap labs guides geom_point geom_ribbon geom_errorbar scale_x_continuous position_dodge theme_minimal position_jitter scale_color_manual scale_fill_manual geom_line geom_jitter scale_y_continuous element_text theme element_line element_rect scale_y_log10
#' @importFrom stats binomial poisson gaussian Gamma inverse.gaussian quasi quasibinomial quasipoisson
#' @importFrom sjmisc empty_cols zap_inf
#' @importFrom sjlabelled as_numeric
#' @importFrom scales percent
#' @importFrom dplyr n_distinct
#' @export
plot.ggeffects <- function(x,
                           ci = TRUE,
                           ci.style = c("ribbon", "errorbar", "dash", "dot"),
                           facets,
                           rawdata = FALSE,
                           colors = "Set1",
                           alpha = .15,
                           dodge = .1,
                           use.theme = TRUE,
                           dot.alpha = .5,
                           jitter = .2,
                           log.y = FALSE,
                           case = NULL,
                           show.legend = TRUE,
                           show.title = TRUE,
                           show.x.title = TRUE,
                           show.y.title = TRUE,
                           dot.size = NULL,
                           line.size = NULL,
                           grid,
                           ...) {

  # set some defaults

  jitter.miss <- missing(jitter)

  if (isTRUE(jitter))
    jitter <- .2
  else if (is.logical(jitter) && length(jitter) == 1L && !is.na(jitter) && !jitter)
    jitter <- NULL

  y.breaks <- NULL
  y.limits <- NULL

  if (is.null(dot.size)) dot.size <- 2.5
  if (is.null(line.size)) line.size <- .7

  if (!missing(grid)) facets <- grid
  ci.style <- match.arg(ci.style)

  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  if (!("breaks" %in% names(add.args)) && isTRUE(log.y)) {
    y.breaks <- 2 ^ sjmisc::zap_inf(unique(round(log2(pretty(c(min(x$conf.low), max(x$conf.high)))))))
    y.breaks <- y.breaks[!is.na(y.breaks)]
    y.limits <- c(min(y.breaks), max(y.breaks))

    # this is a REALLY sloppy hack to avoid that axis limits are not 0 for
    # log-scale, and that axis limits cover the range of the plotted geoms
    # I think there's a more elegant solution, so please let me know...

    if (y.limits[1] > min(x$conf.low)) y.limits[1] <- y.limits[1] / 2
    if (y.limits[2] < max(x$conf.high)) y.limits[2] <- y.limits[2] * 2
    if (y.limits[1] > min(x$conf.low)) y.limits[1] <- y.limits[1] / 2
    if (y.limits[2] < max(x$conf.high)) y.limits[2] <- y.limits[2] * 2
  }


  # do we have groups and facets?
  has_groups <- obj_has_name(x, "group") && length(unique(x$group)) > 1
  has_facets <- obj_has_name(x, "facet") && length(unique(x$facet)) > 1

  # convert x back to numeric
  if (!is.numeric(x$x)) x$x <- sjlabelled::as_numeric(x$x)

  # special solution for polr
  facet_polr <- FALSE
  if (obj_has_name(x, "response.level") && length(unique(x$response.level)) > 1) {
    has_facets <- TRUE
    facet_polr <- TRUE
  }

  # do we have full data (average effects), or expanded grid?
  has_full_data <- attr(x, "full.data", exact = TRUE) == "1"

  # for ggalleffects, we don't have this attribute
  if (sjmisc::is_empty(has_full_data)) has_full_data <- FALSE

  # no CI for full data, because these are not computed
  if (has_full_data) ci <- FALSE

  # set default, if argument not specified
  if (has_facets)
    facets <- TRUE
  else if (missing(facets) || is.null(facets))
    facets <- has_facets


  # facets, but only groups? here the user wants to
  # plot facets for the grouping variable
  facets_grp <- facets && !has_facets

  # is x a factor?
  xif <- attr(x, "x.is.factor", exact = TRUE)
  x_is_factor <- !is.null(xif) && xif == "1"


  # set CI to false if we don't have SE and CI, or if we have full data
  if ("conf.low" %in% names(sjmisc::empty_cols(x)) ||
      has_full_data || !obj_has_name(x, "conf.low"))
    ci <- FALSE


  # base plot, set mappings
  if (has_groups && !facets_grp && colors[1] == "bw" && x_is_factor)
    p <- ggplot2::ggplot(x, ggplot2::aes_string(x = "x", y = "predicted", colour = "group", fill = "group", shape = "group"))
  else if (has_groups && !facets_grp && colors[1] == "bw" && !x_is_factor)
    p <- ggplot2::ggplot(x, ggplot2::aes_string(x = "x", y = "predicted", colour = "group", fill = "group", linetype = "group"))
  else if (has_groups && !facets_grp && colors[1] == "gs" && x_is_factor)
    p <- ggplot2::ggplot(x, ggplot2::aes_string(x = "x", y = "predicted", colour = "group", fill = "group", shape = "group"))
  else if (has_groups && colors[1] != "bw")
    p <- ggplot2::ggplot(x, ggplot2::aes_string(x = "x", y = "predicted", colour = "group", fill = "group"))
  else
    p <- ggplot2::ggplot(x, ggplot2::aes_string(x = "x", y = "predicted"))


  # get color values
  colors <- get_colors(colors, length(unique(x$group)))


  # now plot the geom. we use a smoother for a continuous x, and
  # a point-geom, if x was a factor. In this case, the x-value is still
  # numeric, but we need to plot exact data points between categories
  # and no smoothing across all x-values

  if (has_full_data) {

    # we need a smoother on our predictions, but loess for 1 degree
    p <- p +
      ggplot2::geom_smooth(
        method = "loess",
        method.args = list(family = "symmetric", degree = 1),
        se = FALSE
      ) +
      # if we have full data, also plot data points
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = .1, height = .1),
        alpha = alpha
      )

  } else if (x_is_factor) {
    # for x as factor
    p <- p + ggplot2::geom_point(
      position = ggplot2::position_dodge(width = dodge),
      size = dot.size
    )
  } else {
    # classical line
    p <- p + ggplot2::geom_line(size = line.size)
  }


  # CI?
  if (ci) {

    # for a factor on x-axis, use error bars

    if (x_is_factor) {
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes_string(ymin = "conf.low", ymax = "conf.high"),
        position = ggplot2::position_dodge(width = dodge),
        width = .1,
        size = line.size
      )

    } else {

      if (ci.style == "ribbon") {
        # for continuous x, use ribbons by default
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes_string(ymin = "conf.low", ymax = "conf.high", colour = NULL, linetype = NULL, shape = NULL),
          alpha = alpha
        )
      } else if (ci.style == "errorbar") {
        p <- p + ggplot2::geom_point(
            position = ggplot2::position_dodge(width = dodge),
            size = dot.size
          ) +
          ggplot2::geom_errorbar(
            ggplot2::aes_string(ymin = "conf.low", ymax = "conf.high", shape = NULL),
            position = ggplot2::position_dodge(width = dodge),
            size = line.size,
            width = 0
          )
      } else {

        lt <- switch(
          ci.style,
          dash = 2,
          dot = 3,
          2
        )

        p <- p +
          ggplot2::geom_line(
            ggplot2::aes_string(y = "conf.low", linetype = NULL),
            linetype = lt
          ) +
          ggplot2::geom_line(
            ggplot2::aes_string(y = "conf.high", linetype = NULL),
            linetype = lt
          )
      }
    }
  }


  # If we have x-axis-labels, use these to label the axis
  x_lab <- get_x_labels(x, case)

  if (!is.null(x_lab)) {
    p <- p + ggplot2::scale_x_continuous(breaks = unique(x$x), labels = x_lab)
  }


  # facets?
  if (facets_grp) {
    # facet groups
    p <- p + ggplot2::facet_wrap(~group, scales = "free_x")
    # remove legends
    p <- p + ggplot2::guides(colour = "none", linetype = "none", shape = "none")
  } else if (facet_polr) {
    p <- p + ggplot2::facet_wrap(~response.level, scales = "free_x")
  } else if (facets) {
    p <- p + ggplot2::facet_wrap(~facet, scales = "free_x")
  }


  # plot raw data points. we need an own aes for this
  if (rawdata) {
    # get raw data and check, if any data available
    rawdat <- attr(x, "rawdata", exact = TRUE)

    if (!is.null(rawdat)) {
      # make sure response is numeric
      rawdat$response <- sjlabelled::as_numeric(rawdat$response)

      # check if we have a group-variable with at least two groups
      if (obj_has_name(rawdat, "group"))
        grps <- dplyr::n_distinct(rawdat$group, na.rm = TRUE) > 1
      else
        grps <- FALSE

      # check if we have only selected values for groups, in this case
      # filter raw data to match grouping colours
      if (grps &&
          dplyr::n_distinct(rawdat$group, na.rm = TRUE) > dplyr::n_distinct(x$group, na.rm = TRUE)) {
        rawdat <- rawdat[which(rawdat$group %in% x$group), ]
      }


      # if we have groups, add colour aes, to map raw data to
      # grouping variable

      if (grps)
        mp <- ggplot2::aes_string(x = "x", y = "response", colour = "group")
      else
        mp <- ggplot2::aes_string(x = "x", y = "response")


      # for binary response, no jittering by default

      if ((attr(x, "logistic", exact = TRUE) == "1" && jitter.miss) || is.null(jitter)) {
        p <- p + ggplot2::geom_point(
          data = rawdat,
          mapping = mp,
          alpha = dot.alpha,
          size = dot.size,
          show.legend = FALSE,
          inherit.aes = FALSE
        )
      } else {
        p <- p + ggplot2::geom_jitter(
          data = rawdat,
          mapping = mp,
          alpha = dot.alpha,
          size = dot.size,
          width = jitter,
          height = jitter,
          show.legend = FALSE,
          inherit.aes = FALSE
        )
      }
    } else {
      message("Raw data not available.")
    }
  }


  # set colors
  p <- p +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors)


  # show/hide titles
  if (!show.title) attr(x, "title") <- NULL
  if (!show.x.title) attr(x, "x.title") <- NULL
  if (!show.y.title) attr(x, "y.title") <- NULL


  # set axis titles
  p <- p + ggplot2::labs(
    title = get_title(x, case),
    x = get_x_title(x, case),
    y = get_y_title(x, case),
    fill = NULL
  )

  if (has_groups && show.legend)
    p <- p + ggplot2::labs(
      colour = get_legend_title(x, case),
      linetype = get_legend_title(x, case),
      shape = get_legend_title(x, case)
    )

  # no legend for fill-aes
  p <- p + ggplot2::guides(fill = "none")


  # show or hide legend?
  if (!show.legend) {
    p <- p + ggplot2::labs(
      colour = NULL,
      linetype = NULL,
      shape = NULL
    ) + ggplot2::guides(colour = "none", linetype = "none", shape = "none")
  }


  # for binomial family, fix coord

  if (attr(x, "logistic", exact = TRUE) == "1") {
    if (log.y) {
      if (is.null(y.breaks))
        p <- p + ggplot2::scale_y_log10(labels = scales::percent, ...)
      else
        p <- p + ggplot2::scale_y_log10(labels = scales::percent, breaks = y.breaks, limits = y.limits, ...)
    } else
      p <- p + ggplot2::scale_y_continuous(labels = scales::percent, ...)
  } else if (log.y) {
    if (is.null(y.breaks))
      p <- p + ggplot2::scale_y_log10(...)
    else
      p <- p + ggplot2::scale_y_log10(breaks = y.breaks, limits = y.limits, ...)
  } else {
    p <- p + ggplot2::scale_y_continuous(...)
  }


  # tweak theme

  if (use.theme)
    p <- p + theme_ggeffects()

  p
}


#' @importFrom purrr map map_df
#' @importFrom graphics plot
#' @export
plot.ggalleffects <- function(x,
                              ci = TRUE,
                              facets,
                              rawdata = FALSE,
                              colors = "Set1",
                              alpha = .15,
                              dodge = .1,
                              use.theme = TRUE,
                              dot.alpha = .5,
                              jitter = TRUE,
                              log.y = FALSE,
                              case = NULL,
                              show.legend = TRUE,
                              show.title = TRUE,
                              show.x.title = TRUE,
                              show.y.title = TRUE,
                              dot.size = NULL,
                              line.size = NULL,
                              ...) {

  if (missing(facets)) facets <- NULL

  if (isTRUE(facets)) {
    # merge all effect-data frames into one
    dat <- get_complete_df(x)

    rawdat <- suppressWarnings(
      purrr::map_df(x, function(d) {
        tmp <- attr(d, "rawdata")
        tmp$group <- d$group[1]
        tmp
      })
    )

    # copy raw data
    attr(dat, "rawdata") <- rawdat

    # set various attributes
    attr(dat, "x.is.factor") <- attr(x[[1]], "x.is.factor", exact = T)
    attr(dat, "family") <- attr(x[[1]], "family", exact = T)
    attr(dat, "link") <- attr(x[[1]], "link", exact = T)
    attr(dat, "logistic") <- attr(x[[1]], "logistic", exact = T)
    attr(dat, "fitfun") <- attr(x[[1]], "fitfun", exact = T)

    graphics::plot(
      x = dat,
      ci = ci,
      facets = TRUE,
      rawdata = rawdata,
      colors = colors,
      alpha = alpha,
      dodge = dodge,
      use.theme = use.theme,
      dot.alpha = dot.alpha,
      jitter = jitter,
      log.y = log.y,
      case = case,
      show.legend = show.legend,
      show.title = FALSE,
      show.x.title = show.x.title,
      show.y.title = FALSE,
      dot.size = dot.size,
      line.size = line.size,
      ...
    )
  } else {
    purrr::map(
      x,
      ~ graphics::plot(
          x = .x,
          ci = ci,
          facets = facets,
          rawdata = rawdata,
          colors = colors,
          alpha = alpha,
          dodge = dodge,
          use.theme = use.theme,
          dot.alpha = dot.alpha,
          jitter = jitter,
          log.y = log.y,
          case = case,
          show.legend = show.legend,
          show.title = show.title,
          show.x.title = show.x.title,
          show.y.title = show.y.title,
          dot.size = dot.size,
          line.size = line.size
      )
    )
  }
}


#' @importFrom ggplot2 theme_minimal theme element_line element_rect element_text
#' @rdname plot
#' @export
theme_ggeffects <- function(base_size = 11, base_family = "") {
  (ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
     ggplot2::theme(
       axis.line.x      = ggplot2::element_line(colour = "grey80"),
       axis.line.y      = ggplot2::element_line(colour = "grey80"),
       axis.text        = ggplot2::element_text(colour = "grey50"),
       axis.title       = ggplot2::element_text(colour = "grey30"),
       strip.background = ggplot2::element_rect(colour = "grey70", fill = "grey90"),
       strip.text       = ggplot2::element_text(colour = "grey30"),
       legend.title     = ggplot2::element_text(colour = "grey30"),
       legend.text      = ggplot2::element_text(colour = "grey30")
     ))
}
