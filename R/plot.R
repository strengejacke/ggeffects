#' @title Plot ggeffects-objects
#' @name plot
#'
#' @description A generic plot-method for \code{ggeffects}-objects.
#'
#' @param x An object of class \code{ggeffects}, as returned by the functions
#'   from this package.
#' @param ci Logical, if \code{TRUE}, confidence bands (for continuous variables
#'   at x-axis) resp. error bars (for factors at x-axis) are plotted.
#' @param ci.style Character vector, indicating the style of the confidence
#'   bands. May be either \code{"ribbon"}, \code{"errorbar"}, \code{"dash"} or
#'   \code{"dot"}, to plot a ribbon, error bars, or dashed or dotted lines as
#'   confidence bands.
#' @param facets,grid Logical, defaults to \code{TRUE}, if \code{x} has a column named
#'   \code{facet}, and defaults to \code{FALSE}, if \code{x} has no such
#'   column. Set \code{facets = TRUE} to wrap the plot into facets even
#'   for grouping variables (see 'Examples'). \code{grid} is an alias for
#'   \code{facets}.
#' @param add.data,rawdata Logical, if \code{TRUE}, a layer with raw data from response by
#'   predictor on the x-axis, plotted as point-geoms, is added to the plot.
#' @param limit.range Logical, if \code{TRUE}, limits the range of the prediction
#'   bands to the range of the data.
#' @param residuals Logical, if \code{TRUE}, a layer with partial residuals is
#'   added to the plot. See vignette \href{https://cran.r-project.org/package=effects}{"Effect Displays with Partial Residuals"}
#'   from \pkg{effects} for more details on partial residual plots.
#' @param residuals.line Logical, if \code{TRUE}, a loess-fit line is added to the
#'   partial residuals plot. Only applies if \code{residuals} is \code{TRUE}.
#' @param colors Character vector with color values in hex-format, valid
#'   color value names (see \code{demo("colors")}) or a name of a
#'   ggeffects-color-palette.
#'   Following options are valid for \code{colors}:
#'   \itemize{
#'     \item If not specified, the color brewer palette "Set1" will be used.
#'     \item If \code{"gs"}, a greyscale will be used.
#'     \item If \code{"bw"}, the plot is black/white and uses different line types to distinguish groups.
#'     \item There are some pre-defined color-palettes in this package that can be used, e.g. \code{colors = "metro"}. See \code{\link[=show_pals]{show_pals()}} to show all available palettes.
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
#' @param dot.alpha Alpha value for data points, when \code{add.data = TRUE}.
#' @param jitter Numeric, between 0 and 1. If not \code{NULL} and
#'   \code{add.data = TRUE}, adds a small amount of random variation to
#'   the location of data points dots, to avoid overplotting. Hence the
#'   points don't reflect exact values in the data. May also be a numeric
#'   vector of length two, to add different horizontal and vertical jittering.
#'   For binary outcomes, raw data is not jittered by default to avoid that
#'   data points exceed the axis limits.
#' @param log.y Logical, if \code{TRUE}, the y-axis scale is log-transformed.
#'   This might be useful for binomial models with predicted probabilities on
#'   the y-axis.
#' @param show.legend Logical, shows or hides the plot legend.
#' @param show.title Logical, shows or hides the plot title-
#' @param show.x.title Logical, shows or hides the plot title for the x-axis.
#' @param show.y.title Logical, shows or hides the plot title for the y-axis.
#' @param connect.lines Logical, if \code{TRUE} and plot has point-geoms with
#'   error bars (this is usually the case when the x-axis is discrete), points
#'   of same groups will be connected with a line.
#' @param one.plot Logical, if \code{TRUE} and \code{x} has a \code{panel} column
#'   (i.e. when four \code{terms} were used), a single, integrated plot is produced.
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @param ... Further arguments passed down to \code{ggplot::scale_y*()}, to
#'    control the appearance of the y-axis.
#' @param residuals.type Deprecated. Formally was the residual type. Now is always \code{"working"}.
#'
#' @inheritParams get_title
#'
#' @inheritSection residualize_over_grid Partial Residuals
#'
#' @return A ggplot2-object.
#'
#' @note Load \code{library(ggplot2)} and use \code{theme_set(theme_ggeffects())}
#'   to set the \pkg{ggeffects}-theme as default plotting theme. You can then use
#'   further plot-modifiers from \pkg{sjPlot}, like \code{legend_style()} or
#'   \code{font_size()} without losing the theme-modifications.
#'   \cr \cr
#'   There are pre-defined colour palettes in this package. Use
#'   \code{show_pals()} to show all available colour palettes.
#'
#' @details For proportional odds logistic regression (see \code{?MASS::polr})
#'   or cumulative link models in general, plots are automatically facetted
#'   by \code{response.level}, which indicates the grouping of predictions
#'   based on the level of the model's response.
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
#' \donttest{
#' # facet by group, use pre-defined color palette
#' dat <- ggpredict(fit, terms = c("c12hour", "c172code"))
#' plot(dat, facet = TRUE, colors = "hero")
#'
#' # don't use facets, b/w figure, w/o confidence bands
#' dat <- ggpredict(fit, terms = c("c12hour", "c172code"))
#' plot(dat, colors = "bw", ci = FALSE)
#'
#' # factor at x axis, plot exact data points and error bars
#' dat <- ggpredict(fit, terms = c("c172code", "c161sex"))
#' plot(dat)
#'
#' # for three variables, automatic facetting
#' dat <- ggpredict(fit, terms = c("c12hour", "c172code", "c161sex"))
#' plot(dat)}
#'
#' # show all color palettes
#' show_pals()
#'
#' @importFrom stats binomial poisson gaussian Gamma inverse.gaussian quasi quasibinomial quasipoisson
#' @export
plot.ggeffects <- function(x,
                           ci = TRUE,
                           ci.style = c("ribbon", "errorbar", "dash", "dot"),
                           facets,
                           add.data = FALSE,
                           limit.range = FALSE,
                           residuals = FALSE,
                           residuals.line = FALSE,
                           colors = "Set1",
                           alpha = .15,
                           dodge = .25,
                           use.theme = TRUE,
                           dot.alpha = .35,
                           jitter = .2,
                           log.y = FALSE,
                           case = NULL,
                           show.legend = TRUE,
                           show.title = TRUE,
                           show.x.title = TRUE,
                           show.y.title = TRUE,
                           dot.size = NULL,
                           line.size = NULL,
                           connect.lines = FALSE,
                           grid,
                           one.plot = TRUE,
                           rawdata,
                           residuals.type,
                           ...) {

  if (!requireNamespace("ggplot2", quietly = FALSE)) {
    stop("Package `ggplot2` needed to produce marginal effects plots. Please install it by typing `install.packages(\"ggplot2\", dependencies = TRUE)` into the console.", call. = FALSE)
  }

  if (!missing(residuals.type)) warning("'residuals.type' is deprecated. Using 'working' residuals.")

  # check alias
  if (missing(rawdata)) rawdata <- add.data

  # set some defaults

  jitter.miss <- missing(jitter)

  if (isTRUE(jitter))
    jitter <- .2
  else if (is.logical(jitter) && length(jitter) == 1L && !is.na(jitter) && !jitter)
    jitter <- NULL

  # make sure we have two values, one for horizontal and one for vertical jittering
  if (!is.null(jitter) && length(jitter) == 1 && is.numeric(jitter)) {
    jitter <- c(jitter, jitter)
  }

  y.breaks <- NULL
  y.limits <- NULL

  # is x a factor?
  xif <- attr(x, "x.is.factor", exact = TRUE)
  x_is_factor <- !is.null(xif) && xif == "1"

  if (is.null(dot.size)) dot.size <- 2
  if (is.null(line.size)) line.size <- .7

  if (!missing(grid)) facets <- grid
  if (missing(ci.style) && x_is_factor) ci.style <- "errorbar"
  ci.style <- match.arg(ci.style)

  add.args <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)
  if (!("breaks" %in% names(add.args)) && isTRUE(log.y)) {
    y.breaks <- unique(round(log2(pretty(c(min(x$conf.low), max(x$conf.high))))))
    y.breaks[is.nan(y.breaks)] <- NA
    y.breaks[is.infinite(y.breaks)] <- NA
    y.breaks <- 2^y.breaks[!is.na(y.breaks)]
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
  has_groups <- .obj_has_name(x, "group") && length(unique(x$group)) > 1
  has_facets <- .obj_has_name(x, "facet") && length(unique(x$facet)) > 1
  has_panel <- .obj_has_name(x, "panel") && length(unique(x$panel)) > 1


  # if we add data points, limit to range
  if (isTRUE(limit.range)) {
    raw_data <- attr(x, "rawdata", exact = TRUE)
    if (!is.null(raw_data)) {
      if (has_groups && has_facets) {
        ranges <- lapply(split(raw_data, list(raw_data$group, raw_data$facet)), function(i) range(i$x, na.rm = TRUE))
        for (i in unique(raw_data$group)) {
          for (j in unique(raw_data$facet)) {
            if (any(is.infinite(ranges[[paste0(i, ".", j)]]))) {
              remove <- x$group == i & x$facet == j
              x$x[remove] <- NA
            } else {
              remove <- x$group == i & x$facet == j & x$x < ranges[[paste0(i, ".", j)]][1]
              x$x[remove] <- NA
              remove <- x$group == i & x$facet == j & x$x > ranges[[paste0(i, ".", j)]][2]
              x$x[remove] <- NA
            }
          }
        }
      } else if (has_groups) {
        ranges <- lapply(split(raw_data, raw_data$group), function(i) range(i$x, na.rm = TRUE))
        for (i in names(ranges)) {
          remove <- x$group == i & x$x < ranges[[i]][1]
          x$x[remove] <- NA
          remove <- x$group == i & x$x > ranges[[i]][2]
          x$x[remove] <- NA
        }
      } else {
        remove <- x$x < min(raw_data$x, na.rm = TRUE) | x$x > max(raw_data$x, na.rm = TRUE)
        x$x[remove] <- NA
      }
    }
  }


  # partial residuals?
  if (residuals) {
    obj_name <- attr(x, "model.name", exact = TRUE)
    model <- NULL
    if (!is.null(obj_name)) {
      model <- tryCatch({
        get(obj_name, envir = parent.frame())
      }, error = function(e) {
        NULL
      })
      if (is.null(model)) {
        model <- tryCatch({
          get(obj_name, envir = globalenv())
        }, error = function(e) {
          NULL
        })
      }
    }

    if (!is.null(model)) {
      residual_data <- residualize_over_grid(grid = x, model = model)
      attr(x, "residual_data") <- residual_data

      ## TODO for now, we allow no continuous grouping varialbles for partial residuals
      # it is difficult to match "raw data" values with the specific at-values
      # for continuous variables

      attr(x, "continuous.group") <- FALSE
    } else {
      warning("Could not find model object to extract residuals.", call. = FALSE)
      residals <- FALSE
    }
  }

  # convert x back to numeric
  if (!is.numeric(x$x)) {
    if (x_is_factor && !.is_numeric_factor(x$x)) {
      levels(x$x) <- seq_len(nlevels(x$x))
    }
    x$x <- .factor_to_numeric(x$x)
  }

  # special solution for polr
  facet_polr <- FALSE
  if (.obj_has_name(x, "response.level") && length(unique(x$response.level)) > 1) {
    has_facets <- TRUE
    facet_polr <- TRUE
  }

  # remember if we have a b/w plot
  is_black_white <- colors[1] == "bw"

  # set default, if argument not specified
  if (has_facets)
    facets <- TRUE
  else if (missing(facets) || is.null(facets))
    facets <- has_facets

  # facets, but only groups? here the user wants to
  # plot facets for the grouping variable
  facets_grp <- facets && !has_facets

  # set CI to false if we don't have SE and CI
  if ("conf.low" %in% names(which(colSums(is.na(x)) == nrow(x))) || !.obj_has_name(x, "conf.low"))
    ci <- FALSE


  # if we have a numeric variable as facet, also add variable name for more
  # intuitive labelling
  if (facets) {
    if (is.numeric(x$facet) || isTRUE(attr(x, "numeric.facet", exact = TRUE))) {
      x$facet <- sprintf(
        "%s = %g",
        attr(x, "terms", exact = TRUE)[3],
        .factor_to_numeric(x$facet)
      )
    }
  }


  if (!has_panel) one.plot <- FALSE

  if (one.plot && !requireNamespace("see", quietly = TRUE)) {
    warning("Package `see` needed to plot multiple panels in one integrated figure. Please install it by typing `install.packages(\"see\", dependencies = TRUE)` into the console.", call. = FALSE)
    one.plot <- FALSE
  }


  if (has_panel) {
    panels <- unique(x$panel)
    p <- lapply(1:length(panels), function(.i) {
      .p <- panels[.i]

      attr(x, "panel.title") <- sprintf(
        "%s = %s",
        attr(x, "terms", exact = TRUE)[4],
        as.character(.p)
      )

      if (one.plot && .i < length(panels)) {
        show_l <- FALSE
      } else {
        show_l <- show.legend
      }

      pl <- plot_panel(
        x = x[x$panel == .p, , drop = FALSE],
        colors = colors,
        has_groups = has_groups,
        facets_grp = facets_grp,
        facets = facets,
        facet_polr = facet_polr,
        is_black_white = is_black_white,
        x_is_factor = x_is_factor,
        alpha = alpha,
        dot.alpha = dot.alpha,
        dodge = dodge,
        ci = ci,
        ci.style = ci.style,
        dot.size = dot.size,
        line.size = line.size,
        connect.lines = connect.lines,
        case = case,
        jitter = jitter,
        jitter.miss = jitter.miss,
        rawdata = rawdata,
        residuals = residuals,
        residuals.line = residuals.line,
        show.title = show.title,
        show.x.title = show.x.title,
        show.y.title = show.y.title,
        show.legend = show_l,
        log.y = log.y,
        y.breaks = y.breaks,
        y.limits = y.limits,
        use.theme = use.theme,
        ...
      )

      if (one.plot) {
        if (.i < length(panels)) {
          pl <- pl + ggplot2::labs(x = NULL)
        }

        if (.i > 1) {
          pl <- pl + ggplot2::labs(title = NULL)
        }
      }


      pl
    })
  } else {
    p <- plot_panel(
      x = x,
      colors = colors,
      has_groups = has_groups,
      facets_grp = facets_grp,
      facets = facets,
      facet_polr = facet_polr,
      is_black_white = is_black_white,
      x_is_factor = x_is_factor,
      alpha = alpha,
      dot.alpha = dot.alpha,
      dodge = dodge,
      ci = ci,
      ci.style = ci.style,
      dot.size = dot.size,
      line.size = line.size,
      connect.lines = connect.lines,
      case = case,
      jitter = jitter,
      jitter.miss = jitter.miss,
      rawdata = rawdata,
      residuals = residuals,
      residuals.line = residuals.line,
      show.title = show.title,
      show.x.title = show.x.title,
      show.y.title = show.y.title,
      show.legend = show.legend,
      log.y = log.y,
      y.breaks = y.breaks,
      y.limits = y.limits,
      use.theme = use.theme,
      ...
    )
  }


  if (has_panel && one.plot && requireNamespace("see", quietly = TRUE)) {
    do.call(see::plots, p)
  } else {
    p
  }
}


#' @importFrom stats na.omit
plot_panel <- function(x,
                       colors,
                       has_groups,
                       facets_grp,
                       facets,
                       facet_polr,
                       is_black_white,
                       x_is_factor,
                       alpha,
                       dot.alpha,
                       dodge,
                       ci,
                       ci.style,
                       dot.size,
                       line.size,
                       connect.lines,
                       case,
                       jitter,
                       jitter.miss,
                       rawdata,
                       residuals,
                       residuals.line,
                       show.title,
                       show.x.title,
                       show.y.title,
                       show.legend,
                       log.y,
                       y.breaks,
                       y.limits,
                       use.theme,
                       ...) {

  if (.obj_has_name(x, "group") && is.character(x$group)) x$group <- factor(x$group, levels = unique(x$group))
  if (.obj_has_name(x, "facet") && is.character(x$facet)) x$facet <- factor(x$facet, levels = unique(x$facet))
  if (.obj_has_name(x, "response.level") && is.character(x$response.level)) x$response.level <- ordered(x$response.level, levels = unique(x$response.level))

  if (rawdata & isTRUE(attr(x, "continuous.group"))) {
    x$group_col <- as.numeric(as.character(x$group))
  } else {
    x$group_col <- x$group
  }

  # base plot, set mappings -----

  plot_data <- x[!is.na(x$x), ]

  if (has_groups && !facets_grp && is_black_white && x_is_factor)
    p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = "x", y = "predicted", colour = "group_col", fill = "group_col", shape = "group"))
  else if (has_groups && !facets_grp && is_black_white && !x_is_factor)
    p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = "x", y = "predicted", colour = "group_col", fill = "group_col", linetype = "group"))
  else if (has_groups && !facets_grp && colors[1] == "gs" && x_is_factor)
    p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = "x", y = "predicted", colour = "group_col", fill = "group_col", shape = "group"))
  else if (has_groups && colors[1] != "bw")
    p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = "x", y = "predicted", colour = "group_col", fill = "group_col"))
  else
    p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = "x", y = "predicted"))


  # get color values -----

  colors <- .get_colors(colors, length(unique(stats::na.omit(x$group))), isTRUE(attr(x, "continuous.group")))


  # plot raw data points -----

  # get raw data
  rawdat <- attr(x, "rawdata", exact = TRUE)
  if (rawdata) {
    p <- .add_raw_data_to_plot(p, x, rawdat, ci.style, dot.alpha, dot.size, dodge, jitter, jitter.miss, colors)
  }


  # plot partial residuals -----

  # get residual data
  residual_data <- attr(x, "residual_data", exact = TRUE)
  if (isTRUE(residuals)) {
    p <- .add_residuals_to_plot(p, x, residual_data, residuals.line, ci.style, line.size, dot.alpha, dot.size, dodge, jitter, colors)
  }


  # now plot the geom. we use a smoother for a continuous x, and
  # a point-geom, if x was a factor. In this case, the x-value is still
  # numeric, but we need to plot exact data points between categories
  # and no smoothing across all x-values

  if (x_is_factor) {
    # for x as factor
    p <- p + ggplot2::geom_point(
      position = ggplot2::position_dodge(width = dodge),
      size = dot.size
    )
  } else {
    # classical line
    p <- p + ggplot2::geom_line(size = line.size, ggplot2::aes_string(group = "group"))
  }

  # connect dots with lines...
  if (x_is_factor && connect.lines) {
    p <- p + ggplot2::geom_line(
      size = line.size,
      position = ggplot2::position_dodge(width = dodge)
    )
  }


  # CI ----

  if (ci) {

    # for a factor on x-axis, use error bars

    if (x_is_factor) {

      if (ci.style == "errorbar") {
        p <- p + ggplot2::geom_errorbar(
          ggplot2::aes_string(ymin = "conf.low", ymax = "conf.high"),
          position = ggplot2::position_dodge(width = dodge),
          width = .1,
          size = line.size
        )
      } else {
        lt <- switch(
          ci.style,
          dash = 2,
          dot = 3,
          2
        )

        p <- p + ggplot2::geom_errorbar(
          ggplot2::aes_string(ymin = "conf.low", ymax = "conf.high", linetype = NULL),
          position = ggplot2::position_dodge(width = dodge),
          width = .1,
          linetype = lt,
          size = line.size
        )
      }

    } else {

      if (ci.style == "ribbon") {
        # for continuous x, use ribbons by default
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes_string(ymin = "conf.low", ymax = "conf.high", colour = NULL, linetype = NULL, shape = NULL, group = "group"),
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
    p <- p + ggplot2::scale_x_continuous(breaks = unique(plot_data$x), labels = x_lab)
  }


  # facets ----

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


  # set colors ----

  if (isTRUE(rawdata) && isTRUE(attr(x, "continuous.group"))) {
    p <- p +
      ggplot2::scale_color_gradientn(colors = colors, aesthetics = c("colour", "fill"), guide = "legend", breaks = as.numeric(levels(x$group)), limits = range(c(rawdat$group_col, x$group_col)))
  } else {
    p <- p +
      ggplot2::scale_color_manual(values = colors, aesthetics = c("colour", "fill"))
  }


  # show/hide titles ----

  if (!show.title) attr(x, "title") <- NULL
  if (!show.title) attr(x, "n.trials") <- NULL
  if (!show.x.title) attr(x, "x.title") <- NULL
  if (!show.y.title) attr(x, "y.title") <- NULL


  # set axis titles ----

  p <- p + ggplot2::labs(
    title = get_title(x, case),
    x = get_x_title(x, case),
    y = get_y_title(x, case),
    fill = NULL,
    subtitle = get_sub_title(x)
  )

  if (has_groups && show.legend)
    p <- p + ggplot2::labs(
      colour = get_legend_title(x, case),
      linetype = get_legend_title(x, case),
      shape = get_legend_title(x, case)
    )


  # no legend for fill-aes ----

  p <- p + ggplot2::guides(fill = "none")

  if (is_black_white) {
    p <- p +
      ggplot2::guides(colour = "none") +
      ggplot2::labs(colour = NULL)
  }


  # show or hide legend -----

  if (!show.legend) {
    p <- p + ggplot2::labs(
      colour = NULL,
      linetype = NULL,
      shape = NULL
    ) + ggplot2::guides(colour = "none", linetype = "none", shape = "none")
  }


  # for binomial family, fix coord ----

  if (attr(x, "logistic", exact = TRUE) == "1" && attr(x, "is.trial", exact = TRUE) == "0") {
    if (log.y) {
      if (is.null(y.breaks))
        p <- p + ggplot2::scale_y_log10(labels = .percents, ...)
      else
        p <- p + ggplot2::scale_y_log10(labels = .percents, breaks = y.breaks, limits = y.limits, ...)
    } else {
      p <- p + ggplot2::scale_y_continuous(labels = .percents, ...)
    }
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




#' @importFrom graphics plot
#' @export
plot.ggalleffects <- function(x,
                              ci = TRUE,
                              ci.style = c("ribbon", "errorbar", "dash", "dot"),
                              facets,
                              add.data = FALSE,
                              colors = "Set1",
                              alpha = .15,
                              dodge = .25,
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
                              connect.lines = FALSE,
                              grid,
                              one.plot = TRUE,
                              rawdata,
                              ...) {

  if (!missing(grid)) facets <- grid
  if (missing(facets)) facets <- NULL

  # check alias
  if (missing(rawdata)) rawdata <- add.data

  if (isTRUE(facets)) {
    # merge all effect-data frames into one
    dat <- get_complete_df(x)

    rawdat <- suppressWarnings(
      do.call(rbind, lapply(x, function(d) {
        tmp <- attr(d, "rawdata")
        tmp$group <- d$group[1]
        tmp
      }))
    )

    # copy raw data
    attr(dat, "rawdata") <- rawdat

    # set various attributes
    attr(dat, "x.is.factor") <- attr(x[[1]], "x.is.factor", exact = TRUE)
    attr(dat, "family") <- attr(x[[1]], "family", exact = TRUE)
    attr(dat, "link") <- attr(x[[1]], "link", exact = TRUE)
    attr(dat, "logistic") <- attr(x[[1]], "logistic", exact = TRUE)
    attr(dat, "fitfun") <- attr(x[[1]], "fitfun", exact = TRUE)

    graphics::plot(
      x = dat,
      ci = ci,
      ci.style = ci.style,
      facets = TRUE,
      add.data = rawdata,
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
      connect.lines = connect.lines,
      ...
    )
  } else {
    lapply(x, function(.x) {
      graphics::plot(
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
    })
  }
}



#' @importFrom insight format_value
.percents <- function(x) {
  insight::format_value(x = x, as_percent = TRUE, digits = 0)
}





.add_raw_data_to_plot <- function(p, x, rawdat, ci.style, dot.alpha, dot.size, dodge, jitter, jitter.miss, colors) {

  if (!requireNamespace("ggplot2", quietly = FALSE)) {
    stop("Package `ggplot2` needed to produce marginal effects plots. Please install it by typing `install.packages(\"ggplot2\", dependencies = TRUE)` into the console.", call. = FALSE)
  }

  # we need an own aes for this
  # we plot rawdata first, so it doesn't overlay the
  # dots / lines for marginal effects

  if (!is.null(rawdat)) {
    # make sure response is numeric
    rawdat$response <- .factor_to_numeric(rawdat$response)

    # check if we have a group-variable with at least two groups
    if (.obj_has_name(rawdat, "group")) {

      if (isTRUE(attr(x, "continuous.group"))) {
        rawdat$group_col <- as.numeric(as.character(rawdat$group))
      } else {
        rawdat$group_col <- rawdat$group
      }

      rawdat$group <- as.factor(rawdat$group)
      # levels(rawdat$group) <- unique(x$group)
      grps <- .n_distinct(rawdat$group) > 1
    } else {
      grps <- FALSE
    }

    # check if we have only selected values for groups, in this case
    # filter raw data to match grouping colours
    if (grps && isFALSE(attr(x, "continuous.group")) && .n_distinct(rawdat$group) > .n_distinct(x$group)) {
      rawdat <- rawdat[which(rawdat$group %in% x$group), , drop = FALSE]
    }


    # if we have groups, add colour aes, to map raw data to
    # grouping variable

    if (grps)
      mp <- ggplot2::aes_string(x = "x", y = "response", colour = "group_col")
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
        inherit.aes = FALSE,
        shape = 16
      )
    } else {

      # no jitter
      if (is.null(jitter)) {
        jitter <- c(0, 0)
      }

      if (ci.style == "errorbar") {
        if (grps) {
          p <- p + ggplot2::geom_point(
            data = rawdat,
            mapping = ggplot2::aes_string(x = "x", y = "response", colour = "group_col"),
            alpha = dot.alpha,
            size = dot.size,
            position = ggplot2::position_jitterdodge(
              jitter.width = jitter[1],
              jitter.height = jitter[2],
              dodge.width = dodge
            ),
            show.legend = FALSE,
            inherit.aes = FALSE,
            shape = 16
          )
        } else {
          p <- p + ggplot2::geom_point(
            data = rawdat,
            mapping = ggplot2::aes_string(x = "x", y = "response", fill = "group_col"),
            alpha = dot.alpha,
            size = dot.size,
            position = ggplot2::position_jitterdodge(
              jitter.width = jitter[1],
              jitter.height = jitter[2],
              dodge.width = dodge
            ),
            show.legend = FALSE,
            inherit.aes = FALSE,
            shape = 16,
            color = colors[1]
          )
        }
      } else {
        p <- p + ggplot2::geom_jitter(
          data = rawdat,
          mapping = mp,
          alpha = dot.alpha,
          size = dot.size,
          width = jitter[1],
          height = jitter[2],
          show.legend = FALSE,
          inherit.aes = FALSE,
          shape = 16
        )
      }
    }
  } else {
    message("Raw data not available.")
  }

  p
}




.add_residuals_to_plot <- function(p, x, residuals, residuals.line, ci.style, line.size, dot.alpha, dot.size, dodge, jitter, colors) {
  if (!requireNamespace("ggplot2", quietly = FALSE)) {
    stop("Package `ggplot2` needed to produce marginal effects plots. Please install it by typing `install.packages(\"ggplot2\", dependencies = TRUE)` into the console.", call. = FALSE)
  }

  if (!is.null(residuals)) {

    # make sure x on x-axis is on same scale
    if (is.numeric(x$x) && !is.numeric(residuals$x)) {
      residuals$x <- .factor_to_numeric(residuals$x)
    }

    residuals$facet <- NULL
    residuals$panel <- NULL


    # check if we have a group-variable with at least two groups
    if (.obj_has_name(residuals, "group")) {

      if (isTRUE(attr(x, "continuous.group")) && is.numeric(x$group)) {
        residuals$group_col <- as.numeric(as.character(residuals$group))
      } else {
        residuals$group_col <- as.factor(residuals$group)
      }

      residuals$group <- as.factor(residuals$group)
      grps <- .n_distinct(residuals$group) > 1
    } else {
      grps <- FALSE
    }

    # check if we have only selected values for groups, in this case
    # filter raw data to match grouping colours
    if (grps && isFALSE(attr(x, "continuous.group")) && .n_distinct(residuals$group) > .n_distinct(x$group)) {
      residuals <- residuals[which(residuals$group %in% x$group), , drop = FALSE]
    }


    # if we have groups, add colour aes, to map raw data to
    # grouping variable

    if (grps)
      mp <- ggplot2::aes_string(x = "x", y = "predicted", colour = "group_col")
    else
      mp <- ggplot2::aes_string(x = "x", y = "predicted")



    # if ("group" %in% colnames(residuals)) {
    #   if (isTRUE(attr(x, "continuous.group"))) {
    #     residuals$group_col <- as.numeric(as.character(residuals$group))
    #   } else {
    #     residuals$group_col <- residuals$group
    #   }
    #   residuals$group <- as.factor(residuals$group)
    #   mp <- ggplot2::aes_string(x = "x", y = "predicted", colour = "group_col")
    # } else {
    #   mp <- ggplot2::aes_string(x = "x", y = "predicted")
    # }



    if (is.null(jitter)) {
      p <- p + ggplot2::geom_point(
        data = residuals,
        mapping = mp,
        alpha = dot.alpha,
        size = dot.size,
        show.legend = FALSE,
        inherit.aes = FALSE,
        shape = 16
      )
    } else {
      p <- p + ggplot2::geom_jitter(
        data = residuals,
        mapping = mp,
        alpha = dot.alpha,
        size = dot.size,
        width = jitter[1],
        height = jitter[2],
        show.legend = FALSE,
        inherit.aes = FALSE,
        shape = 16
      )
    }

    if (isTRUE(residuals.line)) {
      p <- p + ggplot2::geom_smooth(
        data = residuals,
        mapping = mp,
        method = "loess",
        inherit.aes = FALSE,
        size = line.size,
        se = FALSE
      )
    }

  } else {
    message("Partial residuals not available.")
  }

  p
}
