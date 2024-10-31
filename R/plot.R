#' @title Plot ggeffects-objects
#' @name plot
#'
#' @description `plot` is a generic plot-method for `ggeffects`-objects.
#' `ggeffects_palette()` returns `show_palettes()`
#'
#' @param x An object of class `ggeffects`, as returned by the functions
#'   from this package.
#' @param show_ci Logical, if `TRUE`, confidence bands (for continuous variables
#'   at x-axis) resp. error bars (for factors at x-axis) are plotted.
#' @param ci_style Character vector, indicating the style of the confidence
#'   bands. May be either `"ribbon"`, `"errorbar"`, `"dash"` or `"dot"`, to plot
#'   a ribbon, error bars, or dashed or dotted lines as confidence bands.
#' @param facets,grid Logical, defaults to `TRUE` if `x` has a column named
#'   `facet`, and defaults to `FALSE` if `x` has no such column. Set
#'   `facets = TRUE` to wrap the plot into facets even for grouping variables
#'   (see 'Examples'). `grid` is an alias for `facets`.
#' @param n_rows Number of rows to align plots. By default, all plots are aligned
#'   in one row. For facets, or multiple panels, plots can also be aligned in
#'   multiiple rows, to avoid that plots are too small.
#' @param show_data Logical, if `TRUE`, a layer with raw data from response
#'   by predictor on the x-axis, plotted as point-geoms, is added to the plot.
#'   Note that if the model has a transformed response variable, and the
#'   predicted values are *not* back-transformed (i.e. if `back_transform = FALSE`),
#'   the raw data points are plotted on the transformed scale, i.e. same scale
#'   as the predictions.
#' @param data_labels Logical, if `TRUE` and row names in data are available,
#'   data points will be labelled by their related row name.
#' @param limit_range Logical, if `TRUE`, limits the range of the prediction
#'   bands to the range of the data.
#' @param show_residuals Logical, if `TRUE`, a layer with partial residuals is
#'   added to the plot. See vignette
#'   [Effect Displays with Partial Residuals](https://cran.r-project.org/package=effects).
#'   from **effects** for more details on partial residual plots.
#' @param show_residuals_line Logical, if `TRUE`, a loess-fit line is added to the
#'   partial residuals plot. Only applies if `residuals` is `TRUE`.
#' @param collapse_group For mixed effects models, name of the grouping variable
#'   of random effects. If `collapse_group = TRUE`, data points "collapsed"
#'   by the first random effect groups are added to the plot. Else, if
#'   `collapse_group` is a name of a group factor, data is collapsed by
#'   that specific random effect. See [`collapse_by_group()`] for further
#'   details.
#' @param colors Character vector with color values in hex-format, valid
#'   color value names (see `demo("colors")`) or a name of a
#'   ggeffects-color-palette (see `ggeffects_palette()`).
#'
#'   Following options are valid for `colors`:
#'
#'   - If not specified, the color brewer palette `"Set1"` will be used.
#'   - If `"gs"`, a greyscale will be used.
#'   - If `"bw"`, the plot is black/white and uses different line types to
#'     distinguish groups.
#'   - There are some pre-defined color-palettes in this package that can be used,
#'     e.g. `colors = "metro"`. See [`show_palettes()`] to show all available palettes.
#'   - Else specify own color values or names as vector (e.g.
#'     `colors = c("#f00000", "#00ff00")`).
#' @param alpha Alpha value for the confidence bands.
#' @param line_size Numeric, size of the line geoms.
#' @param dot_size Numeric, size of the point geoms.
#' @param dodge Value for offsetting or shifting error bars, to avoid overlapping.
#'   Only applies, if a factor is plotted at the x-axis (in such cases, the
#'   confidence bands are replaced by error bars automatically), or if
#'   `ci_style = "errorbars"`.
#' @param use_theme Logical, if `TRUE`, a slightly tweaked version of ggplot's
#'   minimal-theme, `theme_ggeffects()`, is applied to the plot. If `FALSE`, no
#'   theme-modifications are applied.
#' @param dot_alpha Alpha value for data points, when `show_data = TRUE`.
#' @param jitter Numeric, between 0 and 1. If not `NULL` and `show_data = TRUE`,
#'   adds a small amount of random variation to the location of data points dots,
#'   to avoid overplotting. Hence the points don't reflect exact values in the
#'   data. May also be a numeric vector of length two, to add different
#'   horizontal and vertical jittering. For binary outcomes, raw data is not
#'   jittered by default to avoid that data points exceed the axis limits.
#' @param log_y Logical, if `TRUE`, the y-axis scale is log-transformed.
#'   This might be useful for binomial models with predicted probabilities on
#'   the y-axis.
#' @param show_legend Logical, shows or hides the plot legend.
#' @param show_title Logical, shows or hides the plot title-
#' @param show_x_title Logical, shows or hides the plot title for the x-axis.
#' @param show_y_title Logical, shows or hides the plot title for the y-axis.
#' @param connect_lines Logical, if `TRUE` and plot has point-geoms with
#'   error bars (this is usually the case when the x-axis is discrete), points
#'   of same groups will be connected with a line.
#' @param one_plot Logical, if `TRUE` and `x` has a `panel` column (i.e. when
#'   four `terms` were used), a single, integrated plot is produced.
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @param verbose Logical, toggle warnings and messages.
#' @param palette Name of a pre-defined color-palette as string. See
#'   `show_palettes()` to show all available palettes. Use `NULL` to return
#'   a list with names and color-codes of all avaibale palettes.
#' @param n Number of color-codes from the palette that should be returned.
#' @param ... Further arguments passed down to `ggplot::scale_y*()`, to
#'    control the appearance of the y-axis.
#'
#' @inheritParams get_title
#'
#' @inheritSection residualize_over_grid Partial Residuals
#'
#' @return A ggplot2-object.
#'
#' @note Load `library(ggplot2)` and use `theme_set(theme_ggeffects())` to set
#' the **ggeffects**-theme as default plotting theme. You can then use further
#' plot-modifiers, e.g. from **sjPlot**, like `legend_style()` or `font_size()`
#' without losing the theme-modifications.
#'
#' There are pre-defined colour palettes in this package. Use `show_palettes()`
#' to show all available colour palettes as plot, or
#' `ggeffects_palette(palette = NULL)` to show the color codes.
#'
#' @details For proportional odds logistic regression (see `?MASS::polr`)
#' or cumulative link models in general, plots are automatically facetted
#' by `response.level`, which indicates the grouping of predictions based on
#' the level of the model's response.
#'
#' @examplesIf requireNamespace("ggplot2") && requireNamespace("sjlabelled")
#' library(sjlabelled)
#' data(efc)
#' efc$c172code <- as_label(efc$c172code)
#' fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
#'
#' dat <- predict_response(fit, terms = "c12hour")
#' plot(dat)
#'
#' \donttest{
#' # facet by group, use pre-defined color palette
#' dat <- predict_response(fit, terms = c("c12hour", "c172code"))
#' plot(dat, facet = TRUE, colors = "hero")
#'
#' # don't use facets, b/w figure, w/o confidence bands
#' dat <- predict_response(fit, terms = c("c12hour", "c172code"))
#' plot(dat, colors = "bw", show_ci = FALSE)
#'
#' # factor at x axis, plot exact data points and error bars
#' dat <- predict_response(fit, terms = c("c172code", "c161sex"))
#' plot(dat)
#'
#' # for three variables, automatic facetting
#' dat <- predict_response(fit, terms = c("c12hour", "c172code", "c161sex"))
#' plot(dat)
#' }
#'
#' # show color codes of specific palette
#' ggeffects_palette("okabe-ito")
#'
#' # show all color palettes
#' show_palettes()
#' @export
plot.ggeffects <- function(x,
                           # uncertainty
                           show_ci = TRUE,
                           ci_style = c("ribbon", "errorbar", "dash", "dot"),
                           # data points
                           show_data = FALSE,
                           show_residuals = FALSE,
                           show_residuals_line = FALSE,
                           data_labels = FALSE,
                           limit_range = FALSE,
                           collapse_group = FALSE,
                           # annotations
                           show_legend = TRUE,
                           show_title = TRUE,
                           show_x_title = TRUE,
                           show_y_title = TRUE,
                           case = NULL,
                           # appearance colors and geoms
                           colors = NULL,
                           alpha = 0.15,
                           dot_alpha = 0.35,
                           jitter = NULL,
                           dodge = 0.25,
                           dot_size = NULL,
                           line_size = NULL,
                           # appearance theme and axis
                           use_theme = TRUE,
                           log_y = FALSE,
                           connect_lines = FALSE,
                           facets,
                           grid,
                           one_plot = TRUE,
                           n_rows = NULL,
                           verbose = TRUE,
                           ...) {
  insight::check_if_installed("ggplot2", reason = "to produce plots of adjusted predictions")

  # set some defaults for jittering
  jitter.miss <- missing(jitter)

  if (isTRUE(jitter)) {
    jitter <- 0.2
  } else if (isFALSE(jitter)) {
    jitter <- NULL
  }

  # make sure we have two values, one for horizontal and one for vertical jittering
  if (!is.null(jitter) && length(jitter) == 1 && is.numeric(jitter)) {
    jitter <- c(jitter, jitter)
  }

  y.breaks <- NULL
  y.limits <- NULL

  # is x a factor?
  xif <- attr(x, "x.is.factor", exact = TRUE)
  x_is_factor <- !is.null(xif) && xif == "1"

  # set default size for geoms
  if (is.null(dot_size)) dot_size <- 2
  if (is.null(line_size)) line_size <- 0.7

  if (!missing(grid)) facets <- grid
  if (missing(ci_style) && x_is_factor) ci_style <- "errorbar"
  ci_style <- match.arg(ci_style)

  # fix axis limits for log-y-scales
  add.args <- list(...)
  if (!("breaks" %in% names(add.args)) && isTRUE(log_y)) {
    y.breaks <- unique(round(log2(pretty(c(min(x$conf.low), max(x$conf.high))))))
    y.breaks[is.nan(y.breaks)] <- NA
    y.breaks[is.infinite(y.breaks)] <- NA
    y.breaks <- 2^y.breaks[!is.na(y.breaks)]
    y.limits <- c(min(y.breaks), max(y.breaks))

    # this is a REALLY sloppy hack to avoid that axis limits are not 0 for
    # log-scale, and that axis limits cover the range of the plotted geoms
    # I think there's a more elegant solution, so please let me know...

    while (y.limits[1] > min(x$conf.low) && y.limits[1] > 1e-5) {
      y.limits[1] <- y.limits[1] / 2
    }
    while (y.limits[2] < max(x$conf.high)) {
      y.limits[2] <- y.limits[2] * 2
    }
  }


  # do we have groups and facets?
  has_groups <- .obj_has_name(x, "group") && length(unique(x$group)) > 1
  has_facets <- .obj_has_name(x, "facet") && length(unique(x$facet)) > 1
  has_panel <- .obj_has_name(x, "panel") && length(unique(x$panel)) > 1

  # special case, for ordinal models where latent = TRUE
  latent_thresholds <- attr(x, "latent_thresholds", exact = TRUE)

  # if we add data points, limit to range
  if (isTRUE(limit_range)) {
    x <- .limit_x_range(x, has_groups, has_facets)
  }

  # partial residuals?
  if (show_residuals) {
    model <- .get_model_object(x)
    if (!is.null(model)) {
      residual_data <- residualize_over_grid(grid = x, model = model)
      attr(x, "residual_data") <- residual_data

      ## TODO for now, we allow no continuous grouping variables for partial residuals
      # it is difficult to match "raw data" values with the specific at-values
      # for continuous variables

      attr(x, "continuous.group") <- FALSE
    } else {
      if (verbose) {
        insight::format_alert("Could not find model object to extract residuals.")
      }
      show_residuals <- FALSE
    }
  }


  # collapse data by random effects?
  if (isTRUE(collapse_group) || (!is.null(collapse_group) && !isFALSE(collapse_group))) {
    if (isTRUE(collapse_group)) {
      # use first random effect
      collapse_group <- NULL
    }
    re_data <- collapse_by_group(
      x,
      model = .get_model_object(x),
      collapse_by = collapse_group,
      residuals = show_residuals
    )
    attr(x, "random_effects_data") <- re_data
    attr(x, "continuous.group") <- FALSE

    # no additional residuals or raw data
    show_data <- show_residuals <- FALSE
    attr(x, "residual_data") <- NULL
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
  is_black_white <- !is.null(colors) && colors[1] == "bw"

  # set default, if argument not specified
  if (has_facets) {
    facets <- TRUE
  } else if (missing(facets) || is.null(facets)) {
    facets <- has_facets
  }

  # facets, but only groups? here the user wants to
  # plot facets for the grouping variable
  facets_grp <- facets && !has_facets

  # set CI to false if we don't have SE and CI
  if ("conf.low" %in% names(which(colSums(is.na(x)) == nrow(x))) || !.obj_has_name(x, "conf.low")) {
    show_ci <- FALSE
  }


  # if we have a numeric variable as facet, also add variable name for more
  # intuitive labelling
  if (facets && (is.numeric(x$facet) || isTRUE(attr(x, "numeric.facet", exact = TRUE)))) {
    x$facet <- sprintf(
      "%s = %g",
      attr(x, "terms", exact = TRUE)[3],
      .factor_to_numeric(x$facet)
    )
  }

  # one integrated ("patchworked") plot only if we have multiple panels
  if (!has_panel) {
    one_plot <- FALSE
  }

  if (one_plot && !requireNamespace("see", quietly = TRUE)) {
    if (verbose) {
      insight::format_alert("Package {see} needed to plot multiple panels in one integrated figure. Please install it by typing `install.packages(\"see\", dependencies = TRUE)` into the console.")
    }
    one_plot <- FALSE
  }

  # prepare arguments for single and multiple plot
  plot_args <- list(
    colors = colors,
    has_groups = has_groups,
    facets_grp = facets_grp,
    facets = facets,
    facet_polr = facet_polr,
    is_black_white = is_black_white,
    x_is_factor = x_is_factor,
    alpha = alpha,
    dot_alpha = dot_alpha,
    dodge = dodge,
    show_ci = show_ci,
    ci_style = ci_style,
    dot_size = dot_size,
    line_size = line_size,
    connect_lines = connect_lines,
    case = case,
    jitter = jitter,
    jitter.miss = jitter.miss,
    show_data = show_data,
    label.data = data_labels,
    residuals = show_residuals,
    residuals.line = show_residuals_line,
    show_title = show_title,
    show_x_title = show_x_title,
    show_y_title = show_y_title,
    log_y = log_y,
    y.breaks = y.breaks,
    y.limits = y.limits,
    use_theme = use_theme,
    latent_thresholds = latent_thresholds,
    verbose = verbose
  )

  if (has_panel) {
    panels <- unique(x$panel)
    p <- lapply(seq_along(panels), function(.i) {
      .p <- panels[.i]

      attr(x, "panel.title") <- sprintf(
        "%s = %s",
        attr(x, "terms", exact = TRUE)[4],
        as.character(.p)
      )

      if (one_plot && .i < length(panels)) {
        show_temp_legend <- FALSE
      } else {
        show_temp_legend <- show_legend
      }

      plot_args$x <- x[x$panel == .p, , drop = FALSE]
      plot_args$show_legend <- show_temp_legend
      plot_args$n_rows <- NULL

      pl <- do.call(plot_panel, c(plot_args, list(...)))

      if (one_plot) {
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
    plot_args$x <- x
    plot_args$show_legend <- show_legend
    plot_args$n_rows <- n_rows

    p <- do.call(plot_panel, c(plot_args, list(...)))
  }


  if (has_panel && one_plot && requireNamespace("see", quietly = TRUE)) {
    do.call(see::plots, list(p, n_rows = n_rows))
  } else {
    p
  }
}


plot_panel <- function(x, colors, has_groups, facets_grp, facets, facet_polr,
                       is_black_white, x_is_factor, alpha, dot_alpha, dodge,
                       show_ci, ci_style, dot_size, line_size, connect_lines,
                       case, jitter, jitter.miss, show_data, label.data,
                       residuals, residuals.line, show_title, show_x_title,
                       show_y_title, show_legend, log_y, y.breaks, y.limits,
                       use_theme, n_rows = NULL, latent_thresholds, verbose = TRUE, ...) {
  # fake init
  .data <- NULL

  # for plotting, we need to convert groups/facets into factors
  if (.obj_has_name(x, "group") && is.character(x$group)) {
    x$group <- factor(x$group, levels = unique(x$group))
  }
  if (.obj_has_name(x, "facet") && is.character(x$facet)) {
    x$facet <- factor(x$facet, levels = unique(x$facet))
  }
  if (.obj_has_name(x, "response.level") && is.character(x$response.level)) {
    x$response.level <- ordered(x$response.level, levels = unique(x$response.level))
  }

  # when group variable is numeric (like mean +/- SD), we need to preserve
  # numeric values
  if (show_data && isTRUE(attr(x, "continuous.group"))) {
    x$group_col <- as.numeric(as.character(x$group))
  } else {
    x$group_col <- x$group
  }

  # base plot, set mappings -----

  plot_data <- x[!is.na(x$x), ]
  single_color <- FALSE

  aes_args <- list(
    x = str2lang("x"),
    y = str2lang("predicted"),
    colour = str2lang("group_col"),
    fill = str2lang("group_col")
  )

  if (has_groups && !facets_grp && is_black_white && x_is_factor) {
    # - we have more than one level/category for the x-axis
    # - x-axis has a categorical predictor
    # - black/white plot is requested, so we use different point shapes
    aes_args$shape <- str2lang("group")
  } else if (has_groups && !facets_grp && is_black_white && !x_is_factor) {
    # - we have more than one level/category (legend)
    # - x-axis is a numeric / continuous predictor
    # - black/white plot is requested, so we use different line types
    aes_args$linetype <- str2lang("group")
  } else if (has_groups && !facets_grp && !is.null(colors) && colors[1] == "gs" && x_is_factor) {
    # - we have more than one level/category (legend)
    # - x-axis is a numeric / continuous predictor
    # - grey scale plot is requested, so we use different shapes
    aes_args$shape <- str2lang("group")
  } else if (has_groups && (is.null(colors) || colors[1] != "bw")) {
    # - we have more than one level/category (legend)
    # - x-axis is either numeric or factor
    # - default color palette is used, so we don't need to map line types or shapes
  } else {
    # - no groups, so we have a single color plot w/o legend
    # - colors are hardcoded inside geom
    aes_args <- list(
      x = str2lang("x"),
      y = str2lang("predicted")
    )
    # we just have one color, so we set different colors inside geom, not as aes
    single_color <- TRUE
  }

  ggplot_aes <- do.call(ggplot2::aes, args = aes_args)
  p <- ggplot2::ggplot(plot_data, mapping = ggplot_aes)


  # get color values -----

  # we may have shortcuts are "colors", here we retrieve the actual color values
  colors <- .get_colors(
    colors,
    length(unique(stats::na.omit(x$group))),
    isTRUE(attr(x, "continuous.group"))
  )
  if (single_color && length(colors) > 1) {
    single_color <- FALSE
  }

  # plot raw data points -----

  # get raw data
  rawdat <- attr(x, "rawdata", exact = TRUE)
  if (show_data) {
    p <- .add_raw_data_to_plot(
      p, x, rawdat, label.data, ci_style, dot_alpha, dot_size, dodge, jitter,
      jitter.miss, colors, verbose = verbose
    )
  }


  # plot partial residuals -----

  # get residual data
  residual_data <- attr(x, "residual_data", exact = TRUE)
  if (isTRUE(residuals)) {
    p <- .add_residuals_to_plot(
      p, x, residual_data, residuals.line, ci_style, line_size, dot_alpha,
      dot_size, dodge, jitter, colors, x_is_factor, verbose = verbose
    )
  }


  # plot random effects group data -----

  # get re-group data
  random_effects_data <- attr(x, "random_effects_data", exact = TRUE)
  if (!is.null(random_effects_data)) {
    p <- .add_re_data_to_plot(
      p, x, random_effects_data, dot_alpha, dot_size, dodge, jitter,
      verbose = verbose
    )
  }


  # now plot the geom. we use a smoother for a continuous x, and
  # a point-geom, if x was a factor. In this case, the x-value is still
  # numeric, but we need to plot exact data points between categories
  # and no smoothing across all x-values

  # for x as factor
  if (x_is_factor) {
    plot_geom <- list(
      geom = "point",
      position = ggplot2::position_dodge(width = dodge),
      params = list(size = dot_size)
    )
    # when user provides a single color, we do not use the color-aes.
    # Thus, we need to specify the color directly as argument
    if (single_color) {
      plot_geom$params$colour <- colors
    }
    # classical line
  } else {
    plot_geom <- list(
      geom = "line",
      mapping = do.call(ggplot2::aes, list(group = str2lang("group"))),
      params = list(linewidth = line_size),
      position = "identity"
    )
    # when user provides a single color, we do not use the color-aes.
    # Thus, we need to specify the color directly as argument
    if (single_color) {
      plot_geom$params$colour <- colors
    }
  }
  # add layer
  plot_geom$stat <- "identity"
  p <- p + do.call(ggplot2::layer, plot_geom)


  # connect dots with lines...
  if (x_is_factor && connect_lines) {
    plot_geom <- list(
      geom = "line",
      stat = "identity",
      params = list(linewidth = line_size),
      position = ggplot2::position_dodge(width = dodge)
    )
    # when user provides a single color, we do not use the color-aes.
    # Thus, we need to specify the color directly as argument
    if (single_color) {
      plot_geom$params$colour <- colors
    }
    # add layer
    p <- p + do.call(ggplot2::layer, plot_geom)
  }


  # CI ----

  if (show_ci) {
    # we need to layers here
    plot_geom2 <- NULL

    # for a factor on x-axis, always use error bars
    if (x_is_factor) {
      plot_geom <- list(
        geom = "errorbar",
        stat = "identity",
        mapping = do.call(
          ggplot2::aes,
          list(ymin = str2lang("conf.low"), ymax = str2lang("conf.high"))
        ),
        params = list(width = 0, linewidth = line_size),
        position = ggplot2::position_dodge(width = dodge)
      )
      # when user provides a single color, we do not use the color-aes.
      # Thus, we need to specify the color directly as argument
      if (single_color) {
        plot_geom$params$colour <- colors
      }
      if (ci_style != "errorbar") {
        lt <- switch(ci_style,
          dash = 2,
          dot = 3,
          2
        )
        plot_geom$params$linetype <- lt
      }

      # for continuous x, use ribbons by default
    } else if (ci_style == "ribbon") {
      plot_geom <- list(
        geom = "ribbon",
        stat = "identity",
        position = "identity",
        mapping = do.call(
          ggplot2::aes,
          list(
            ymin = str2lang("conf.low"), ymax = str2lang("conf.high"),
            colour = NULL, linetype = NULL, shape = NULL, group = str2lang("group")
          )
        ),
        params = list(alpha = alpha)
      )
      # when user provides a single color, we do not use the color-aes.
      # Thus, we need to specify the color directly as argument
      if (single_color) {
        plot_geom$params$fill <- colors
      }

    } else if (ci_style == "errorbar") {
      plot_geom <- list(
        geom = "point",
        stat = "identity",
        params = list(size = dot_size),
        position = ggplot2::position_dodge(width = dodge)
      )
      plot_geom2 <- list(
        geom = "errorbar",
        stat = "identity",
        mapping = do.call(
          ggplot2::aes,
          list(ymin = str2lang("conf.low"), ymax = str2lang("conf.high"), shape = NULL)
        ),
        params = list(linewidth = line_size, width = 0),
        position = ggplot2::position_dodge(width = dodge)
      )
      # when user provides a single color, we do not use the color-aes.
      # Thus, we need to specify the color directly as argument
      if (single_color) {
        plot_geom$params$colour <- colors
        plot_geom2$params$colour <- colors
      }

    } else {
      lt <- switch(ci_style,
        dash = 2,
        dot = 3,
        2
      )
      plot_geom <- list(
        geom = "line",
        stat = "identity",
        position = "identity",
        mapping = do.call(
          ggplot2::aes,
          list(y = str2lang("conf.low"), linetype = NULL)
        ),
        params = list(linetype = lt)
      )
      plot_geom2 <- list(
        geom = "line",
        stat = "identity",
        position = "identity",
        mapping = do.call(
          ggplot2::aes,
          list(y = str2lang("conf.high"), linetype = NULL)
        ),
        params = list(linetype = lt)
      )
      # when user provides a single color, we do not use the color-aes.
      # Thus, we need to specify the color directly as argument
      if (single_color) {
        plot_geom$params$colour <- colors
        plot_geom2$params$colour <- colors
      }
    }
    # add layer(s)
    p <- p + do.call(ggplot2::layer, plot_geom)
    if (!is.null(plot_geom2)) {
      p <- p + do.call(ggplot2::layer, plot_geom2)
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
    p <- p + ggplot2::facet_wrap(~group, scales = "free_x", nrow = n_rows)
    # remove legends
    p <- p + ggplot2::guides(colour = "none", linetype = "none", shape = "none")
  } else if (facet_polr) {
    p <- p + ggplot2::facet_wrap(~response.level, scales = "free_x", nrow = n_rows)
  } else if (facets) {
    p <- p + ggplot2::facet_wrap(~facet, scales = "free_x", nrow = n_rows)
  }


  # add latent_thresholds ----

  if (!is.null(latent_thresholds)) {
    p <- p + ggplot2::geom_hline(
      yintercept = unname(latent_thresholds),
      linetype = "dotted",
      colour = "black",
      alpha = 0.3
    ) + ggplot2::annotate(
      geom = "text",
      x = 0.5,
      y = unname(latent_thresholds) + 0.2,
      label = names(latent_thresholds),
      alpha = 0.6
    )
  }


  # set colors ----

  if (isTRUE(show_data) && isTRUE(attr(x, "continuous.group"))) {
    p <- p +
      ggplot2::scale_color_gradientn(
        colors = colors,
        aesthetics = c("colour", "fill"),
        guide = "legend",
        breaks = as.numeric(levels(x$group)),
        limits = range(c(rawdat$group_col, x$group_col))
      )
  } else {
    p <- p +
      ggplot2::scale_color_manual(values = colors, aesthetics = c("colour", "fill"))
  }


  # show/hide titles ----

  if (!show_title) attr(x, "title") <- NULL
  if (!show_title) attr(x, "n.trials") <- NULL
  if (!show_x_title) attr(x, "x.title") <- NULL
  if (!show_y_title) attr(x, "y.title") <- NULL


  # set axis titles ----

  p <- p + ggplot2::labs(
    title = get_title(x, case),
    x = get_x_title(x, case),
    y = get_y_title(x, case),
    fill = NULL,
    subtitle = get_sub_title(x)
  )

  if (has_groups && show_legend)
    p <- p + ggplot2::labs(
      colour = get_legend_title(x, case),
      linetype = get_legend_title(x, case),
      shape = get_legend_title(x, case)
    )


  # no legend for fill-aes ----

  p <- p +
    ggplot2::guides(fill = "none", label = "none") +
    ggplot2::labs(label = NULL)

  if (is_black_white) {
    p <- p +
      ggplot2::guides(colour = "none", fill = "none", label = "none") +
      ggplot2::labs(colour = NULL, fill = NULL, label = NULL)
  }


  # show or hide legend -----

  if (!show_legend) {
    p <- p + ggplot2::labs(
      colour = NULL,
      linetype = NULL,
      shape = NULL,
      label = NULL
    ) + ggplot2::guides(colour = "none", linetype = "none", shape = "none", label = "none")
  }


  # for binomial family, fix coord ----

  if (attr(x, "logistic", exact = TRUE) == "1" && attr(x, "is.trial", exact = TRUE) == "0") {
    if (log_y) {
      if (is.null(y.breaks)) {
        p <- p + ggplot2::scale_y_log10(labels = .percents, ...)
      } else {
        p <- p + ggplot2::scale_y_log10(labels = .percents, breaks = y.breaks, limits = y.limits, ...)
      }
    } else {
      p <- p + ggplot2::scale_y_continuous(labels = .percents, ...)
    }
  } else if (log_y) {
    if (is.null(y.breaks)) {
      p <- p + ggplot2::scale_y_log10(...)
    } else {
      p <- p + ggplot2::scale_y_log10(breaks = y.breaks, limits = y.limits, ...)
    }
  } else {
    p <- p + ggplot2::scale_y_continuous(...)
  }

  # tweak theme
  if (use_theme) {
    p <- p + theme_ggeffects()
  }

  suppressWarnings(p)
}


#' @export
plot.ggalleffects <- function(x,
                              # uncertainty
                              show_ci = TRUE,
                              ci_style = c("ribbon", "errorbar", "dash", "dot"),
                              # data points
                              show_data = FALSE,
                              show_residuals = FALSE,
                              show_residuals_line = FALSE,
                              data_labels = FALSE,
                              limit_range = FALSE,
                              collapse_group = FALSE,
                              # annotations
                              show_legend = TRUE,
                              show_title = TRUE,
                              show_x_title = TRUE,
                              show_y_title = TRUE,
                              case = NULL,
                              # appearance colors and geoms
                              colors = NULL,
                              alpha = 0.15,
                              dot_alpha = 0.35,
                              jitter = NULL,
                              dodge = 0.25,
                              dot_size = NULL,
                              line_size = NULL,
                              # appearance theme and axis
                              use_theme = TRUE,
                              log_y = FALSE,
                              connect_lines = FALSE,
                              facets,
                              grid,
                              one_plot = TRUE,
                              verbose = TRUE,
                              ...) {

  if (!missing(grid)) facets <- grid
  if (missing(facets)) facets <- NULL

  ci_style <- match.arg(ci_style)

  # compose base arguments
  my_args <- list(
    show_ci = show_ci,
    ci_style = ci_style,
    facets = FALSE,
    show_data = show_data,
    data_labels = data_labels,
    limit_range = limit_range,
    show_residuals = show_residuals,
    show_residuals_line = show_residuals_line,
    collapse_group = collapse_group,
    colors = colors,
    alpha = alpha,
    dodge = dodge,
    use_theme = use_theme,
    dot_alpha = dot_alpha,
    jitter = jitter,
    log_y = log_y,
    case = case,
    show_legend = show_legend,
    show_title = show_title,
    show_x_title = show_x_title,
    show_y_title = show_y_title,
    dot_size = dot_size,
    line_size = line_size,
    connect_lines = connect_lines,
    one_plot = one_plot,
    verbose = verbose
  )
  my_args <- c(my_args, list(...))

  if (length(x) == 1) {
    x <- x[[1]]
    do.call(graphics::plot, c(list(x), my_args))
  } else if (isTRUE(facets)) {
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

    do.call(graphics::plot, c(list(x = dat), my_args))
  } else {
    lapply(x, function(.x) {
      do.call(graphics::plot, c(list(x = .x), my_args))
    })
  }
}


#' @export
plot.see_equivalence_test_ggeffects <- function(x,
                                                size_point = 0.7,
                                                rope_color = "#0171D3",
                                                rope_alpha = 0.2,
                                                show_intercept = FALSE,
                                                n_columns = 1,
                                                ...) {
  insight::check_if_installed("ggplot2")
  .data <- NULL

  .rope <- c(x$ROPE_low[1], x$ROPE_high[1])

  # check for user defined arguments

  fill.color <- c("#CD423F", "#018F77", "#FCDA3B")
  legend.title <- "Decision on H0"
  x.title <- NULL

  fill.color <- fill.color[sort(unique(match(x$ROPE_Equivalence, c("Accepted", "Rejected", "Undecided"))))]

  add.args <- match.call(expand.dots = FALSE)[["..."]]
  if ("colors" %in% names(add.args)) fill.color <- eval(add.args[["colors"]])
  if ("x.title" %in% names(add.args)) x.title <- eval(add.args[["x.title"]])
  if ("legend.title" %in% names(add.args)) legend.title <- eval(add.args[["legend.title"]])
  if ("labels" %in% names(add.args)) plot_labels <- eval(add.args[["labels"]])

  rope.line.alpha <- 1.25 * rope_alpha
  if (rope.line.alpha > 1) rope.line.alpha <- 1

  # make sure we have standardized column names for parameters and estimates
  parameter_columns <- attributes(x)$parameter_columns
  estimate_columns <- which(colnames(x) %in% c("Estimate", "Slope", "Predicted", "Contrast"))
  colnames(x)[estimate_columns[1]] <- "Estimate"

  if (length(parameter_columns) > 1) {
    x$Parameter <- unname(apply(x[parameter_columns], MARGIN = 1, toString))
  } else {
    x$Parameter <- x[[parameter_columns]]
  }

  p <- ggplot2::ggplot(
    x,
    ggplot2::aes(
      y = .data[["Parameter"]],
      x = .data[["Estimate"]],
      xmin = .data[["CI_low"]],
      xmax = .data[["CI_high"]],
      colour = .data[["ROPE_Equivalence"]]
    )
  ) +
    ggplot2::annotate(
      "rect",
      xmin = .rope[1],
      xmax = .rope[2],
      ymin = 0,
      ymax = Inf,
      fill = rope_color,
      alpha = (rope_alpha / 3)
    ) +
    ggplot2::geom_vline(
      xintercept = .rope,
      linetype = "dashed",
      colour = rope_color,
      linewidth = 0.8,
      alpha = rope.line.alpha
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      colour = rope_color,
      linewidth = 0.8,
      alpha = rope.line.alpha
    ) +
    ggplot2::geom_pointrange(size = size_point) +
    ggplot2::scale_colour_manual(values = fill.color) +
    ggplot2::labs(y = x.title, x = NULL, colour = legend.title) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_y_discrete()

  p
}


# helper ---------------------------------------------------------------------

#' @keywords internal
.percents <- function(x) {
  insight::format_value(x = x, as_percent = TRUE, digits = 0)
}


.limit_x_range <- function(x, has_groups, has_facets) {
  raw_data <- attr(x, "rawdata", exact = TRUE)
  if (!is.null(raw_data)) {
    if (has_groups && has_facets) {
      ranges <- lapply(
        split(raw_data, list(raw_data$group, raw_data$facet)),
        function(i) range(i$x, na.rm = TRUE)
      )
      for (i in unique(raw_data$group)) {
        for (j in unique(raw_data$facet)) {
          if (any(is.infinite(ranges[[paste0(i, ".", j)]]))) {
            remove_indices <- x$group == i & x$facet == j
            x$x[remove_indices] <- NA
          } else {
            remove_indices <- x$group == i & x$facet == j & x$x < ranges[[paste0(i, ".", j)]][1]
            x$x[remove_indices] <- NA
            remove_indices <- x$group == i & x$facet == j & x$x > ranges[[paste0(i, ".", j)]][2]
            x$x[remove_indices] <- NA
          }
        }
      }
    } else if (has_groups) {
      ranges <- lapply(
        split(raw_data, raw_data$group),
        function(i) range(i$x, na.rm = TRUE)
      )
      for (i in names(ranges)) {
        remove_indices <- x$group == i & x$x < ranges[[i]][1]
        x$x[remove_indices] <- NA
        remove_indices <- x$group == i & x$x > ranges[[i]][2]
        x$x[remove_indices] <- NA
      }
    } else {
      remove_indices <- x$x < min(raw_data$x, na.rm = TRUE) | x$x > max(raw_data$x, na.rm = TRUE)
      x$x[remove_indices] <- NA
    }
  }
  x
}


#' @keywords internal
.add_raw_data_to_plot <- function(p,
                                  x,
                                  rawdat,
                                  label.data,
                                  ci_style,
                                  dot_alpha,
                                  dot_size,
                                  dodge,
                                  jitter,
                                  jitter.miss,
                                  colors,
                                  verbose = TRUE) {
  insight::check_if_installed("ggplot2", reason = "to produce plots of adjusted predictions")
  .data <- NULL

  # we need an own aes for this
  # we plot rawdata first, so it doesn't overlay the
  # dots / lines for marginal effects

  if (!is.null(rawdat)) {
    # recode binary response to numeric? if so, make sure it starts with 0
    if (identical(attributes(x)$logistic, "1")) {
      lowest <- 0
    } else {
      lowest <- NULL
    }
    # make sure response is numeric
    rawdat$response <- .factor_to_numeric(rawdat$response, lowest = lowest)

    # transform response when offset is used, to match predictions
    offset_term <- attr(x, "offset", exact = TRUE)
    if (!is.null(offset_term)) {
      fixed_offset <- attributes(x)$condition
      if (offset_term %in% names(fixed_offset)) {
        fixed_value <- fixed_offset[[offset_term]]
        offset_values <- attributes(x)$offset_values
        rawdat$response <- (rawdat$response / offset_values) * fixed_value
      }
    }

    # check if we have a group-variable with at least two groups
    if (.obj_has_name(rawdat, "group")) {

      # we need to make sure that scale of raw data matches scale of predictions
      if (isTRUE(attr(x, "continuous.group"))) {
        rawdat$group_col <- as.numeric(as.character(rawdat$group))
      } else {
        rawdat$group_col <- rawdat$group
      }

      rawdat$group <- as.factor(rawdat$group)
      grps <- .n_distinct(rawdat$group) > 1
    } else {
      grps <- FALSE
    }

    # check if we have only selected values for groups, in this case
    # filter raw data to match grouping colours
    if (grps && isFALSE(attr(x, "continuous.group")) && .n_distinct(rawdat$group) > .n_distinct(x$group)) {
      rawdat <- rawdat[which(rawdat$group %in% x$group), , drop = FALSE]
    }

    aes_args <- list(
      x = str2lang("x"),
      y = str2lang("response")
    )
    # if we have groups, add colour aes, to map raw data to grouping variable
    if (grps) {
      aes_args$colour <- str2lang("group_col")
    } else if (ci_style == "errorbar") {
      aes_args$fill <- str2lang("group_col")
    }
    mp <- do.call(ggplot2::aes, aes_args)

    # no jitter? Tell user about overlap
    if ((is.null(jitter) || isTRUE(all(jitter == 0))) && verbose) {
      insight::format_alert("Data points may overlap. Use the `jitter` argument to add some amount of random variation to the location of data points and avoid overplotting.") # nolint
    }

    # base geom
    plot_geom <- list(
      geom = "point",
      stat = "identity",
      position = "identity",
      data = rawdat,
      mapping = mp,
      params = list(
        alpha = dot_alpha,
        size = dot_size,
        show.legend = FALSE,
        inherit.aes = FALSE,
        shape = 16
      )
    )

    # for binary response, no jittering by default
    if (attr(x, "logistic", exact = TRUE) != "1" || !jitter.miss || !is.null(jitter)) {
      # no jitter desired?
      if (is.null(jitter) || isTRUE(all(jitter == 0))) {
        jitter <- c(0, 0)
      }
      if (ci_style == "errorbar") {
        # if we have error bars, these are dodged, so we need to dodge the
        # data points as well
        plot_geom$position <- ggplot2::position_jitterdodge(
          jitter.width = jitter[1],
          jitter.height = jitter[2],
          dodge.width = dodge
        )
        if (!grps) {
          plot_geom$params$colour <- colors[1]
        }
      } else {
        # for ribbons, we have no dodged position, so just add
        # some jitter to the data points
        plot_geom$position <- ggplot2::position_jitter(
          width = jitter[1],
          height = jitter[2]
        )
      }
    }
    # add layer(s)
    p <- p + do.call(ggplot2::layer, plot_geom)

    if (label.data) {
      if (grps) {
        mp2 <- ggplot2::aes(
          x = .data[["x"]],
          y = .data[["response"]],
          label = .data[["rowname"]],
          colour = .data[["group_col"]]
        )
      } else {
        mp2 <- ggplot2::aes(
          x = .data[["x"]],
          y = .data[["response"]],
          label = .data[["rowname"]]
        )
      }
      if (insight::check_if_installed("ggrepel", quietly = TRUE)) {
        p <- p + ggrepel::geom_text_repel(
          data = rawdat,
          mapping = mp2,
          alpha = dot_alpha,
          show.legend = FALSE,
          inherit.aes = FALSE
        )
      } else {
        p <- p + ggplot2::geom_text(
          data = rawdat,
          mapping = mp2,
          alpha = dot_alpha,
          show.legend = FALSE,
          inherit.aes = FALSE
        )
      }
    }
  } else if (verbose) {
    message("Raw data not available.")
  }

  p
}


#' @keywords internal
.add_residuals_to_plot <- function(p,
                                   x,
                                   residuals,
                                   residuals.line,
                                   ci_style,
                                   line_size,
                                   dot_alpha,
                                   dot_size,
                                   dodge,
                                   jitter,
                                   colors,
                                   x_is_factor,
                                   verbose = TRUE) {
  insight::check_if_installed("ggplot2", reason = "to produce plots of adjusted predictions")
  .data <- NULL

  if (!is.null(residuals)) {

    # if we have a categorical x, we may need to reorder values, e.g. if we
    # have a reference level that results in non-alphabetical order of levels, see #288
    if (x_is_factor) {
      xlab <- attributes(x)$x.axis.labels
      # check if labels of original data is also present for residuals, and if
      # labels are not sorted - then resort x-values of residuals
      if (!is.null(xlab) && all(xlab %in% residuals$x) && is.unsorted(xlab)) {
        residuals$x <- datawizard::recode_values(
          residuals$x,
          recode = as.list(stats::setNames(xlab, sort(xlab)))
        )
      }
    }

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
    if (grps) {
      mp <- ggplot2::aes(x = .data[["x"]], y = .data[["predicted"]], colour = .data[["group_col"]])
    } else {
      mp <- ggplot2::aes(x = .data[["x"]], y = .data[["predicted"]])
    }

    # if ("group" %in% colnames(residuals)) {
    #   if (isTRUE(attr(x, "continuous.group"))) {
    #     residuals$group_col <- as.numeric(as.character(residuals$group))
    #   } else {
    #     residuals$group_col <- residuals$group
    #   }
    #   residuals$group <- as.factor(residuals$group)
    #   mp <- ggplot2::aes(x = .data[["x, y = .data[["predicted, colour = .data[["group_col)
    # } else {
    #   mp <- ggplot2::aes(x = .data[["x, y = .data[["predicted)
    # }

    if (is.null(jitter)) {
      p <- p + ggplot2::geom_point(
        data = residuals,
        mapping = mp,
        alpha = dot_alpha,
        size = dot_size,
        show.legend = FALSE,
        inherit.aes = FALSE,
        shape = 16
      )
      if (verbose) {
        insight::format_alert("Data points may overlap. Use the `jitter` argument to add some amount of random variation to the location of data points and avoid overplotting.")
      }
    } else {
      p <- p + ggplot2::geom_jitter(
        data = residuals,
        mapping = mp,
        alpha = dot_alpha,
        size = dot_size,
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
        size = line_size,
        se = FALSE
      )
    }

  } else if (verbose) {
    message("Partial residuals not available.")
  }

  p
}


#' @keywords internal
.add_re_data_to_plot <- function(p,
                                 x,
                                 random_effects_data,
                                 dot_alpha,
                                 dot_size,
                                 dodge,
                                 jitter,
                                 verbose = TRUE) {
  insight::check_if_installed("ggplot2", reason = "to produce plots of adjusted predictions")
  .data <- NULL

  # make sure x on x-axis is on same scale
  if (is.numeric(x$x) && !is.numeric(random_effects_data$x)) {
    random_effects_data$x <- .factor_to_numeric(random_effects_data$x)
  }

  # make sure group_col from legend is on same scale
  if (!is.null(x$group_col) && is.factor(x$group_col) && !is.factor(random_effects_data$group_col)) {
    random_effects_data$group_col <- as.factor(random_effects_data$group_col)
  }

  if ("response" %in% names(random_effects_data)) {
    mp <- ggplot2::aes(x = .data[["x"]], y = .data[["response"]], colour = .data[["group_col"]])
  } else {
    mp <- ggplot2::aes(x = .data[["x"]], y = .data[["predicted"]], colour = .data[["group_col"]])
  }

  if (is.null(jitter)) {
    p <- p + ggplot2::geom_point(
      data = random_effects_data,
      mapping = mp,
      alpha = dot_alpha,
      size = dot_size,
      position = ggplot2::position_dodge(width = dodge),
      show.legend = FALSE,
      inherit.aes = FALSE,
      shape = 16
    )
    if (verbose) {
      insight::format_alert("Data points may overlap. Use the `jitter` argument to add some amount of random variation to the location of data points and avoid overplotting.") # nolint
    }
  } else {
    p <- p + ggplot2::geom_point(
      data = random_effects_data,
      mapping = mp,
      alpha = dot_alpha,
      size = dot_size,
      position = ggplot2::position_jitterdodge(
        jitter.width = jitter[1],
        jitter.height = jitter[2],
        dodge.width = dodge
      ),
      show.legend = FALSE,
      inherit.aes = FALSE,
      shape = 16
    )
  }

  p
}


#' @keywords internal
.get_model_object <- function(x = NULL, name = NULL) {
  if (!is.null(name)) {
    obj_name <- name
  } else {
    obj_name <- attr(x, "model.name", exact = TRUE)
  }
  .model_obj <- NULL
  if (!is.null(obj_name)) {
    obj <- str2lang(obj_name)
    .model_obj <- .safe(get(obj_name, envir = parent.frame()))
    if (is.null(.model_obj)) {
      .model_obj <- .safe(get(obj_name, envir = globalenv()))
    }
    if (is.null(.model_obj)) {
      .model_obj <- .safe(dynGet(obj_name, ifnotfound = NULL))
    }
    if (is.null(.model_obj)) {
      .model_obj <- .safe(.dynEval(obj, ifnotfound = NULL))
    }
    # we may have a list of models, which are accessed via "modellist$model"
    # or "modellist[["model"]]"
    if (is.null(.model_obj) && grepl("\\$|\\[", obj_name)) {
      .model_obj <- .safe(eval(obj))
      if (is.null(.model_obj)) {
        .model_obj <- .safe(.dynEval(obj, ifnotfound = NULL))
      }
    }
  }
  .model_obj
}


.deprecated_warning <- function(old, new) {
  insight::format_warning(paste0("Argument `", old, "` is deprecated and will be removed in the future. Please use `", new, "` instead.")) # nolint
}
