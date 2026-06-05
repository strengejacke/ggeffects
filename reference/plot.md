# Plot ggeffects-objects

`plot` is a generic plot-method for `ggeffects`-objects.
`ggeffects_palette()` returns `show_palettes()`

## Usage

``` r
# S3 method for class 'ggeffects'
plot(
  x,
  show_ci = TRUE,
  ci_style = c("ribbon", "errorbar", "dash", "dot"),
  show_data = FALSE,
  show_residuals = FALSE,
  show_residuals_line = FALSE,
  data_labels = FALSE,
  limit_range = FALSE,
  collapse_group = FALSE,
  show_legend = TRUE,
  show_title = TRUE,
  show_x_title = TRUE,
  show_y_title = TRUE,
  case = NULL,
  colors = NULL,
  alpha = 0.15,
  dot_size = NULL,
  dot_alpha = 0.35,
  dot_shape = NULL,
  line_size = NULL,
  jitter = NULL,
  dodge = 0.25,
  use_theme = TRUE,
  log_y = FALSE,
  connect_lines = FALSE,
  facets,
  grid,
  one_plot = TRUE,
  n_rows = NULL,
  verbose = TRUE,
  ...
)

theme_ggeffects(base_size = 11, base_family = "")

ggeffects_palette(palette = "metro", n = NULL)

show_palettes()
```

## Arguments

- x:

  An object of class `ggeffects`, as returned by the functions from this
  package.

- show_ci:

  Logical, if `TRUE`, confidence bands (for continuous variables at
  x-axis) resp. error bars (for factors at x-axis) are plotted.

- ci_style:

  Character vector, indicating the style of the confidence bands. May be
  either `"ribbon"`, `"errorbar"`, `"dash"` or `"dot"`, to plot a
  ribbon, error bars, or dashed or dotted lines as confidence bands.

- show_data:

  Logical, if `TRUE`, a layer with raw data from response by predictor
  on the x-axis, plotted as point-geoms, is added to the plot. Note that
  if the model has a transformed response variable, and the predicted
  values are *not* back-transformed (i.e. if `back_transform = FALSE`),
  the raw data points are plotted on the transformed scale, i.e. same
  scale as the predictions.

- show_residuals:

  Logical, if `TRUE`, a layer with partial residuals is added to the
  plot. See vignette [Effect Displays with Partial
  Residuals](https://cran.r-project.org/package=effects). from
  **effects** for more details on partial residual plots.

- show_residuals_line:

  Logical, if `TRUE`, a loess-fit line is added to the partial residuals
  plot. Only applies if `residuals` is `TRUE`.

- data_labels:

  Logical, if `TRUE` and row names in data are available, data points
  will be labelled by their related row name.

- limit_range:

  Logical, if `TRUE`, limits the range of the prediction bands to the
  range of the data.

- collapse_group:

  For mixed effects models, name of the grouping variable of random
  effects. If `collapse_group = TRUE`, data points "collapsed" by the
  first random effect groups are added to the plot. Else, if
  `collapse_group` is a name of a group factor, data is collapsed by
  that specific random effect. See
  [`collapse_by_group()`](https://strengejacke.github.io/ggeffects/reference/collapse_by_group.md)
  for further details.

- show_legend:

  Logical, shows or hides the plot legend.

- show_title:

  Logical, shows or hides the plot title-

- show_x_title:

  Logical, shows or hides the plot title for the x-axis.

- show_y_title:

  Logical, shows or hides the plot title for the y-axis.

- case:

  Desired target case. Labels will automatically converted into the
  specified character case. See
  [`?sjlabelled::convert_case`](https://strengejacke.github.io/sjlabelled/reference/convert_case.html)
  for more details on this argument.

- colors:

  Character vector with color values in hex-format, valid color value
  names (see `demo("colors")`) or a name of a ggeffects-color-palette
  (see `ggeffects_palette()`).

  Following options are valid for `colors`:

  - If not specified, the color brewer palette `"Set1"` will be used.

  - If `"gs"`, a greyscale will be used.

  - If `"bw"`, the plot is black/white and uses different line types to
    distinguish groups.

  - There are some pre-defined color-palettes in this package that can
    be used, e.g. `colors = "metro"`. See `show_palettes()` to show all
    available palettes.

  - Else specify own color values or names as vector (e.g.
    `colors = c("#f00000", "#00ff00")`).

- alpha:

  Alpha value for the confidence bands.

- dot_size:

  Numeric, size of the point geoms.

- dot_alpha:

  Alpha value for data points, when `show_data = TRUE`.

- dot_shape:

  Shape of data points, when `show_data = TRUE`.

- line_size:

  Numeric, size of the line geoms.

- jitter:

  Numeric, between 0 and 1. If not `NULL` and `show_data = TRUE`, adds a
  small amount of random variation to the location of data points dots,
  to avoid overplotting. Hence the points don't reflect exact values in
  the data. May also be a numeric vector of length two, to add different
  horizontal and vertical jittering. For binary outcomes, raw data is
  not jittered by default to avoid that data points exceed the axis
  limits.

- dodge:

  Value for offsetting or shifting error bars, to avoid overlapping.
  Only applies, if a factor is plotted at the x-axis (in such cases, the
  confidence bands are replaced by error bars automatically), or if
  `ci_style = "errorbars"`.

- use_theme:

  Logical, if `TRUE`, a slightly tweaked version of ggplot's
  minimal-theme, `theme_ggeffects()`, is applied to the plot. If
  `FALSE`, no theme-modifications are applied.

- log_y:

  Logical, if `TRUE`, the y-axis scale is log-transformed. This might be
  useful for binomial models with predicted probabilities on the y-axis.

- connect_lines:

  Logical, if `TRUE` and plot has point-geoms with error bars (this is
  usually the case when the x-axis is discrete), points of same groups
  will be connected with a line.

- facets, grid:

  Logical, defaults to `TRUE` if `x` has a column named `facet`, and
  defaults to `FALSE` if `x` has no such column. Set `facets = TRUE` to
  wrap the plot into facets even for grouping variables (see
  'Examples'). `grid` is an alias for `facets`.

- one_plot:

  Logical, if `TRUE` and `x` has a `grid` column (i.e. when five `terms`
  were used), a single, integrated plot is produced.

- n_rows:

  Number of rows to align plots. By default, all plots are aligned in
  one row. For facets, or multiple panels, plots can also be aligned in
  multiiple rows, to avoid that plots are too small.

- verbose:

  Logical, toggle warnings and messages.

- ...:

  Further arguments passed down to `ggplot::scale_y*()`, to control the
  appearance of the y-axis.

- base_size:

  Base font size.

- base_family:

  Base font family.

- palette:

  Name of a pre-defined color-palette as string. See `show_palettes()`
  to show all available palettes. Use `NULL` to return a list with names
  and color-codes of all avaibale palettes.

- n:

  Number of color-codes from the palette that should be returned.

## Value

A ggplot2-object.

## Details

For proportional odds logistic regression (see
[`?MASS::polr`](https://rdrr.io/pkg/MASS/man/polr.html)) or cumulative
link models in general, plots are automatically facetted by
`response.level`, which indicates the grouping of predictions based on
the level of the model's response.

## Note

Load [`library(ggplot2)`](https://ggplot2.tidyverse.org) and use
`theme_set(theme_ggeffects())` to set the **ggeffects**-theme as default
plotting theme. You can then use further plot-modifiers, e.g. from
**sjPlot**, like `legend_style()` or `font_size()` without losing the
theme-modifications.

There are pre-defined colour palettes in this package. Use
`show_palettes()` to show all available colour palettes as plot, or
`ggeffects_palette(palette = NULL)` to show the color codes.

## Partial Residuals

For **generalized linear models** (glms), residualized scores are
computed as `inv.link(link(Y) + r)` where `Y` are the predicted values
on the response scale, and `r` are the *working* residuals.\
\
For (generalized) linear **mixed models**, the random effect are also
partialled out.

## Examples

``` r
library(sjlabelled)
#> 
#> Attaching package: ‘sjlabelled’
#> The following object is masked from ‘package:ggplot2’:
#> 
#>     as_label
#> The following objects are masked from ‘package:datawizard’:
#> 
#>     to_factor, to_numeric
data(efc)
efc$c172code <- as_label(efc$c172code)
fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

dat <- predict_response(fit, terms = "c12hour")
plot(dat)


# \donttest{
# facet by group, use pre-defined color palette
dat <- predict_response(fit, terms = c("c12hour", "c172code"))
plot(dat, facet = TRUE, colors = "hero")


# don't use facets, b/w figure, w/o confidence bands
dat <- predict_response(fit, terms = c("c12hour", "c172code"))
plot(dat, colors = "bw", show_ci = FALSE)
#> Ignoring unknown labels:
#> • shape : "carer's level of education"


# factor at x axis, plot exact data points and error bars
dat <- predict_response(fit, terms = c("c172code", "c161sex"))
plot(dat)
#> Ignoring unknown labels:
#> • linetype : "carer's gender"
#> • shape : "carer's gender"


# for three variables, automatic facetting
dat <- predict_response(fit, terms = c("c12hour", "c172code", "c161sex"))
plot(dat)

# }

# show color codes of specific palette
ggeffects_palette("okabe-ito")
#> [1] "#E69F00" "#56B4E9" "#009E73" "#CC79A7" "#F0E442" "#999999" "#000000"
#> [8] "#0072B2" "#D55E00"

# show all color palettes
show_palettes()
```
