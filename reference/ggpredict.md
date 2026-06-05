# Adjusted predictions from regression models

After fitting a model, it is useful generate model-based estimates
(expected values, or *adjusted predictions*) of the response variable
for different combinations of predictor values. Such estimates can be
used to make inferences about relationships between variables.

The **ggeffects** package computes marginal means and adjusted predicted
values for the response, at the margin of specific values or levels from
certain model terms. The package is built around three core functions:
[`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)
(understanding results),
[`test_predictions()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
(testing results for statistically significant differences) and
[`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
(communicate results).

By default, adjusted predictions or marginal means are by returned on
the *response* scale, which is the easiest and most intuitive scale to
interpret the results. There are other options for specific models as
well, e.g. with zero-inflation component (see documentation of the
`type`-argument). The result is returned as consistent data frame, which
is nicely printed by default.
[`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
can be used to easily create figures.

The main function to calculate marginal means and adjusted predictions
is
[`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md).
In previous versions of **ggeffects**, the functions `ggpredict()`,
`ggemmeans()`, `ggeffect()` and `ggaverage()` were used to calculate
marginal means and adjusted predictions. These functions are still
available, but
[`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)
as a "wrapper" around these functions is the preferred way to do this
now.

- `ggpredict()` calls
  [`get_predictions()`](https://strengejacke.github.io/ggeffects/reference/get_predictions.md)
  (which in turn calls
  [`stats::predict()`](https://rdrr.io/r/stats/predict.html))

- `ggemmeans()` calls
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)

- `ggaverage()` calls
  [`marginaleffects::avg_predictions()`](https://rdrr.io/pkg/marginaleffects/man/predictions.html)

- `ggeffect()` calls
  [`effects::Effect()`](https://rdrr.io/pkg/effects/man/effect.html)

## Usage

``` r
# S3 method for class 'ggeffects'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  stringsAsFactors = FALSE,
  terms_to_colnames = FALSE
)

ggaverage(
  model,
  terms,
  ci_level = 0.95,
  type = "fixed",
  typical = "mean",
  condition = NULL,
  back_transform = TRUE,
  vcov = NULL,
  vcov_args = NULL,
  weights = NULL,
  verbose = TRUE,
  ...
)

ggeffect(
  model,
  terms,
  ci_level = 0.95,
  bias_correction = FALSE,
  verbose = TRUE,
  ...
)

ggemmeans(
  model,
  terms,
  ci_level = 0.95,
  type = "fixed",
  typical = "mean",
  condition = NULL,
  interval = "confidence",
  back_transform = TRUE,
  vcov = NULL,
  vcov_args = NULL,
  bias_correction = FALSE,
  weights = NULL,
  verbose = TRUE,
  ...
)

ggpredict(
  model,
  terms,
  ci_level = 0.95,
  type = "fixed",
  typical = "mean",
  condition = NULL,
  interval = "confidence",
  back_transform = TRUE,
  vcov = NULL,
  vcov_args = NULL,
  bias_correction = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  An object of class `ggeffects`, as returned by
  [`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md),
  `ggpredict()`, `ggeffect()`, `ggaverage()` or `ggemmeans()`.

- row.names:

  `NULL` or a character vector giving the row names for the data frame.
  Missing values are not allowed.

- optional:

  logical. If `TRUE`, setting row names and converting column names (to
  syntactic names: see
  [`make.names`](https://rdrr.io/r/base/make.names.html)) is optional.
  Note that all of R's base package
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) methods
  use `optional` only for column names treatment, basically with the
  meaning of
  [`data.frame`](https://rdrr.io/r/base/data.frame.html)`(*, check.names = !optional)`.
  See also the `make.names` argument of the `matrix` method.

- ...:

  Arguments are passed down to `ggpredict()` (further down to
  [`predict()`](https://rdrr.io/r/stats/predict.html)) or `ggemmeans()`
  (and thereby to
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)),
  If `type = "simulate"`, `...` may also be used to set the number of
  simulation, e.g. `nsim = 500`. When calling `ggeffect()`, further
  arguments passed down to
  [`effects::Effect()`](https://rdrr.io/pkg/effects/man/effect.html).

- stringsAsFactors:

  logical: should the character vector be converted to a factor?

- terms_to_colnames:

  Logical, if `TRUE`, standardized column names (like `"x"`, `"group"`
  or `"facet"`) are replaced by the variable names of the focal
  predictors specified in `terms`.

- model:

  A model object, or a list of model objects.

- terms:

  Names of those terms from `model`, for which predictions should be
  displayed (so called *focal terms*). Can be:

  - A character vector, specifying the names of the focal terms. This is
    the preferred and probably most flexible way to specify focal terms,
    e.g. `terms = "x [40:60]"`, to calculate predictions for the values
    40 to 60.

  - A list, where each element is a named vector, specifying the focal
    terms and their values. This is the "classical" R way to specify
    focal terms, e.g. `list(x = 40:60)`.

  - A formula, e.g. `terms = ~ x + z`, which is internally converted to
    a character vector. This is probably the least flexible way, as you
    cannot specify representative values for the focal terms.

  - A data frame representing a "data grid" or "reference grid".
    Predictions are then made for all combinations of the variables in
    the data frame.

  `terms` at least requires one variable name. The maximum length is
  five terms, where the second to fifth term indicate the groups, i.e.
  predictions of the first term are grouped at meaningful values or
  levels of the remaining terms (see
  [`values_at()`](https://strengejacke.github.io/ggeffects/reference/values_at.md)).
  It is also possible to define specific values for focal terms, at
  which adjusted predictions should be calculated (see details below).
  All remaining covariates that are not specified in `terms` are
  "marginalized", see the `margin` argument in
  [`?predict_response`](https://strengejacke.github.io/ggeffects/reference/predict_response.md).
  See also argument `condition` to fix non-focal terms to specific
  values, and argument `typical` for `ggpredict()` or `ggemmeans()`.

- ci_level:

  Numeric, the level of the confidence intervals. Use `ci_level = NA` if
  confidence intervals should not be calculated (for instance, due to
  computation time). Typically, confidence intervals are based on the
  returned standard errors for the predictions, assuming a t- or normal
  distribution (based on the model and the available degrees of freedom,
  i.e. roughly `+/- 1.96 * SE`). See introduction of [this
  vignette](https://strengejacke.github.io/ggeffects/articles/ggeffects.html)
  for more details.

- type:

  Character, indicating whether predictions should be conditioned on
  specific model components or not, or whether population or unit-level
  predictions are desired. Consequently, most options only apply for
  survival models, mixed effects models and/or models with
  zero-inflation (and their Bayesian counter-parts); only exception is
  `type = "simulate"`, which is available for some other model classes
  as well (which respond to
  [`simulate()`](https://rdrr.io/r/stats/simulate.html)).

  **Note 1:** For `brmsfit`-models with zero-inflation component, there
  is no `type = "zero_inflated"` nor `type = "zi_random"`; predicted
  values for these models *always* condition on the zero-inflation part
  of the model. The same is true for `MixMod`-models from
  **GLMMadaptive** with zero-inflation component (see 'Details').

  **Note 2:** If `margin = "empirical"`, or when calling `ggaverage()`
  respectively, (i.e. counterfactual predictions), the `type` argument
  is handled differently. It is set to `"response"` by default, but
  usually accepts all possible options from the `type`-argument of the
  model's respective [`predict()`](https://rdrr.io/r/stats/predict.html)
  method. E.g., passing a `glm` object would allow the options
  `"response"`, `"link"`, and `"terms"`. For models with zero-inflation
  component, the below mentioned options `"fixed"`, `"zero_inflated"`
  and `"zi_prob"` can also be used and will be "translated" into the
  corresponding `type` option of the model's respective
  [`predict()`](https://rdrr.io/r/stats/predict.html)-method.

  **Note 3:** If `margin = "marginalmeans"`, or when calling
  `ggemmeans()` respectively, `type = "random"` and `type = "zi_random"`
  are not available, i.e. no unit-level predictions are possible.

  - `"fixed"` (or `"count"`)

    Predicted values are conditioned on the fixed effects or conditional
    model only. For mixed models, predicted values are on the
    *population-level*, i.e. `re.form = NA` when calling
    [`predict()`](https://rdrr.io/r/stats/predict.html). For models with
    zero-inflation component, this type would return the predicted mean
    from the count component (without conditioning on the zero-inflation
    part).

  - `"random"`

    This only applies to mixed models, and `type = "random"` does not
    condition on the zero-inflation component of the model. Use this for
    *unit-level* predictions, i.e. predicted values for each level of
    the random effects groups. Add the name of the related random effect
    term to the `terms`-argument (for more details, see [this
    vignette](https://strengejacke.github.io/ggeffects/articles/introduction_effectsatvalues.html)).

  - `"zero_inflated"` (or `"zi"`)

    Predicted values are conditioned on the fixed effects and the
    zero-inflation component, returning the expected value of the
    response (`mu*(1-p)`). For For mixed models with zero-inflation
    component (e.g. from package **glmmTMB**), this would return the
    expected response `mu*(1-p)` on the *population-level*. See
    'Details'.

  - `"zi_random"` (or `"zero_inflated_random"`)

    This only applies to mixed models. Predicted values are conditioned
    on the fixed effects and the zero-inflation component. Use this for
    *unit-level* predictions, i.e. predicted values for each level of
    the random effects groups. Add the name of the related random effect
    term to the `terms`-argument (for more details, see [this
    vignette](https://strengejacke.github.io/ggeffects/articles/introduction_effectsatvalues.html)).

  - `"zi_prob"`

    Returns the predicted zero-inflation probability, i.e. probability
    of a structural or "true" zero.

  - `"simulate"`

    Predicted values and confidence resp. prediction intervals are based
    on simulations, i.e. calls to
    [`simulate()`](https://rdrr.io/r/stats/simulate.html). This type of
    prediction takes all model uncertainty into account. Currently
    supported models are objects of class `lm`, `glm`, `glmmTMB`, `wbm`,
    `MixMod` and `merMod`. Use `nsim` to set the number of simulated
    draws (see `...` for details).

  - `"survival"`, `"cumulative_hazard"` and `"quantile"`

    `"survival"` and `"cumulative_hazard"` apply only to `coxph`-objects
    from the **survial**-package. These options calculate the survival
    probability or the cumulative hazard of an event.
    `type = "quantile"` only applies to `survreg`-objects from package
    **survival**, which returns the predicted quantiles. For this
    option, the `p` argument is passed to
    [`predict()`](https://rdrr.io/r/stats/predict.html), so that
    quantiles for different probabilities can be calculated, e.g.
    `predict_response(..., type = "quantile", p = c(0.2, 0.5, 0.8))`.

  When `margin = "empirical"` (or when calling `ggaverage()`), the
  `type` argument accepts all values from the `type`-argument of the
  model's respective
  [`predict()`](https://rdrr.io/r/stats/predict.html)-method.

- typical:

  Character vector, naming the function to be applied to the covariates
  (non-focal terms) over which the effect is "averaged". The default is
  `"mean"`. Can be `"mean"`, "`weighted.mean`", `"median"`, `"mode"` or
  `"zero"`, which call the corresponding R functions (except `"mode"`,
  which calls an internal function to compute the most common value);
  `"zero"` simply returns 0. By default, if the covariate is a factor,
  only `"mode"` is applicable; for all other values (including the
  default, `"mean"`) the reference level is returned. For character
  vectors, only the mode is returned. You can use a named vector to
  apply different functions to integer, numeric and categorical
  covariates, e.g. `typical = c(numeric = "median", factor = "mode")`.
  If `typical` is `"weighted.mean"`, weights from the model are used. If
  no weights are available, the function falls back to `"mean"`.
  **Note** that this argument is ignored for
  [`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md),
  because the `margin` argument takes care of this.

- condition:

  Named character vector, which indicates covariates that should be held
  constant at specific values. Unlike `typical`, which applies a
  function to the covariates to determine the value that is used to hold
  these covariates constant, `condition` can be used to define exact
  values, for instance `condition = c(covariate1 = 20, covariate2 = 5)`.
  See 'Examples'.

- back_transform:

  Logical, if `TRUE` (the default), predicted values for log-, log-log,
  exp, sqrt and similar transformed responses will be back-transformed
  to original response-scale. See
  [`insight::find_transformation()`](https://easystats.github.io/insight/reference/find_transformation.html)
  for more details.

- vcov:

  Variance-covariance matrix used to compute uncertainty estimates
  (e.g., for confidence intervals based on robust standard errors). This
  argument accepts a covariance matrix, a function which returns a
  covariance matrix, or a string which identifies the function to be
  used to compute the covariance matrix.

  - A covariance matrix

  - A function which returns a covariance matrix (e.g.,
    [`stats::vcov()`](https://rdrr.io/r/stats/vcov.html))

  - A string which indicates the kind of uncertainty estimates to
    return.

    - Heteroskedasticity-consistent: `"HC"`, `"HC0"`, `"HC1"`, `"HC2"`,
      `"HC3"`, `"HC4"`, `"HC4m"`, `"HC5"`. See
      [`?sandwich::vcovHC`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html)

    - Cluster-robust: `"vcovCR"`, `"CR0"`, `"CR1"`, `"CR1p"`, `"CR1S"`,
      `"CR2"`, `"CR3"`. See
      [`?clubSandwich::vcovCR`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md).

    - Bootstrap: `"BS"`, `"xy"`, `"fractional"`, `"jackknife"`,
      `"residual"`, `"wild"`, `"mammen"`, `"norm"`, `"webb"`. See
      [`?sandwich::vcovBS`](https://sandwich.R-Forge.R-project.org/reference/vcovBS.html)

    - Other `sandwich` package functions: `"HAC"`, `"PC"`, `"CL"`, or
      `"PL"`.

  If `NULL`, standard errors (and confidence intervals) for predictions
  are based on the standard errors as returned by the
  [`predict()`](https://rdrr.io/r/stats/predict.html)-function. **Note**
  that probably not all model objects that work with
  [`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)
  are also supported by the **sandwich** or **clubSandwich** packages.

  See details in [this
  vignette](https://strengejacke.github.io/ggeffects/articles/practical_robustestimation.html).

- vcov_args:

  List of arguments to be passed to the function identified by the
  `vcov` argument. This function is typically supplied by the
  **sandwich** or **clubSandwich** packages. Please refer to their
  documentation (e.g.,
  [`?sandwich::vcovHAC`](https://sandwich.R-Forge.R-project.org/reference/vcovHAC.html))
  to see the list of available arguments. If no estimation type
  (argument `type`) is given, the default type for `"HC"` equals the
  default from the **sandwich** package; for type `"CR"` the default is
  set to `"CR3"`. For other defaults, refer to the documentation in the
  **sandwich** or **clubSandwich** package.

- weights:

  This argument is used in two different ways, depending on the `margin`
  argument.

  - When `margin = "empirical"` (or when calling `ggaverag()`),
    `weights` can either be a character vector, naming the weigthing
    variable in the data, or a vector of weights (of same length as the
    number of observations in the data). This variable will be used to
    weight adjusted predictions.

  - When `margin = "marginalmeans"` (or when calling `ggemmeans()`),
    `weights` must be a character vector and is passed to
    [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html),
    specifying weights to use in averaging non-focal categorical
    predictors. Options are `"equal"`, `"proportional"`, `"outer"`,
    `"cells"`, `"flat"`, and `"show.levels"`. See
    [`?emmeans::emmeans`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
    for details.

- verbose:

  Toggle messages or warnings.

- bias_correction:

  Logical, if `TRUE`, adjusts for bias-correction when back-transforming
  the predicted values (to the response scale) for non-Gaussian *mixed
  models*. Back-transforming the the population-level predictions
  ignores the effect of the variation around the population mean, so the
  result on the original data scale is biased due to *Jensen's
  inequality*. That means, when `type = "fixed"` (the default) and
  population level predictions are returned, it is recommended to set
  `bias_correction = TRUE`. To apply bias-correction, a valid value of
  sigma is required, which is extracted by default using
  [`insight::get_variance_residual()`](https://easystats.github.io/insight/reference/get_variance.html).
  Optionally, to provide own estimates of uncertainty, use the `sigma`
  argument. Note that `bias_correction` currently only applies to mixed
  models, where there are additive random components involved and where
  that bias-adjustment can be appropriate. If `ggemmeans()` is called,
  bias-correction can also be applied to GEE-models.

- interval:

  Type of interval calculation, can either be `"confidence"` (default)
  or `"prediction"`. May be abbreviated. Unlike *confidence intervals*,
  *prediction intervals* include the residual variance (sigma^2) to
  account for the uncertainty of predicted values. Note that prediction
  intervals are not available for all models, but only for models that
  work with
  [`insight::get_sigma()`](https://easystats.github.io/insight/reference/get_sigma.html).
  For Bayesian models, when `interval = "confidence"`, predictions are
  based on posterior draws of the linear predictor
  [`rstantools::posterior_epred()`](https://mc-stan.org/rstantools/reference/posterior_epred.html).
  If `interval = "prediction"`,
  [`rstantools::posterior_predict()`](https://mc-stan.org/rstantools/reference/posterior_predict.html)
  is called.

## Value

A data frame (with `ggeffects` class attribute) with consistent data
columns:

- `"x"`: the values of the first term in `terms`, used as x-position in
  plots.

- `"predicted"`: the predicted values of the response, used as
  y-position in plots.

- `"std.error"`: the standard error of the predictions. *Note that the
  standard errors are always on the link-scale, and not back-transformed
  for non-Gaussian models!*

- `"conf.low"`: the lower bound of the confidence interval for the
  predicted values.

- `"conf.high"`: the upper bound of the confidence interval for the
  predicted values.

- `"group"`: the grouping level from the second term in `terms`, used as
  grouping-aesthetics in plots.

- `"facet"`: the grouping level from the third term in `terms`, used to
  indicate facets in plots.

  The estimated marginal means (or predicted values) are always on the
  response scale!

  For proportional odds logistic regression (see
  [`?MASS::polr`](https://rdrr.io/pkg/MASS/man/polr.html)) resp.
  cumulative link models (e.g., see
  [`?ordinal::clm`](https://rdrr.io/pkg/ordinal/man/clm.html)), an
  additional column `"response.level"` is returned, which indicates the
  grouping of predictions based on the level of the model's response.

  Note that for convenience reasons, the columns for the intervals are
  always named `"conf.low"` and `"conf.high"`, even though for Bayesian
  models credible or highest posterior density intervals are returned.

  There is an
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) method
  for objects of class `ggeffects`, which has an `terms_to_colnames`
  argument, to use the term names as column names instead of the
  standardized names `"x"` etc.

## Details

Please see
[`?predict_response`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)
for details and examples.
