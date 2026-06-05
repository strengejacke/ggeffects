# Changelog

## ggeffects 2.3.2

CRAN release: 2025-12-16

- Fixed issues related to latest *glmmTMB* update.

## ggeffects 2.3.1

CRAN release: 2025-08-20

- Fixed issues related to latest *insight* update.

## ggeffects 2.3.0

CRAN release: 2025-06-13

- `engine = "marginaleffects"` was removed from
  [`test_predictions()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md).
  Full support for the {marginaleffects} package is now available in the
  {modelbased} package from the easystats-projects. Consequently, the
  `johnson_neyman()` function was also removed. This decision was mainly
  to reduce maintainance burden. For marginal means, contrasts and
  comparisons, as well as marginal effects (slopes), the ggeffects
  package will be superseded by the modelbased package. For predictions,
  simulated responses, or emmeans-support, support in ggeffects will be
  continued in the future, however, almost all functionality is or will
  be available in the modelbased package, too.

## ggeffects 2.2.0

CRAN release: 2025-02-05

### Major changes

- The functions
  [`test_predictions()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  and `jonhson_neyman()` have been largely revised, due to breaking
  changes in the *marginaleffects* package. The two functions will now
  internally call
  [`modelbased::estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.html)
  and
  [`modelbased::estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.html),
  to reduce maintanance load. Thus, some features have changed or are
  probably no longer / not yet available.

## ggeffects 2.1.0

CRAN release: 2025-01-20

### Changes

- The `terms` argument can now include up to five focal terms (formerly:
  four). Accordingly, the
  [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  method was revised. For four focal terms, a facet grid is used to plot
  all relevant panels. For five focal terms, multiple plots with facet
  grids are arranged using the **patchwork** package.

- [`values_at()`](https://strengejacke.github.io/ggeffects/reference/values_at.md)
  gains a new `"threenum"` option.

- The `test` argument is
  [`test_predictions()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  can now also be a formula to calculate consecutive contrasts,
  contrasts against the reference level or against the “average” factor
  level.

## ggeffects 2.0.0

CRAN release: 2024-11-27

### Breaking changes

- The way how `type = "random"` works has been revised.
  `type = "random"` no longer returns predictions intervals. Instead,
  use `interval = "prediction"`. `type = "random"` is now mainly
  responsible for *unit-level* predictions in mixed models, as opposed
  to `type = "fixed"`, which should be used for *population-level*
  predictions. The separation from `type = "random"` and the `interval`
  argument makes the handling for mixed models easier, more intuitive
  and consistent. Accordingly, the vignette regarding the introduction
  into mixed models with *ggeffects* has been largely revised.

- The `vcov_fun` and `vcov_type` argument were removed and are now
  replaced by the single `vcov` argument, to be in line with the
  handling of heteroscedasticity-consistent standard errors in other
  packages (mainly: *easystats* eco-system).

- The deprecated arguments for
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md),
  [`vcov()`](https://strengejacke.github.io/ggeffects/reference/vcov.md),
  [`ggeffect()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  and
  [`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  have been removed.

- The deprecated arguments for the
  [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  method have been removed.

- Options `type = "random"` and `type = "zi_random"` are not longer
  available for
  [`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md).
  These were only responsible to set *prediction* intervals, which can
  be done with `interval = "prediction"` now.

### Changes

- Added a `get_preditions()` method, which can be used to implement own
  S3-classes to add support for new models to **ggeffects**. There is a
  corresponding vignette, too.

- The
  [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  method gets a `dot_shape` argument, to change the shape of data points
  when `show_data = TRUE`.

- `test_predictions` gains a `test_args` argument, to optionally pass
  further options to `test` for *emmeans* engine/options.

- [`vcov()`](https://strengejacke.github.io/ggeffects/reference/vcov.md)
  returns a more informative warning, when the variance-covariance
  matrix could not be extracted due to problems in creating the model
  matrix (which prevents confidence intervals from being calculated).

- Added Okabe-Ito color scale to the available color ggeffects-palettes.

- For models of class `survreg`, argument `type` can also be
  `"quantile"`.

### Bug fixes

- Fixed issue with argument `condition` for
  [`ggaverage()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md).

- Fixed issues with missing confidence intervals for objects of class
  `averaging`.

## ggeffects 1.7.2

CRAN release: 2024-10-13

### Breaking changes

- The deprecated argument `ppd` was removed.

- Some of the deprecated arguments in
  [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  were removed.

- Pooling functions now use the model’s degrees of freedom to calculate
  the critical values for the confidence intervals.

### Changes

- `test = "slope"` (or `test = "trend"`) are aliases in
  [`test_predictions()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  for `test = NULL` with numeric predictors.

- [`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)
  (and
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md),
  [`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  and
  [`ggeffect()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md))
  get an argument `bias_correction`, to correct for bias when
  back-transforming predictions for non-Gaussian mixed models.

## ggeffects 1.7.1

CRAN release: 2024-09-01

### General

- Added support for models of class `glm_weightit`, `ordinal_weightit`,
  `multinom_weightit` from package *WeightIt*.

### Bug fixes

- Fixed issues for multivariate response models due changes in the last
  *insight* package updates.

- Fixed issue with swapped lower and higher confidence interval values
  for models with inverse-link.

- Fixed CRAN check issues due to breaking changes in the last
  *marginaleffects* update.

## ggeffects 1.7.0

CRAN release: 2024-06-20

### Breaking

- The deprecated argument `ci.lvl` in
  [`test_predictions()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  was removed.

### General

- [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  now supports models of class `glmgee` (package *glmtoolbox*).

- [`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  gains arguments `vcov_fun`, `vcov_type` and `vcov_args` to specify the
  variance-covariance matrix for the marginal means, similar to what is
  already available in
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  and
  [`ggaverage()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md).

- When `test = "contrast"`, the `engine` is automatically set to
  `"emmeans"` in
  [`test_predictions()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md).

- [`ggaverage()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  (or `predict_response(..., margin = "empirical")`) now also supports
  following `type` options for zero-inflated models: `"zi_prob"`,
  `"zero_inflated"` and `"fixed"`.

- Support for zero-inflated models was massively improved in
  [`test_predictions()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md),
  which, for instance, now also supports `scale = "zprob"` to calculate
  contrasts for the zero-inflation probability for zero-inflated models
  from *glmmTMB* or *pscl*. Furthermore, when predictions for
  zero-inflation probabilities were calculated using
  `pr <- predict_response(..., type = "zi_prob")`, corresponding
  contrasts will be calculated with `test_predictions(pr)`
  automatically. Additionally, other types for models with
  zero-inflation component (`"zero_inflated"`, `"fixed"`) are supported
  as well.

- [`ggeffect()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  now warns the user about arguments that are supported by
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  or
  [`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md),
  but not by this function (e.g., `vcov_fun`).

- Improved accuracy of standard errors for
  `test_predictions(..., engine = "ggeffects")`.

- The `terms` argument now also accepts the shortcut `"percentile"`
  (plus numeric value) to select a range of percentiles for continuous
  variables, e.g. `terms = "x [percentile90]"` to select a range of the
  90% percentile.

### Bug fixes

- Fixed issue with *brms* models with monotonic effects in formula
  (`mo()`).

- Fixed issue in
  [`vcov()`](https://strengejacke.github.io/ggeffects/reference/vcov.md)
  for `ggeffects` objects, which could occur in rare situations when
  some of the predictors were character vectors.

- Fixed issue with calculation of standard errors when one of the focal
  term was a character vector.

- Fixed issue in
  [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  method with `show_data = TRUE`, where in certain situations the raw
  data points were not colored when groups were present.

- Fixed issue in
  [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  with too many data points when collapsing random effects groups.

## ggeffects 1.6.0

CRAN release: 2024-05-18

### General

- [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  now works for models of class `clm2` from package *ordinal*, however,
  confidence intervals are not yet supported for these models.

- [`ggeffect()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  now passes the `latent` argument for models with ordinal outcome down
  to [`effects::Effect()`](https://rdrr.io/pkg/effects/man/effect.html),
  to plot effects for ordinal models on the latent scale.

- When argument `test` in
  [`test_predictions()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  is `"interaction"`, `"consecutive"`, or a data frame, *emmeans* is
  automatically used as backend, as this is the relevant package that
  supports these argument types.

- [`format()`](https://rdrr.io/r/base/format.html) (and hence,
  [`print()`](https://strengejacke.github.io/ggeffects/reference/print.md))
  for
  [`test_predictions()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  gains a `combine_levels` argument, to combine levels of the focal term
  in the output table.

- The `engine` argument in
  [`test_predictions()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  can now also be `"ggeffects"`. However, this is currently
  work-in-progress and offers muss less options as the default engine,
  `"marginaleffects"`. It can be faster in some cases, though, and works
  for comparing predicted random effects in mixed models.

- [`test_predictions()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  now automatically falls back to engines `"emmeans"` or `"ggeffects"`
  if the *marginaleffects* (or *emmeans*) package is not installed.

- [`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md),
  [`test_predictions()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  and
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  will warn the user when all focal terms are only included as random
  effects in the model and no appropriate `type` or `margin` is
  specified. This is to avoid meaningless results.

- [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  gets an `n_rows` argument, to define the number of rows for the panel
  alignment. This is useful when the number of facets is large and the
  default alignment is not optimal.

- The `ppd` argument for Bayesian models will be superseded by the
  `interval` argument, i.e. `ppd = TRUE` is equivalent to
  `interval = "prediction"` (and `ppd = FALSE` is equivalent to
  `interval = "confidence"`).

- When `back_transform = FALSE`, and model has a transformed response
  variable, the
  [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  method for `ggeffects` objects now rescales the raw data points. This
  ensures that the raw data points are plotted on the same scale as the
  predicted values when `show_data = TRUE`.

- Minor revisions of documentation and vignettes, to improve readability
  and clarity.

- Several arguments have been deprecated and replaced by new argument
  names. A warning is printed when deprecated arguments are used. The
  deprecated arguments will be removed in a future release.

### Bug fixes

- Fixed issue in
  [`print()`](https://strengejacke.github.io/ggeffects/reference/print.md)
  for
  [`ggeffect()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  and models with ordinal outcome, where one column was too much in the
  output.

- Fixed issue in
  [`test_predictions()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  with wrong order of term labels when a focal term was a character
  vector.

- Fixed issue in
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  with `wbm` models from package *panelr*.

- Fixed issue in
  [`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  for `glmmTMB` models with zero-inflation, when `terms` included
  variables that were specified in the conditional, but not in the
  zero-inflation model formula.

- Fixed issue in
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  for Stan models (from packages *rstanarm* and *brms*) where the
  `ci_level` argument was not correctly recognized.

- Fixed CRAN check issues due to latest *marginaleffects* update.

## ggeffects 1.5.2

CRAN release: 2024-04-15

### General

- [`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  (and hence, `predict_response(..., margin = "marginalmeans"))` now
  supports `type = "zi_prob"` for zero-inflated models from package
  *glmmTMB*, i.e. can now predict the zero-inflation probability.

- [`test_predictions()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  and
  [`ggaverage()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  were updated to work with the latest release of the *marginaleffects*
  package. That release fixed issues with inaccurate standard errors for
  *glmmTMB* models.

- [`test_predictions()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  gains a `margin` argument, to indicate how to marginalize over
  non-focal terms. This ensures that estimates of pairwise comparisons
  are in line with estimates of predictions.

- [`test_predictions()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  gains an `engine` argument, to indicate which package to use to
  compute pairwise comparisons or contrasts. By default, the
  *marginaleffects* package is used, but you can also use the *emmeans*
  package.

### Bug fixes

- Fixed issue in
  [`ggeffect()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  when representative values for a focal term included a zero,
  e.g. `terms = "focal [0,3,5]"`.

## ggeffects 1.5.1

CRAN release: 2024-03-26

### General

- Overhaul of the documentation (again), to provide more clarity about
  the terminology “adjusted predictions”, “marginal means” and “marginal
  effects”, and how to calculate each of these quantities using the
  *ggeffects* package.

- [`print_html()`](https://easystats.github.io/insight/reference/display.html)
  methods were updated to work with the latest release of *tinytable*.

- New
  [`print_md()`](https://easystats.github.io/insight/reference/display.html)
  method, to print the output as markdown table. This is useful inside
  RMarkdown or Quarto documents, where the output can be directly
  included.

## ggeffects 1.5.0

CRAN release: 2024-02-24

### New functions

- [`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)
  as “generic” high-level function, which is a replacement for
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md),
  [`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  and
  [`ggaverage()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md).
  The new function is more clear about how the function marginalizes
  over non-focal terms. The `margin` argument can be used to specify how
  to marginalize over non-focal terms, i.e. which function internally is
  used to compute the marginal effects.

### General

- The documentation was revised, to provide more clarity about what the
  package functions do and how to decide, which function or method to
  calculate marginal effects is the most appropriate.

- Improved calculation of prediction intervals for Poisson regression
  models.

- Improved handling of the `vcov_fun` argument. This argument now
  accepts an estimation type as string, e.g. `vcov_fun = "HC0"`, which
  is then used to compute the variance-covariance matrix. Thus, it is no
  longer necessary to define both `vcov_fun` and `vcov_type`, if the
  variance-covariance matrix is covered by one of the pre-defined
  estimation types. See
  [`?ggpredict`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  for details.

- [`hypothesis_test()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  now also accepts the `vcov_fun` argument, and not only `vcov`. This
  ensures consistency with the `vcov_fun` argument in
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md).
  Furthermore, the information about the type of variance-covariance
  matrix is saved to the *ggeffects* object returned by
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md),
  [`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)
  etc., and if this information is available, it is automatically used
  in
  [`hypothesis_test()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  when a *ggeffects* object is passed to the function.

### Bug fixes

- Fixed bug in wrong order of printed (sub-)tables for predictions.

- Fixed wrong table column name for confidence interval columns for
  other confidence levels than the default 95% in
  [`print()`](https://strengejacke.github.io/ggeffects/reference/print.md)
  for `ggeffects` objects.

- Fixed issue with
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  for models of class `fixest` when the cluster variable was numeric.

## ggeffects 1.4.0

CRAN release: 2024-02-05

### Breaking Changes

- The
  [`print()`](https://strengejacke.github.io/ggeffects/reference/print.md)
  method has been revised. A
  [`format()`](https://rdrr.io/r/base/format.html) method was added,
  which allows to format the output of
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  (and
  [`ggeffect()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  etc.) for printing. The refactoring of the
  [`print()`](https://strengejacke.github.io/ggeffects/reference/print.md)
  method makes the code base easier to maintain and it is easier to
  enhance the print-functionality. Now it is possible to create HTML
  tables as well, using
  [`print_html()`](https://easystats.github.io/insight/reference/display.html).
  The style of the output has also slightly changed. By default,
  confidence intervals are no longer enclosed in parentheses. You can
  change this behaviour by passing the `ci_brackets` argument to
  [`print()`](https://strengejacke.github.io/ggeffects/reference/print.md)
  (see examples), or permanently define custom parentheses or brackets
  with, e.g., `options(ggeffects_ci_brackets = c("[", "]"))`.
  Additionally, there are new arguments to further control the output of
  the tables: `collapse_ci` can be used to collapse confidence intervals
  into a single column together with the predicted values.
  `collapse_tables` can be used to collapse multiple tables into a
  single table (only applies when there is more than one focal term).
  Again, these settings can be permanently defined via options (see
  [`?print.ggeffects`](https://strengejacke.github.io/ggeffects/reference/print.md)
  for details).

### New functions

- [`print_html()`](https://easystats.github.io/insight/reference/display.html),
  to print the output as HTML table. This method is available for
  objects from
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  (and alike) as well as
  [`hypothesis_test()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md).

### General

- A new vignette was added, showing examples for the new
  print-functionality.

### Bug fixes

- Fixed issue with
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  for models of class `vglm` with multivariate responses.

## ggeffects 1.3.4

CRAN release: 2023-12-18

### General

- [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  now supports models of class `rqs` from package *quantreg*.

- Fixed issues to be compatible with forthcoming update of *emmeans*.

## ggeffects 1.3.3

CRAN release: 2023-12-15

### New functions

- [`ggaverage()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md),
  to compute average predicted values. This function is based on
  `marginaleffects::avg_predictons()`.

- [`pool_comparisons()`](https://strengejacke.github.io/ggeffects/reference/pool_comparisons.md),
  to pool results from multiple calls to
  [`hypothesis_test()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md),
  e.g. with imputed data sets.

### General

- Support for `sdmTMB` (*sdmTMB*) models.

- Improved support for the *logistf* package, including models `flic()`
  and `flac()`.

- Confidence intervals for predictions from `merMod` models (package
  *lme4*) now use the standard errors returned by
  `predict(..., se.fit = TRUE)`. This should not affect numerical
  results, but *can* be more robust for certain edge cases. Note that
  standard errors are only based on
  [`predict()`](https://rdrr.io/r/stats/predict.html) when
  `tpye = "fixed"`. For `type = "random"`, standard errors are still
  based on the model’s variance-covariance matrix, taking uncertainty
  from random effects into account.

- [`hypothesis_test()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  now suppports models from package *parsnip*.

- `johnson_neyman()` gains a `p_adjust` argument, to adjust p-values for
  multiple comparisons. Currently, only `p_adjust = "esarey"` (resp.
  `p_adjust = "es"`) and `p_adjust = "fdr"` (resp. `p_adjust = "bh"`)
  are supported.

### Bug fixes

- [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  now computes appropriate predicted probabilites for models of class
  [`rms::lrm()`](https://rdrr.io/pkg/rms/man/lrm.html) with ordinal
  outcome.

- Fixed issue in
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  for `type = "random"` when sampling from random effects levels, where
  the levels were numeric characters with a pattern like `"001"`,
  `"002"`, etc.

- Fixed minor issue in `plot.ggalleffects()`.

- `...` arguments in
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  are now passed down to the
  [`predict()`](https://rdrr.io/r/stats/predict.html) method for
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) models.

## ggeffects 1.3.2

CRAN release: 2023-10-17

### Breaking changes

- Some function arguments will be renamed, to achieve consistency across
  the package and across other packages where I’m involved in the
  development. This will be a soft transition, i.e. the old argument
  names will still work for some package updates.

### Changes

- The `typical` argument now supports a mix of functions for different
  variable types at which numeric or categorical covariates (non-focal
  terms) are held constant.

- Clarification of how the `re.form` argument is set when using
  `type = "random"` resp. `type = "fixed"` in
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md).

- [`hypothesis_test()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  now returns the standard error of contrasts or pairwise comparisons as
  attribute `standard_error`. This can be used to compute the
  test-statistic, if required. In forthcoming updates, there will be
  methods for
  [`insight::get_statistic()`](https://easystats.github.io/insight/reference/get_statistic.html)
  and
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  to include standard errors and test-statistics in the output.

- [`test_predictions()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  was added as an alias for
  [`hypothesis_test()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md).

### Bug fixes

- Fixed issue in
  [`hypothesis_test()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  for mixed models, which sometimes failed when random effects group
  variables were numeric, and not factors.

## ggeffects 1.3.1

CRAN release: 2023-09-05

### New functions

- `johnson_neyman()`, to create Johnson-Neyman intervals and plots from
  `ggeffects` objects.

### Changes

- Better automatic handling of offset-terms, both for predictions and
  generating plots with raw data. When the model formula contains an
  offset-term, and the offset term is fixed at a specific value, the
  response variable is now automatically transformed back to the
  original scale, and the offset-term is added to the predicted values.
  A warning is printed when model contains transformed offset-terms that
  are not fixed, e.g. via the `condition` argument.

- [`ggeffect()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  now supports `nestedLogit` models.

### Bug fixes

- Fixed issue in
  [`hypothesis_test()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md),
  where the `by` argument did not work together with the
  `collapse_levels` argument.

- Fixed issue in
  [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  method when adding raw data points for data frame that had now row
  names.

## ggeffects 1.3.0

CRAN release: 2023-08-21

### Breaking

- To avoid confusion when adding raw data or residuals to plots, the
  `jitter` argument that is used to add some noice to data points to
  avoid overlapping now defaults to `NULL`. Formerly, a small jitter was
  added by default, leading to confusion when data points did not match
  the original data.

### Changes

- The
  [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  method gets a `label.data` argument, to add row names to data points
  when `add.data = TRUE`.

- `tibbles` are always converted into data frames, to avoid issues.

- [`hypothesis_test()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  gains a `by` argument, to specify a variable that is used to group the
  comparisons or contrasts. This is useful for models with interaction
  terms.

### Bug fixes

- Plotting residuals did not work when model object passed to
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  were inside a list, or when called from inside functions (scoping
  issues).

- Fixed issue where plotting raw data
  (i.e. `plot(..., add.data = TRUE)`) did not work when there were
  missing data in weight variables (i.e. when the regression model used
  weights).

- Fixes issue in
  [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  when no term was specified in the call to
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md).

- Fixed issues with robust estimation for models of package *pscl*.

- Fixed issues introduced by breaking changes in *marginaleffects*.

## ggeffects 1.2.3

CRAN release: 2023-06-11

### General

- Support for `nestedLogit` (*nestedLogit*) models.

- `hyothesis_test()` gains a `scale` argument, to explicitely modulate
  the scale of the contrasts or comparisons (e.g. `"response"` or
  `"link"`, or `"exp"` to return transformed contrasts/comparisons).

- `hyothesis_test()` now includes the response level for models with
  ordinal outcomes (and alike).

- When
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  is used inside functions and a name for a vector variable (passed as
  argument to that function) in `terms` is used, the variable is now
  correctly recognized.

- Partial residuals (when `plot(..., residuals = TRUE)`) now supports
  more linear (mixed) models, including models from package *lme* (such
  as `gls()` or `lme()`).

- For mixed models, `type = "random"` used to calculate *prediction
  intervals* that always accounted for random effects variances, leading
  to larger intervals. Using `interval = "confidence"` together with
  `type = "random"` now allows to calculate “usual” confidence intervals
  for random effects. This is usefule for predictions at specific group
  levels of random effects (when focal terms are only fixed effects, use
  `type = "fixed"` for regular confidence intervals).

- The `vcov.fun` argument can now also be a function that returns a
  variance-covariance matrix.

- The `verbose` argument in
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  and
  [`hypothesis_test()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  now also toggle messages for the respective
  [`print()`](https://strengejacke.github.io/ggeffects/reference/print.md)
  methods.

- The
  [`print()`](https://strengejacke.github.io/ggeffects/reference/print.md)
  method for
  [`hypothesis_test()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  has been revised and now provides more details for possible
  transformation of the scale of comparisons and contrasts.

- The
  [`print()`](https://strengejacke.github.io/ggeffects/reference/print.md)
  method now shows all rows by default when the focal term is a factor.
  If rows are not shown in the output, a message is printed to inform
  the user about truncated output.

- A new vignette about using *ggeffects* in the context of an
  intersectional multilevel analysis of individual heterogeneity, using
  the MAIHDA framework.

### Bug fixes

- Fixed issue with wrong order of x-axis-labels for plots when the focal
  term on the x-axis was a character vector, where alphabetical order of
  values did not match order of predictions.

- Fixed issues in `hyothesis_test()` for models with ordinal outcomes
  (and alike).

## ggeffects 1.2.2

CRAN release: 2023-05-04

### General

- Added a new `[.ggeffects` function, which allows to subset `ggeffects`
  objects in the same way as regular data frames, i.e. it is now
  possible to do:

      gge <- ggpredict(model, "x1")
      gge[c(1:2)]

- Using a name for a vector variable in `terms` now works from inside
  functions. E.g., you can now do:

      foo <- function(data) {
        fit <- lm(barthtot ~ c12hour + c172code, data = data)
        v <- c(20, 50, 70)
        ggpredict(fit, terms = "c12hour [v]")
      }
      foo(efc)

- The `colors` argument in
  [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  can now also be applied to single-colored plots.

- `hyothesis_test()` gains a `collapse_level` argument, to collapse term
  labels that refer to the same levels into a singel unique level
  string.

### Bug fixes

- Fixed issue with misplaced residuals when x-axis was categorical and
  the factor levels were not in alphabetical order.

- [`pool_predictions()`](https://strengejacke.github.io/ggeffects/reference/pool_predictions.md)
  now correctly handles models with transformed response variables (like
  `log(y)`) and returns the correct back-transformed pooled predictions
  (and their confidence intervals).

- Fixed issue with wrong computation of confidence intervals for models
  of class `clm` from package *ordinal*.

- Fixed failing tests due to changes in the *logistf* package, which now
  also supports *emmeans*. That means,
  [`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  now also works for models from package *logistf*.

- Fixed bug in
  [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  when partial residuals were added (i.e. `residuals = TRUE`) and
  `collapse.group` was provided (in case of mixed models).

- Fixed issue with on-the-fly created factors inside formulas, which
  were not correctly treated as factors in the
  [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  method. This bug was related to recent changes in
  [`insight::get_data()`](https://easystats.github.io/insight/reference/get_data.html).

- Fixed issue with wrong labels in `hyothesis_test()` for comparisons
  with many rows, when betas starting with same digit were specified
  (e.g. `test = "(b1-b13)=(b3-b15)"`).

- Fixed issue in `hyothesis_test()` for mixed models when focal terms
  included factors with factor levels that contained a comma.

- Fixed issue with missing confidence intervals for mixed models when
  one of the variable names contains white space characters
  (e.g. `y ~ 'x a' + xb`).

## ggeffects 1.2.1

CRAN release: 2023-04-02

### General

- Support for `mblogit` (*mclogit*), `phylolm` and `phyloglm`
  (*phylolm*) models.

### Changes to functions

- [`hypothesis_test()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  gains an `equivalence` argument, to compute tests of practical
  equivalence for contrasts and comparisons.

- The message whether contrasts or comparisons from
  [`hypothesis_test()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  are on the link-scale is now printed below the table.

- Dot arguments (`...`) in
  [`hypothesis_test()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  are now passed to the functions in *marginaleffects*, thereby allowing
  to use further options in functions
  [`marginaleffects::predictions()`](https://rdrr.io/pkg/marginaleffects/man/predictions.html),
  like `transform` etc.

### Bug fixes

- Fixed issues in
  [`hypothesis_test()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md)
  for mixed models with one focal term only, and when this term was
  categorical.

## ggeffects 1.2.0

CRAN release: 2023-02-24

### Breaking

- Confidence intervals of adjusted predictions now take the model’s
  degrees of freedom into account, thereby leading to slightly larger
  intervals for models that do not have infinite degrees of freedom
  (like linear models with t-statistic).

### New functions

- [`hypothesis_test()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md),
  to compute contrasts and comparisons of predictions and test
  differences for statistical significance. Additionally, an
  accompanying vignette that explains the new function in detail is
  added.

- [`install_latest()`](https://strengejacke.github.io/ggeffects/reference/install_latest.md),
  to install the latest official package version from CRAN, or the
  latest development version from r-universe.

- An [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  method was added, which converts `ggeffects` objects returned by
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  into data frame, where standard column names are replaced by their
  related variable names.

### General

- Response values are now also back-transformed when these were
  transformed using [`log2()`](https://rdrr.io/r/base/Log.html),
  [`log10()`](https://rdrr.io/r/base/Log.html) or
  [`log1p()`](https://rdrr.io/r/base/Log.html).

- The `terms` argument can now also be a named list. Thus, instead of
  `terms = c("score [30,50,70]", "status [low, middle]")` one could also
  write
  `terms = list(score = c(30,50,70), status = c("low", "middle"))`.

## ggeffects 1.1.5

CRAN release: 2023-01-25

### General

- Minor changes to meet forthcoming update of *insight*.

- [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  or
  [`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  get a `verbose` argument to suppress some messages and warnings when
  calling

## ggeffects 1.1.4

CRAN release: 2022-10-23

### General

- Reduced package dependencies. Packages *sjlabelled* and *MASS* were
  moved from imports to suggests. *ggeffects* is now a very lightweight
  package to compute adjusted predictions and estimated marginal means.

### New supported models

- `logitr` (package **logitr**)

### Bug fixes

- Fixed issue with wrong standard errors for predicting random effect
  groups for more multiple levels.

- Fixed issue in
  [`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md),
  which did not correctly averaged over character vectors when these
  were hold constant.

- Fixed bug for models of class `lme` when `type = "re"` was requested.

## ggeffects 1.1.3

CRAN release: 2022-08-07

### Bug fixes

- Fix wrong computations of predictions for `arm::bayesglm()` models.

- Fix CRAN check issues.

## ggeffects 1.1.2

CRAN release: 2022-04-10

### General

- Speed improvement for some models when calculating uncertainty
  intervals of predictions.

- Minor fixes.

## ggeffects 1.1.1

CRAN release: 2021-07-29

### Changes to functions

- Add more informative error message for *brmsfit* models using `mo()`
  with numeric predictors, which only allow to predict for values that
  are actually present in the data.

### Bug fixes

- Fixed issue with adding raw data points for plots from logistic
  regression models, when the response variable was no factor with
  numeric levels.

- Fixed issues with CRAN checks.

## ggeffects 1.1.0

CRAN release: 2021-04-30

### New supported models

- `orm` (package **rms**)

### Breaking Changes

- Prediction intervals (where possible, or when `type = "random"`), are
  now always based on sigma^2 (i.e. `insight::get_sigma(model)^2`). This
  is in line with `interval = "prediction"` for *lm*, or for predictions
  based on simulations (when `type = "simulate"`).

- [`print()`](https://strengejacke.github.io/ggeffects/reference/print.md)
  now uses the name of the focal variable as column name (instead) of
  `"x"`).

### New function

- [`collapse_by_group()`](https://strengejacke.github.io/ggeffects/reference/collapse_by_group.md),
  to generate a data frame where the response value of the raw data is
  averaged over the levels of a (random effect) grouping factor.

### General

- A new vignette was added related to the definition and meaning of
  “marginal effects” and “adjusted predictions”. To be more strict and
  to avoid confusion with the term “marginal effect”, which meaning may
  vary across fields, either “marginal effects” was replaced by
  “adjusted predictions”, or “adjusted predictions” was added as term
  throughout the package’s documentation and vignettes.

- Allow confidence intervals when predictions are conditioned on random
  effect groups (i.e. when `type = "random"` and `terms` includes a
  random effect group factor).

- Predicted response values based on
  [`simulate()`](https://rdrr.io/r/stats/simulate.html) (i.e. when
  `type = "simulate"`) is now possible for more model classes (see
  [`?ggpredict`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)).

- [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  now computes confidence intervals for some edge cases where it
  previously failed (e.g. some models that do not compute standard
  errors for predictions, and where a factor was included in the model
  and not the focal term).

- [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  gains a `collapse.group` argument, which - in conjunction with
  `add.data` - averages (“collapses”) the raw data by the levels of the
  group factors (random effects).

- [`data_grid()`](https://strengejacke.github.io/ggeffects/reference/new_data.md)
  was added as more common alias for
  [`new_data()`](https://strengejacke.github.io/ggeffects/reference/new_data.md).

### Bug fixes

- [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  and
  [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  for survival-models now always start with time = 1.

- Fixed issue in
  [`print()`](https://strengejacke.github.io/ggeffects/reference/print.md)
  for survival-models.

- Fixed issue with `type = "simulate"` for `glmmTMB` models.

- Fixed issue with `gamlss` models that had `random()` function in the
  model formula.

- Fixed issue with incorrect back-transformation of predictions for
  `geeglm` models.

## ggeffects 1.0.2

CRAN release: 2021-03-17

### Breaking changes

- `residuals.type` argument in
  [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  is deprecated. Always using `"working"` residuals.

### General

- [`pretty_range()`](https://strengejacke.github.io/ggeffects/reference/pretty_range.md)
  and
  [`values_at()`](https://strengejacke.github.io/ggeffects/reference/values_at.md)
  can now also be used as function factories.

- [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  gains a `limit.range` argument, to limit the range of the prediction
  bands to the range of the data.

### Bug fixes

- Fixed issue with unnecessary back-transformation of log-transformed
  offset-terms from *glmmTMB* models.

- Fixed issues with plotting raw data when predictor on x-axis was a
  character vector.

- Fixed issues from CRAN checks.

## ggeffects 1.0.1

CRAN release: 2020-12-14

### General

- Fixed CRAN check issues.
- Added argument `interval` to
  [`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md),
  to either compute confidence or prediction intervals.

## ggeffects 1.0.0

CRAN release: 2020-11-29

### New supported models

- `averaging` (package **MuMIn**)

### New functions

- [`pool_predictions()`](https://strengejacke.github.io/ggeffects/reference/pool_predictions.md),
  to pool multiple `ggeffects` objects. This can be used when predicted
  values or estimated marginal means are calculated for models fit to
  multiple imputed datasets.

### General

- The function
  [`residualize_over_grid()`](https://strengejacke.github.io/ggeffects/reference/residualize_over_grid.md)
  is now exported.
- The back-transformation of the response-variable (if these were log-
  or square root-transformed in the model) now also works with square
  root-transformations and correctly handles
  [`log1p()`](https://rdrr.io/r/base/Log.html) and `log(mu + x)`.
- Since standard errors were on the link-scale and not back-transformed
  for non-Gaussian models, these are now no longer printed (to avoid
  confusion between standard errors on the link-scale and predictions
  and confidence intervals on the response-scale).

### Bug fixes

- Fixed issue for mixed models when predictions should be conditioned on
  random effects variances (e.g. `type = "random"` or `"zi_random"`),
  but random effects variances could not be calculated or were almost
  zero.
- Fixed issue with confidence intervals for `multinom` models in
  [`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md).
- Fixed issue in
  [`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  for models from *nlme*.
- Fixed issue with
  [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  for some models in
  [`ggeffect()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md).
- Fixed issue with computation of confidence intervals for zero-inflated
  models with offset-term.

## ggeffects 0.16.0

CRAN release: 2020-09-13

### Breaking changes

- Package *insight* since version 0.9.5 now returns the “raw”
  (untransformed, i.e. original) data that was used to fit the model
  also for log-transformed variables. Thus, exponentiation like using
  `terms = "predictor [exp]"` is no longer necessary.

### New supported models

- `mlogit` (package **mlogit**)

### General

- [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  now can also create partial residuals plots. There, arguments
  `residuals`, `residuals.type` and `residuals.line` were added to add
  partial residuals, the type of residuals and a possible loess-fit
  regression line for the residual data.

### Bug fixes

- The message for models with a back-transformation to the response
  scale (all non-Gaussian models), that standard errors are still on the
  link-scale, did not show up for models of class `glm` since some time.
  Should be fixed now.
- Fixed issue with
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  and `rlmerMods` models when using factors as adjusted terms.
- Fixed issue with brms-multi-response models.

## ggeffects 0.15.1

CRAN release: 2020-07-27

### New supported models

- `mclogit` (package **mclogit**)

### Bug fixes

- Fixed issues due to latest *rstanarm* update.
- Fixed some issues around categorical/cumulative *brms* models when the
  outcome is numeric.
- Fixed bug with factor level ordering when plotting raw data from
  [`ggeffect()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md).

## ggeffects 0.15.0

CRAN release: 2020-06-16

### Changes to functions

- [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  gets a new `type`-option, `"zi.prob"`, to predict the zero-inflation
  probability (for models from *pscl*, *glmmTMB* and *GLMMadaptive*).
- When model has log-transformed response variable and `add.data = TRUE`
  in
  [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md),
  the raw data points are also transformed accordingly.
- [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  with `add.data = TRUE` first adds the layer with raw data, then the
  points / lines for the marginal effects, so raw data points to not
  overlay the predicted values.
- The `terms`-argument now also accepts the name of a variable to define
  specific values. See vignette *Marginal Effects at Specific Values*.

### Bug fixes

- Fix issues in cluster-robust variance-covariance estimation when
  `vcov.type` was not specified.

## ggeffects 0.14.3

CRAN release: 2020-04-20

### General

- Fixed issues to due changes in other CRAN packages.

## ggeffects 0.14.2

CRAN release: 2020-03-14

### General

- *ggeffects* now requires *glmmTMB* version 1.0.0 or higher.
- Added human-readable alias-options to the `type`-argument.

### Bug fixes

- Fixed issue when log-transformed predictors where held constant and
  their typical value was negative.
- Fixed issue when plotting raw data to a plot with categorical
  predictor in the x-axis, which had numeric factor levels that did not
  start at `1`.
- Fixed issues for model objects that used (log) transformed
  [`offset()`](https://rdrr.io/r/stats/offset.html) terms.

## ggeffects 0.14.1

CRAN release: 2020-01-28

### General

- Reduce package dependencies.
- New package-vignette *(Cluster) Robust Standard Errors*.

### New supported models

- `mixor` (package **mixor**), `cgam`, `cgamm` (package **cgam**)

### Bug fixes

- Fix CRAN check issues due to latest *emmeans* update.

## ggeffects 0.14.0

CRAN release: 2019-12-16

### Breaking Changes

- The argument `x.as.factor` is considered as less useful and was
  removed.

### New supported models

- `fixest` (package **fixest**), `glmx` (package **glmx**).

### General

- Reduce package dependencies.
- `plot(rawdata = TRUE)` now also works for objects from
  [`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md).
- [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  now computes confidence intervals for predictions from `geeglm`
  models.
- For *brmsfit* models with `trials()` as response variable,
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  used to choose the median value of trials were the response was hold
  constant. Now, you can use the `condition`-argument to hold the number
  of trials constant at different values.
- Improve
  [`print()`](https://strengejacke.github.io/ggeffects/reference/print.md).

### Bug fixes

- Fixed issue with `clmm`-models, when group factor in random effects
  was numeric.
- Raw data is no longer omitted in plots when grouping variable is
  continuous and added raw data doesn’t numerically match the grouping
  levels (e.g., mean +/- one standard deviation).
- Fix CRAN check issues due to latest *geepack* update.

## ggeffects 0.13.0

CRAN release: 2019-11-08

### Breaking Changes

- The use of `emm()` is discouraged, and so it was removed.

### New supported models

- `bracl`, `brmultinom` (package **brglm2**) and models from packages
  **bamlss** and **R2BayesX**.

### General

- Updated package dependencies.
- [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  now uses dodge-position for raw data for categorical x-axis, to align
  raw data points with points and error bars geoms from predictions.
- Updated and re-arranged internal color palette, especially to have a
  better behaviour when selecting colors from continuous palettes (see
  `show_pals()`).

### New functions

- Added a
  [`vcov()`](https://strengejacke.github.io/ggeffects/reference/vcov.md)
  function to calculate variance-covariance matrix for marginal effects.

### Changes to Functions

- [`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  now also accepts `type = "re"` and `type = "re.zi"`, to add random
  effects variances to prediction intervals for mixed models.
- The ellipses-argument `...` is now passed down to the
  [`predict()`](https://rdrr.io/r/stats/predict.html)-method for
  *gamlss*-objects, so predictions can be computed for sigma, nu and tau
  as well.

### Bug fixes

- Fixed issue with wrong order of plot x-axis for
  [`ggeffect()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md),
  when one term was a character vector.

## ggeffects 0.12.0

CRAN release: 2019-09-03

### Breaking Changes

- The use of
  [`ggaverage()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  is discouraged, and so it was removed.
- The name `rprs_values()` is now deprecated, the function is named
  [`values_at()`](https://strengejacke.github.io/ggeffects/reference/values_at.md),
  and its alias is
  [`representative_values()`](https://strengejacke.github.io/ggeffects/reference/values_at.md).
- The `x.as.factor`-argument defaults to `TRUE`.

### General

- [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  now supports cumulative link and ordinal *vglm* models from package
  **VGAM**.
- More informative error message for *clmm*-models when `terms` included
  random effects.
- `add.data` is an alias for the `rawdata`-argument in
  [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md).
- [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  and
  [`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  now also support predictions for *gam* models from `ziplss` family.

### Changes to Functions

- Improved
  [`print()`](https://strengejacke.github.io/ggeffects/reference/print.md)-method
  for ordinal or cumulative link models.
- The
  [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)-method
  no longer changes the order of factor levels for groups and facets.
- `pretty_data()` gets a
  [`length()`](https://rdrr.io/r/base/length.html) argument to define
  the length of intervals to be returned.

### Bug fixes

- Added “population level” to output from print-method for *lme*
  objects.
- Fixed issue with correct identification of gamm/gamm4 models.
- Fixed issue with weighted regression models from *brms*.
- Fixed broken tests due to changes of forthcoming *effects* update.

## ggeffects 0.11.0

CRAN release: 2019-07-01

### General

- Revised docs and vignettes - the use of the term *average marginal
  effects* was replaced by a less misleading wording, since the
  functions of **ggeffects** calculate marginal effects at the mean or
  at representative values, but not average marginal effects.
- Replace references to internal vignettes in docstrings to
  website-vignettes, so links on website are no longer broken.
- [`values_at()`](https://strengejacke.github.io/ggeffects/reference/values_at.md)
  is an alias for `rprs_values()`.

### New supported models

- `betabin`, `negbin` (package **aod**), `wbm` (package *panelr*)

### Changes to functions

- [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  now supports prediction intervals for models from *MCMCglmm*.
- [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  gets a `back.transform`-argument, to tranform predicted values from
  log-transformed responses back to their original scale (the default
  behaviour), or to allow predictions to remain on log-scale (new).
- [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  and
  [`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  now can calculate marginal effects for specific values from up to
  three terms (i.e. `terms` can be of lenght four now).
- The `ci.style`-argument from
  [`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  now also applies to error bars for categorical variables on the
  x-axis.

### Bug fixes

- Fixed issue with *glmmTMB* models that included model weights.

## ggeffects 0.10.0

CRAN release: 2019-05-13

### General

- Better support, including confidence intervals, for some of the
  already supported model types.
- New package-vignette *Logistic Mixed Effects Model with Interaction
  Term*.

### New supported models

- `gamlss`, `geeglm` (package **geepack**), `lmrob` and `glmrob`
  (package **robustbase**), `ols` (package **rms**), `rlmer` (package
  **robustlmm**), `rq` and `rqss` (package **quantreg**), `tobit`
  (package **AER**), `survreg` (package **survival**)

### Changes to functions

- The steps for specifying a range of values
  (e.g. `terms = "predictor [1:10]"`) can now be changed with `by`,
  e.g. `terms = "predictor [1:10 by=.5]"` (see also vignette *Marginal
  Effects at Specific Values*).
- Robust standard errors for predictions (see argument `vcov.fun` in
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md))
  now also works for following model-objects: `coxph`, `plm`, `polr`
  (and probably also `lme` and `gls`, not tested yet).
- [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  gets an `interval`-argument, to compute prediction intervals instead
  of confidence intervals.
- [`plot.ggeffects()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
  now allows different horizontal and vertical jittering for `rawdata`
  when `jitter` is a numeric vector of length two.

### Bug fixes

- Models with `AsIs`-conversion from division of two variables as
  dependent variable, e.g. `I(amount/frequency)`, now should work.
- [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
  failed for `MixMod`-objects when `ci.lvl=NA`.
