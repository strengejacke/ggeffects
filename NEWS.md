# ggeffects 1.3.0

## Breaking

* To avoid confusion when adding raw data or residuals to plots, the `jitter`
  argument that is used to add some noice to data points to avoid overlapping
  now defaults to `NULL`. Formerly, a small jitter was added by default,
  leading to confusion when data points did not match the original data.

## Changes

* The `plot()` method gets a `label.data` argument, to add row names to data
  points when `add.data = TRUE`.

* `tibbles` are always converted into data frames, to avoid issues.

* `hypothesis_test()` gains a `by` argument, to specify a variable that is used
  to group the comparisons or contrasts. This is useful for models with interaction
  terms.

## Bug fixes

* Plotting residuals did not work when model object passed to `ggpredict()`
  were inside a list, or when called from inside functions (scoping issues).

* Fixed issue where plotting raw data (i.e. `plot(..., add.data = TRUE)`) did
  not work when there were missing data in weight variables (i.e. when the
  regression model used weights).

* Fixes issue in `plot()` when no term was specified in the call to `ggpredict()`.

* Fixed issues with robust estimation for models of package *pscl*.

# ggeffects 1.2.3

## General

* Support for `nestedLogit` (*nestedLogit*) models.

* `hyothesis_test()` gains a `scale` argument, to explicitely modulate the scale
  of the contrasts or comparisons (e.g. `"response"` or `"link"`, or `"exp"` to
  return transformed contrasts/comparisons).

* `hyothesis_test()` now includes the response level for models with ordinal
  outcomes (and alike).

* When `ggpredict()` is used inside functions and a name for a vector variable
  (passed as argument to that function) in `terms` is used, the variable is
  now correctly recognized.

* Partial residuals (when `plot(..., residuals = TRUE)`) now supports more
  linear (mixed) models, including models from package *lme* (such as `gls()`
  or `lme()`).

* For mixed models, `type = "random"` used to calculate _prediction intervals_
  that always accounted for random effects variances, leading to larger intervals.
  Using `interval = "confidence"` together with `type = "random"` now allows to
  calculate "usual" confidence intervals for random effects. This is usefule for
  predictions at specific group levels of random effects (when focal terms are
  only fixed effects, use `type = "fixed"` for regular confidence intervals).

* The `vcov.fun` argument can now also be a function that returns a
  variance-covariance matrix.

* The `verbose` argument in `ggpredict()` and `hypothesis_test()` now also toggle
  messages for the respective `print()` methods.

* The `print()` method for `hypothesis_test()` has been revised and now provides
  more details for possible transformation of the scale of comparisons and
  contrasts.

* The `print()` method now shows all rows by default when the focal term is
  a factor. If rows are not shown in the output, a message is printed to inform
  the user about truncated output.

* A new vignette about using *ggeffects* in the context of an intersectional
  multilevel analysis of individual heterogeneity, using the MAIHDA framework.

## Bug fixes

* Fixed issue with wrong order of x-axis-labels for plots when the focal term
  on the x-axis was a character vector, where alphabetical order of values did
  not match order of predictions.

* Fixed issues in `hyothesis_test()` for models with ordinal outcomes (and alike).

# ggeffects 1.2.2

## General

* Added a new `[.ggeffects` function, which allows to subset `ggeffects` objects
  in the same way as regular data frames, i.e. it is now possible to do:
  ```
  gge <- ggpredict(model, "x1")
  gge[c(1:2)]
  ```

* Using a name for a vector variable in `terms` now works from inside functions.
  E.g., you can now do:
  ```
  foo <- function(data) {
    fit <- lm(barthtot ~ c12hour + c172code, data = data)
    v <- c(20, 50, 70)
    ggpredict(fit, terms = "c12hour [v]")
  }
  foo(efc)
  ```

* The `colors` argument in `plot()` can now also be applied to single-colored
  plots.

* `hyothesis_test()` gains a `collapse_level` argument, to collapse term labels
  that refer to the same levels into a singel unique level string.

## Bug fixes

* Fixed issue with misplaced residuals when x-axis was categorical and the
  factor levels were not in alphabetical order.

* `pool_predictions()` now correctly handles models with transformed response
  variables (like `log(y)`) and returns the correct back-transformed pooled
  predictions (and their confidence intervals).

* Fixed issue with wrong computation of confidence intervals for models of class
  `clm` from package *ordinal*.

* Fixed failing tests due to changes in the *logistf* package, which now also
  supports *emmeans*. That means, `ggemmeans()` now also works for models from
  package *logistf*.

* Fixed bug in `plot()` when partial residuals were added (i.e. `residuals = TRUE`)
  and `collapse.group` was provided (in case of mixed models).

* Fixed issue with on-the-fly created factors inside formulas, which were not
  correctly treated as factors in the `plot()` method. This bug was related to
  recent changes in `insight::get_data()`.

* Fixed issue with wrong labels in `hyothesis_test()` for comparisons with many
  rows, when betas starting with same digit were specified (e.g.
  `test = "(b1-b13)=(b3-b15)"`).

* Fixed issue in `hyothesis_test()` for mixed models when focal terms included
  factors with factor levels that contained a comma.

* Fixed issue with missing confidence intervals for mixed models when one of
  the variable names contains white space characters (e.g. `y ~ 'x a' + xb`).

# ggeffects 1.2.1

## General

* Support for `mblogit` (*mclogit*), `phylolm` and `phyloglm` (*phylolm*) models.

## Changes to functions

* `hypothesis_test()` gains an `equivalence` argument, to compute tests of
  practical equivalence for contrasts and comparisons.

* The message whether contrasts or comparisons from `hypothesis_test()` are on
  the link-scale is now printed below the table.

* Dot arguments (`...`) in `hypothesis_test()` are now passed to the functions
  in *marginaleffects*, thereby allowing to use further options in functions
  `marginaleffects::predictions()`, like `transform_post` etc.

## Bug fixes

* Fixed issues in `hypothesis_test()` for mixed models with one focal term only,
  and when this term was categorical.

# ggeffects 1.2.0

## Breaking

* Confidence intervals of adjusted predictions now take the model's degrees of
  freedom into account, thereby leading to slightly larger intervals for models
  that do not have infinite degrees of freedom (like linear models with
  t-statistic).

## New functions

* `hypothesis_test()`, to compute contrasts and comparisons of predictions and
  test differences for statistical significance. Additionally, an accompanying
  vignette that explains the new function in detail is added.

* `install_latest()`, to install the latest official package version from
  CRAN, or the latest development version from r-universe.

* An `as.data.frame()` method was added, which converts `ggeffects` objects
  returned by `ggpredict()` into data frame, where standard column names are
  replaced by their related variable names.

## General

* Response values are now also back-transformed when these were transformed
  using `log2()`, `log10()` or `log1p()`.

* The `terms` argument can now also be a named list. Thus, instead of
  `terms = c("score [30,50,70]", "status [low, middle]")` one could also write
  `terms = list(score = c(30,50,70), status = c("low", "middle"))`.

# ggeffects 1.1.5

## General

* Minor changes to meet forthcoming update of _insight_.

* `ggpredict()` or `ggemmeans()` get a `verbose` argument to suppress some
  messages and warnings when calling

# ggeffects 1.1.4

## General

* Reduced package dependencies. Packages *sjlabelled* and *MASS* were moved
  from imports to suggests. *ggeffects* is now a very lightweight package to
  compute adjusted predictions and estimated marginal means.

## New supported models

* `logitr` (package **logitr**)

## Bug fixes

* Fixed issue with wrong standard errors for predicting random effect groups
  for more multiple levels.

* Fixed issue in `ggemmeans()`, which did not correctly averaged over character
  vectors when these were hold constant.

* Fixed bug for models of class `lme` when `type = "re"` was requested.

# ggeffects 1.1.3

## Bug fixes

* Fix wrong computations of predictions for `arm::bayesglm()` models.

* Fix CRAN check issues.

# ggeffects 1.1.2

## General

* Speed improvement for some models when calculating uncertainty intervals of
  predictions.
  
* Minor fixes.

# ggeffects 1.1.1

## Changes to functions

* Add more informative error message for *brmsfit* models using `mo()` with 
  numeric predictors, which only allow to predict for values that are actually
  present in the data.

## Bug fixes

* Fixed issue with adding raw data points for plots from logistic regression
  models, when the response variable was no factor with numeric levels.

* Fixed issues with CRAN checks.

# ggeffects 1.1.0

## New supported models

* `orm` (package **rms**)

## Breaking Changes

* Prediction intervals (where possible, or when `type = "random"`), are now
  always based on sigma^2 (i.e. `insight::get_sigma(model)^2`). This is in
  line with `interval = "prediction"` for *lm*, or for predictions based on
  simulations (when `type = "simulate"`).

* `print()` now uses the name of the focal variable as column name (instead)
  of `"x"`).

## New function

* `collapse_by_group()`, to generate a data frame where the response value of
  the raw data is averaged over the levels of a (random effect) grouping factor.

## General

* A new vignette was added related to the definition and meaning of "marginal
  effects" and "adjusted predictions". To be more strict and to avoid confusion
  with the term "marginal effect", which meaning may vary across fields, either
  "marginal effects" was replaced by "adjusted predictions", or "adjusted
  predictions" was added as term throughout the package's documentation and
  vignettes.

* Allow confidence intervals when predictions are conditioned on random effect
  groups (i.e. when `type = "random"` and `terms` includes a random effect
  group factor).

* Predicted response values based on `simulate()` (i.e. when 
  `type = "simulate"`) is now possible for more model classes 
  (see `?ggpredict`).

* `ggpredict()` now computes confidence intervals for some edge cases where
  it previously failed (e.g. some models that do not compute standard errors
  for predictions, and where a factor was included in the model and not the
  focal term).

* `plot()` gains a `collapse.group` argument, which - in conjunction with
  `add.data` - averages ("collapses") the raw data by the levels of the
  group factors (random effects).

* `data_grid()` was added as more common alias for `new_data()`.

## Bug fixes

* `ggpredict()` and `plot()` for survival-models now always start with time = 1.

* Fixed issue in `print()` for survival-models.

* Fixed issue with `type = "simulate"` for `glmmTMB` models.

* Fixed issue with `gamlss` models that had `random()` function in the
  model formula.

* Fixed issue with incorrect back-transformation of predictions for 
  `geeglm` models.

# ggeffects 1.0.2

## Breaking changes

* `residuals.type` argument in `plot()` is deprecated. Always using `"working"` residuals.

## General

* `pretty_range()` and `values_at()` can now also be used as function factories.

* `plot()` gains a `limit.range` argument, to limit the range of the prediction bands to the range of the data.

## Bug fixes

* Fixed issue with unnecessary back-transformation of log-transformed offset-terms from *glmmTMB* models.

* Fixed issues with plotting raw data when predictor on x-axis was a character vector.

* Fixed issues from CRAN checks.

# ggeffects 1.0.1

## General

* Fixed CRAN check issues.
* Added argument `interval` to `ggemmeans()`, to either compute confidence or prediction intervals.

# ggeffects 1.0.0

## New supported models

* `averaging` (package **MuMIn**)

## New functions

* `pool_predictions()`, to pool multiple `ggeffects` objects. This can be used when predicted values or estimated marginal means are calculated for models fit to multiple imputed datasets.

## General

* The function `residualize_over_grid()` is now exported.
* The back-transformation of the response-variable (if these were log- or square root-transformed in the model) now also works with square root-transformations and correctly handles `log1p()` and `log(mu + x)`.
* Since standard errors were on the link-scale and not back-transformed for non-Gaussian models, these are now no longer printed (to avoid confusion between standard errors on the link-scale and predictions and confidence intervals on the response-scale).

## Bug fixes

* Fixed issue for mixed models when predictions should be conditioned on random effects variances (e.g. `type = "random"` or `"zi_random"`), but random effects variances could not be calculated or were almost zero.
* Fixed issue with confidence intervals for `multinom` models in `ggemmeans()`.
* Fixed issue in `ggemmeans()` for models from *nlme*.
* Fixed issue with `plot()` for some models in `ggeffect()`.
* Fixed issue with computation of confidence intervals for zero-inflated models with offset-term.

# ggeffects 0.16.0

## Breaking changes

* Package _insight_ since version 0.9.5 now returns the "raw" (untransformed, i.e. original) data that was used to fit the model also for log-transformed variables. Thus, exponentiation like using `terms = "predictor [exp]"` is no longer necessary.

## New supported models

* `mlogit` (package **mlogit**)

## General

* `plot()` now can also create partial residuals plots. There, arguments `residuals`, `residuals.type` and `residuals.line` were added to add partial residuals, the type of residuals and a possible loess-fit regression line for the residual data.

## Bug fixes

* The message for models with a back-transformation to the response scale (all non-Gaussian models), that standard errors are still on the link-scale, did not show up for models of class `glm` since some time. Should be fixed now.
* Fixed issue with `ggpredict()` and `rlmerMods` models when using factors as adjusted terms.
* Fixed issue with brms-multi-response models.

# ggeffects 0.15.1

## New supported models

* `mclogit` (package **mclogit**)

## Bug fixes

* Fixed issues due to latest *rstanarm* update.
* Fixed some issues around categorical/cumulative *brms* models when the outcome is numeric.
* Fixed bug with factor level ordering when plotting raw data from `ggeffect()`.

# ggeffects 0.15.0

## Changes to functions

* `ggpredict()` gets a new `type`-option, `"zi.prob"`, to predict the zero-inflation probability (for models from *pscl*, *glmmTMB* and *GLMMadaptive*).
* When model has log-transformed response variable and `add.data = TRUE` in `plot()`, the raw data points are also transformed accordingly.
* `plot()` with `add.data = TRUE` first adds the layer with raw data, then the points / lines for the marginal effects, so raw data points to not overlay the predicted values.
* The `terms`-argument now also accepts the name of a variable to define specific values. See vignette _Marginal Effects at Specific Values_.

## Bug fixes

* Fix issues in cluster-robust variance-covariance estimation when `vcov.type` was not specified.

# ggeffects 0.14.3

## General

* Fixed issues to due changes in other CRAN packages.

# ggeffects 0.14.2

## General

* *ggeffects* now requires _glmmTMB_ version 1.0.0 or higher.
* Added human-readable alias-options to the `type`-argument.

## Bug fixes

* Fixed issue when log-transformed predictors where held constant and their typical value was negative.
* Fixed issue when plotting raw data to a plot with categorical predictor in the x-axis, which had numeric factor levels that did not start at `1`.
* Fixed issues for model objects that used (log) transformed `offset()` terms.

# ggeffects 0.14.1

## General

* Reduce package dependencies.
* New package-vignette _(Cluster) Robust Standard Errors_.

## New supported models

* `mixor` (package **mixor**), `cgam`, `cgamm` (package **cgam**)

## Bug fixes

* Fix CRAN check issues due to latest *emmeans* update.

# ggeffects 0.14.0

## Breaking Changes

* The argument `x.as.factor` is considered as less useful and was removed.

## New supported models

* `fixest` (package **fixest**), `glmx` (package **glmx**).

## General

* Reduce package dependencies.
* `plot(rawdata = TRUE)` now also works for objects from `ggemmeans()`.
* `ggpredict()` now computes confidence intervals for predictions from `geeglm` models.
* For *brmsfit* models with `trials()` as response variable, `ggpredict()` used to choose the median value of trials were the response was hold constant. Now, you can use the `condition`-argument to hold the number of trials constant at different values.
* Improve `print()`.

## Bug fixes

* Fixed issue with `clmm`-models, when group factor in random effects was numeric.
* Raw data is no longer omitted in plots when grouping variable is continuous and added raw data doesn't numerically match the grouping levels (e.g., mean +/- one standard deviation).
* Fix CRAN check issues due to latest *geepack* update.

# ggeffects 0.13.0

## Breaking Changes

* The use of `emm()` is discouraged, and so it was removed.

## New supported models

* `bracl`, `brmultinom` (package **brglm2**) and models from packages **bamlss** and **R2BayesX**.

## General

* Updated package dependencies.
* `plot()` now uses dodge-position for raw data for categorical x-axis, to align raw data points with points and error bars geoms from predictions.
* Updated and re-arranged internal color palette, especially to have a better behaviour when selecting colors from continuous palettes (see `show_pals()`).

## New functions

* Added a `vcov()` function to calculate variance-covariance matrix for marginal effects.

## Changes to Functions

* `ggemmeans()` now also accepts `type = "re"` and `type = "re.zi"`, to add random effects variances to prediction intervals for mixed models.
* The ellipses-argument `...` is now passed down to the `predict()`-method for *gamlss*-objects, so predictions can be computed for sigma, nu and tau as well.

## Bug fixes

* Fixed issue with wrong order of plot x-axis for `ggeffect()`, when one term was a character vector.

# ggeffects 0.12.0

## Breaking Changes

* The use of `ggaverage()` is discouraged, and so it was removed.
* The name `rprs_values()` is now deprecated, the function is named `values_at()`, and its alias is `representative_values()`.
* The `x.as.factor`-argument defaults to `TRUE`.

## General

* `ggpredict()` now supports cumulative link and ordinal *vglm* models from package **VGAM**.
* More informative error message for *clmm*-models when `terms` included random effects.
* `add.data` is an alias for the `rawdata`-argument in `plot()`.
* `ggpredict()` and `ggemmeans()` now also support predictions for *gam* models from `ziplss` family.

## Changes to Functions

* Improved `print()`-method for ordinal or cumulative link models.
* The `plot()`-method no longer changes the order of factor levels for groups and facets.
* `pretty_data()` gets a `length()` argument to define the length of intervals to be returned.

## Bug fixes

* Added "population level" to output from print-method for *lme* objects.
* Fixed issue with correct identification of gamm/gamm4 models.
* Fixed issue with weighted regression models from *brms*.
* Fixed broken tests due to changes of forthcoming *effects* update.

# ggeffects 0.11.0

## General

* Revised docs and vignettes - the use of the term _average marginal effects_ was replaced by a less misleading wording, since the functions of **ggeffects** calculate marginal effects at the mean or at representative values, but not average marginal effects.
* Replace references to internal vignettes in docstrings to website-vignettes, so links on website are no longer broken.
* `values_at()` is an alias for `rprs_values()`.

## New supported models

* `betabin`, `negbin` (package **aod**), `wbm` (package *panelr*)

## Changes to functions

* `ggpredict()` now supports prediction intervals for models from *MCMCglmm*.
* `ggpredict()` gets a `back.transform`-argument, to tranform predicted values from log-transformed responses back to their original scale (the default behaviour), or to allow predictions to remain on log-scale (new).
* `ggpredict()` and `ggemmeans()` now can calculate marginal effects for specific values from up to three terms (i.e. `terms` can be of lenght four now).
* The `ci.style`-argument from `plot()` now also applies to error bars for categorical variables on the x-axis.

## Bug fixes

* Fixed issue with *glmmTMB* models that included model weights.

# ggeffects 0.10.0

## General

* Better support, including confidence intervals, for some of the already supported model types.
* New package-vignette _Logistic Mixed Effects Model with Interaction Term_.

## New supported models

* `gamlss`, `geeglm` (package **geepack**), `lmrob` and `glmrob` (package **robustbase**), `ols` (package **rms**), `rlmer` (package **robustlmm**), `rq` and `rqss` (package **quantreg**), `tobit` (package **AER**), `survreg` (package **survival**)

## Changes to functions

* The steps for specifying a range of values (e.g. `terms = "predictor [1:10]"`) can now be changed with `by`, e.g. `terms = "predictor [1:10 by=.5]"` (see also vignette _Marginal Effects at Specific Values_).
* Robust standard errors for predictions (see argument `vcov.fun` in `ggpredict()`) now also works for following model-objects: `coxph`, `plm`, `polr` (and probably also `lme` and `gls`, not tested yet).
* `ggpredict()` gets an `interval`-argument, to compute prediction intervals instead of confidence intervals.
* `plot.ggeffects()` now allows different horizontal and vertical jittering for `rawdata` when `jitter` is a numeric vector of length two.

## Bug fixes

* Models with `AsIs`-conversion from division of two variables as dependent variable, e.g. `I(amount/frequency)`, now should work.
* `ggpredict()` failed for `MixMod`-objects when `ci.lvl=NA`.
