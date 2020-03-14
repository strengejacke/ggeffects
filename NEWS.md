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

# ggeffects 0.9.0

## General

* Minor revisions to docs and vignettes.
* Reduce package dependencies.
* Better support, including confidence intervals, for some of the already supported model types.
* New package-vignette _Customize Plot Appearance_.

## New supported models

* `ggemmeans()` now supports `type = "fe.zi"` for **glmmTMB**-models, i.e. predicted values are conditioned on the fixed effects and the zero-inflation components of glmmTMB-models.
* `ggpredict()` now supports **MCMCglmm**, **ivreg** and **MixMod** (package **GLMMadaptive**) models.
* `ggemmeans()` now supports **MCMCglmm** and **MixMod** (package **GLMMadaptive**) models.
* `ggpredict()` now computes confidence intervals for **gam** models (package **gam**).

## New functions

* `new_data()`, to create a data frame from all combinations of predictor values. This data frame typically can be used for the `newdata`-argument in `predict()`, in case it is necessary to quickly create an own data frame for this argument.

## Changes to functions

* `ggpredict()` no longer stops when predicted values with confidence intervals for **glmmTMB**- and other zero-inflated models can't be computed with `type = "fe.zi"`, and only returns the predicted values without confidence intervals.
* When `ggpredict()` fails to compute confidence intervals, a more informative error message is given.
* `plot()` gets a `connect.lines`-argument, to connect dots from plots with discrete x-axis.

## Bug fixes

* `ggpredict()` did not work with **glmmTMB**- and other zero-inflated models, when `type = "fe.zi"` and model- or zero-inflation formula had a polynomial term that was held constant (i.e. not part of the `terms`-argument).
* Confidence intervals for zero-inflated models and `type = "fe.zi"` could not be computed when the model contained polynomial terms and a _very_ long formula (issue with `deparse()`, cutting off very long formulas).
* The `plot()`-method put different spacing between groups when a numeric factor was used along the x-axis, where the factor levels where non equal-spaced.
* Minor fixes regarding calculation of predictions from some already supported models
* Fixed issues with multiple response models of class `lm` in `ggeffects()`.
* Fixed issues with encoding in help-files.

# ggeffects 0.8.0

## General

* Minor changes to meet forthcoming changes in purrr.
* For consistency reasons, both `type = "fe"` and `type = "re"` return population-level predictions for mixed effects models (**lme4**, **glmmTMB**). The difference is that `type = "re"` also takes the random effect variances for prediction intervals into account. Predicted values at specific levels of random effect terms is described in the package-vignettes _Marginal Effects for Random Effects Models_ and _Marginal Effects at Specific Values_.
* Revised docs and vignettes.
* Give more informative warning for misspelled variable names in `terms`-argument.
* Added custom (pre-defined) color-palettes, that can be used with `plot()`. Use `show_pals()` to show all available palettes.
* Use more appropriate calculation for confidence intervals of predictions for model with zero-inflation component.

## New supported models

* `ggpredict()` and `ggeffect()` now support **brms**-models with additional response information (like `trial()`).
* `ggpredict()` now supports **Gam**, **glmmPQL**, **clmm**, and **zerotrunc**-models.
* All models supported by the **emmeans** should also work with the new `ggemmeans()`-function. Since this function is quite new, there still might be some bugs, though.

## New functions

* `ggemmeans()` to compute marginal effects by calling `emmeans::emmeans()`.
* `theme_ggeffects()`, which can be used with `ggplot2::theme_set()` to set the **ggeffects**-theme as default plotting theme. This makes it easier to add further theme-modifications like `sjPlot::legend_style()` or `sjPlot::font_size()`.

## Changes to functions

* Added prediction-type based on simulations (`type = "sim"`) to `ggpredict()`, currently for models of class **glmmTMB** and **merMod**.
* `x.cat` is a new alias for the argument `x.as.factor`.
* The `plot()`-method gets a `ci.style`-argument, to define different styles for the confidence bands for numeric x-axis-terms.
* The `print()`-method gets a `x.lab`-argument to print value labels instead of numeric values if `x` is categorical.
* `emm()` now also supports all prediction-types, like `ggpredict()`.

## Bug fixes

* Fixed issue where confidence intervals could not be computed for variables with very small values, that differ only after the second decimal part.
* Fixed issue with `ggeffect()`, which did not work if data had variables with more that 8 digits (fractional part longer than 8 numbers).
* Fixed issue with multivariate response models fitted with **brms** or **rstanarm** when argument `ppd = TRUE`.
* Fixed issue with glmmTMB-models for `type = "fe.zi"`, which could mess up the correct order of predicted values for `x`.
* Fixed minor issue with glmmTMB-models for `type = "fe.zi"` or `type = "re.zi"`, when first terms had the `[all]`-tag.
* Fixed minor issue in the `print()`-method for mixed effects models, when predictions were conditioned on all model terms and adjustment was only done for random effects (output-line "adjusted for").
* Fixed issue for mixed models, where confidence intervals were not completely calculated, if `terms` included a factor and `contrasts` were set to other values than `contr.treatment`.
* Fixed issue with messed up order of confidence intervals for `glm`-object and heteroskedasticity-consistent covariance matrix estimation.
* Fixed issue for **glmmTMB**-models, when variables in dispersion or zero-inflation formula did not appear in the fixed effects formula.
* The `condition`-argument was not always considered for some model types when calculating confidence intervals for predicted values.

