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

# Bug fixes

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

# ggeffects 0.7.0

## General

* Support for monotonic predictors in **brms** models (`mo()`).
* For generalized additive models, values for splines are no longer automatically prettified (which ensures smooth plots, without the need to use the `[all]` tag, i.e. `terms="... [all]"`).
* If splines or plolynomial terms are used, a message is printed to indicate that using the `[all]` tag, i.e. `terms="... [all]"`, will produce smoother plots.
* The package-vignette _Marginal Effects at Specific Values_ now has examples on how to get marginal effects for each group level of random effects in mixed models.
* Revised `print()`-method, that - for larger data frames - only prints representative data rows. Use the `n`-argument inside the `print()`-method to force a specific number of rows to be printed.

## Changes to functions

* Added an `n`-tag for the `terms`-argument in `ggpredict()` and `ggeffect()`, to give more flexibility according to how many values are used for "prettifying" large value ranges.
* Added a `sample`-tag for the `terms`-argument in `ggpredict()` and `ggeffect()`, to pick a random sample of values for plotting.
* `ggpredict()` and `ggeffect()` now also return the standard error of predictions, if available.
* The `jitter`-argument in `plot()` now also changes the amount of noise for plots of models with binary outcome (when `rawdata = TRUE`).

## Bug fixes

* Fix issue with proper calculation of random effect variances for **glmmTMB** models for `type="re"` and `type="re.zi"` in general, and also for models with `ar1` random effects structure.

# ggeffects 0.6.0

## General

* Reduce package dependencies.
* Moved package **effects** from dependencies to suggested packages, due to the restrictive requirements (R >= 3.5).
* New `print()`-method, with a nicer print of the returned data frame. This method replaces the `summary()`-method, which was removed.
* `ggeffect()` now supports `clm2`-models from the **ordinal**-package.
* `ggpredict()` has improved support for `coxph`-models from the **survival**-package (survival probabilities, cumulative hazards).

## Changes to functions

* The `type`-argument in `ggpredict()` now has additional options, `type = "fe.zi"` and `type = "re.zi"`, to explicitely condition zero-inflated (mixed) models on their zero-inflation component.
* The `type`-argument in `ggpredict()` now has additional options, `type = "surv"` and `type = "cumhaz"`, to plot probabilities of survival or cumulative hazards from `coxph`-models.
* `ggpredict()` gets arguments `vcov.fun`, `vcov.type` and `vcov.args` to calculate robust standard errors for confidence intervals of predicted values. These are based on the various `sandwich::vcov*()`-functions, hence robust standard errors can be calculated for all models that are supported by `sandwich::vcov*()`.
* The `plot()`-method gets two arguments `line.size` and `dot.size`, to determine the size of the geoms.
* The `ci`-argument for the `plot()`-method now also accepts the character values `"dash"` and `"dot"` to plot dashed or dotted lines as confidence bands.
* The `terms`-argument in `ggpredict()` and `ggeffect()` may also be a formula, which is more convenient for typing, but less flexible than specifying the terms as character vector with specific options.

## Bug fixes

* Fixed improper calculation of confidence intervals for hurdle- and zero-inflated models (from package **pscl**), which could exceed the range of plausible values (e.g. below zero for incidence rates).
* Fixed issues with calculation of confidence intervals for mixed models with polynomial terms.

# ggeffects 0.5.0

## General

* New vignette _Different Output between Stata and ggeffects_.

## Changes to functions

* `ggpredict()` now automatically back-transforms predictions to the response scale for model with log-transformed response.
* `ggeffect()` and `ggpredict()` now automatically set numeric vectors with 10 or more unique values to representative values (see `rprs_values()`), if these are used as second or third value in the `terms`-argument (to represent a grouping structure).
* Fix memory allocation issue in `ggeffect()`.
* `rprs_values()` is now exported.
* The `pretty`-argument is deprecated, because prettifying values almost always makes sense - so this is done automatically.
* `ggpredict()` now supports `brmsfit`-objects with categorical-family.
* `ggalleffect()` has been removed. `ggeffect()` now plots effects for all model terms if `terms = NULL`.
* `gginteraction()` and `ggpoly()` have been removed, as `ggpredict()` and `ggeffect()` are more efficient and generic for plotting interaction or polynomial terms.

## Bug fixes

* Fix issues with categorical or ordinal outcome models (`polr`, `clm`, `multinom`) for `ggeffect()`.
* Fix issues with confidence intervals for mixed models with log-transformed response value.
* Fix issues with confidence intervals for generalized mixed models when response value was a rate or proportion created with `cbind()` in model formula.
