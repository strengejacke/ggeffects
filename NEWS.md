# ggeffects 0.7.1

## General

* For consistency reasons, both `type = "fe"` and `type = "re"` return population-level predictions for mixed effects models (**lme4**, **glmmTMB**). The difference is that `type = "re"` also takes the random effect variances for prediction intervals into account. Predicted values at specific levels of random effect terms is described in the package-vignette _Marginal Effects at Specific Values_.
* Revised docs and vignettes.
* Give more informative warning for misspelled variable names in `terms`-argument.

## New functions

* `theme_ggeffects()`, which can be used with `ggplot2::theme_set()` to set the **ggeffects**-theme as default plotting theme. This makes it easier to add further theme-modifications like `sjPlot::legend_style()` or `sjPlot::font_size()`.

## Changes to functions

* The `plot()`-method gets a `ci.style`-argument, to define different styles for the confidence bands for numeric x-axis-terms.

## Bug fixes

* Fixed issue where confidence intervals could not be computed for variables with very small values, that differ only after the second decimal part.
* Fixed issue with `ggeffect()`, which did not work if data had variables with more that 8 digits (fractional part longer than 8 numbers).
* Fixed issue with multivariate response models fitted with **brms** or **rstanarm** when argument `ppd = TRUE`.
* Fixed issue with glmmTMB-models for `type = "fe.zi"`, which could mess up the correct order of predicted values for `x`.
* Fixed minor issue with glmmTMB-models for `type = "fe.zi"` or `type = "re.zi"`, when first terms hat the `[all]`-tag.
* Fixed minor issue in the `print()`-method for mixed effects models, when predictions were conditioned on all model terms and adjustment was only done for random effects (output-line "adjusted for").
* Fixed issue for mixed models, where confidence intervals were not completely calculated, if `terms` included a factor and `contrasts` were set to other values than `contr.treatment`.

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

# ggeffects 0.4.0

## General

* Removed alias names `mem()`, `eff()` and `ame()`.
* For mixed models (packages **lme4**, **nlme**, **glmmTMB**), the uncertainty of the random effect variances is now taken into account when `type = "re"`.
* Computing confidence intervals for mixed models should be much more memory efficient now, resulting less often in warnings about memory allocation problems.
* Updated reference in `CITATION` to the publication in the Journal of Open Source Software.
* A test-suite was added to the package.

## New functions

* `pretty_range()`, to create a pretty sequence of integers of a vector.

## Changes to functions

* `ggpredict()` gets a `condition`-argument to specify values at which covariates should be held constant, instead of their `typical` value.
* The `pretty`-option for `ggpredict()` now calculates more values, leading to smoother plots.
* The `terms`-argument in `ggpredict()` can now also select a range of feasible values for numeric values, e.g. `terms = "age [pretty]"`. In contrast to the `pretty`-argument, which prettyfies all terms, you can selectively prettify specific terms with this option.
* The `terms`-argument in `ggpredict()` now also supports all shortcuts that are possible for the `mdrt.values`-argument in `gginteraction()`, so for instance `term = "age [meansd]"` would return three values: mean(age) - sd(age), mean(age) and mean(age) + sd(age).
* `plot()` gets some new arguments to control which plot-title to show or hide: `show.title`, `show.x.title` and `show.y.title`.
* `plot()` gets a `log.y` argument to transform the y-axis to logarithmic scale, which might be useful for binomial models with predicted probabilities, or other models with log-alike link-functions.
* The `plot()`-method for plotting all effects with `ggpredict()` (when `term = NULL`) now allows to arrange the plot in facets (using `facets = TRUE`).
* Values in dot-argument for `plot()` are now passed down to `ggplot::scale_y*()`, to control the appearance of the y-axis (like `breaks`).

## Bug fixes

* Fixed issue with binomial models that used `cbind(...)` as response variable.
* Fixed issue with suboptimal precision of confidence resp. prediction intervals for mixed models (packages **lme4**, **nlme**), which are now more accurate.

# ggeffects 0.3.4

## General

* Prediction for `glmmTMB`-objects now compute proper confidence intervals, due to fix in package _glmmTMB_ 0.2.1
* If `terms` in `ggpredict()` is missing or `NULL`, marginal effects for each model term are calculated. `ggpredict()` then returns a list of data frames, which can also be plotted with `plot()`.

## Changes to functions

* The `jitter`-argument from `plot()` now accepts a numeric value between 0 and 1, to control the width of the random variation in data points.
* `ggpredict()` and `ggeffect()` can now predict transformed values, which is useful, for instance, to exponentiate predictions for `log(term)` on the original scale of the variable. See package vignette, section _Marginal effects at specific values or levels_ for examples.

## Bug fixes

* Multivariate response models in _brms_ with variable names with underscores and dots were not correctly plotted.

# ggeffects 0.3.3

## General

* Better support for multivariate-response-models from _brms_.
* Support for cumulative-link-models from _brms_.
* `ggpredict()` now supports linear multivariate response models, i.e. `lm()` with multiple outcomes.

## Changes to functions

* `ggpredict()` gets a `pretty`-argument to reduce and "prettify" the value range from variables in `terms` for predictions. This applies to all variables in `terms` with more than 25 unique values.

## Bug fixes

* Recognize negative binomial family from `brmsfit`-models.

# ggeffects 0.3.2

## General

* `ggpredict()`, `ggeffect()` and `gginteraction()` get a `x.as.factor`-argument to preserve factor-class for the `x`-column in the returned data frame.
* The `terms`-argument now also allows the specification of a range of numeric values in square brackets, e.g. `terms = "age [30:50]"`.

## Bug fixes

* Give proper warning that `clm`-models don't support `full.data`-argument.
* `emm()` did not work properly for some random effects models.

# ggeffects 0.3.1

## General

* Use `convert_case()` from *sjlabelled*, in preparation for the latest *snakecase*-package update.

## Bug fixes

* Model weights are now correctly taken into account.
