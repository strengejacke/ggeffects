# ggeffects 0.2.1

## General

* Support for `stanreg` models (pkg _rstanarm_).

## Bug fixes

* Plotting raw data with `plot()` did not work for predictions at specific values (i.e. when certain levels of predictor where selected in square brackets). 

# ggeffects 0.2.0

## General

* Updated package imports and dependencies.
* Support for `polr` models (pkg _MASS_).
* Support for `hurdle` and `zeroinfl` models (pkg _pscl_).
* Support for `betareg` models (pkg _betareg_).
* Support for `truncreg` models (pkg _truncreg_).
* Support for `coxph` models (pkg _survival_).

## New functions

* `emm()` as convenient shortcut to compute the estimate marginal mean of the model's response value.

## Changes to functions

* `plot()` gets a `use.theme`-argument, to use the default _ggeffects_-theme, or to use the default _ggplot_-theme.

## Bug fixes

* Fixed issues with columns resp. column names for models that used special functions in formula (e.g. `s()` for `gam`-models, or `bs()` for splines).
* Fixed issue for wrong legend values when grouping term was a non-labelled factor with non-ordered numeric levels.

# ggeffects 0.1.1

## Changes to functions

* `ggpredict()` computes proper confidence intervals for _merMod_- and _lme_-objects.
* Improved `plot()`-method, to better plot raw data.

## Bug fixes

* Confidence intervals were not properly calculated for glm's.
* For `plot()`, argument `rawdata` did not work for models with discrete binary response.
* Fixed issues with models of class `lme` and `glmmTMB`.
* Fixed issues with model-classes that preserved NA-values in model-frame.

# ggeffects 0.1.0

## General

* initial release
