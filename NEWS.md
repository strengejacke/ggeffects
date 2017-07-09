# ggeffects 0.1.2

## General

* Updated package imports and dependencies.
* Support for `polr` models (pkg _MASS_).
* Support for `hurdle` and `zeroinfl` models (pkg _pscl_).

## New functions

* `emm()` as convenient shortcut to compute the estimate marginal mean of the model's response value.

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
