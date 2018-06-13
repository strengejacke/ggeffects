# Contributing to ggeffects

This outlines how to propose a change to ggeffects. 

## Fixing typos

Small typos or grammatical errors in documentation may be edited directly using the GitHub web interface, so long as the changes are made in the _source_ file. If you want to fix typos in the documentation, please edit the related `.R` file in the `R/` folder. Do _not_ edit an `.Rd` file in `man/`.

## Filing an issue

The easiest way to propose a change or new feature is to file an issue. If you've found a
bug, you may also create an associated issue. If possible, try to illustrate your proposal or the bug with a minimal [reproducible example](https://www.tidyverse.org/help/#reprex).

## Adding support for new models

The package is easily extendable, to add support for other model objects. The only requirement is that following methods are available: `predict()`, `model.frame()` and `family()`. If model objects do not support these methods, you may implement workarounds (see below).

Following code needs to be revised to add further model objects:

* file *utils_model_function.R*, function `get_model_function()` needs a line to specify whether the new model can be considered as linear or generalized linear model.
* file *utils_model_function.R*, function `get_predict_function()` needs a line to specify the class.
* finally, in the file *predictions.R*, add a line to `select_prediction_method()` to call the right prediction-method, and add a method `get_predictions_<class>()`, if one of the existing prediction-methods does not fit the needs of the new model object.

When the model object _does not_ support one of `predict()`, `model.frame()` or `family()`, you may add workarounds:

* if the model does _not_ have a `family()`-function, a workaround has to be added to `model_family()` in [the sjstats-package](https://github.com/strengejacke/sjstats/blob/master/R/pred_vars.R).
* if the model does _not_ have a `model.frame()`-function with standard arguments or return values, a workaround has to be added to `model_frame()` in [the sjstats-package](https://github.com/strengejacke/sjstats/blob/master/R/pred_vars.R).
* if the model does _not_ have a `predict()`-function, a workaround has to be added to `get_predictions_<class>()` in the file *predictions.R* (in this package).

## Adding new functions

Most likely, there's no need to add more functions to this package. It's main purpose is to calculate predicted values, so `ggpredict()` and `ggeffect()` should cover the main functionality of this package.

## Pull requests

*  Please create a Git branch for each pull request (PR).
*  Your contributed code should roughly follow the tidyverse [style guide](http://style.tidyverse.org). There is probably no need to add more arguments to existing functions, but if so, please use _dots_ as separator for _argumentnames_ ([see here](https://github.com/strengejacke/ggeffects/blob/master/R/predictions.R)).
*  ggeffects uses [roxygen2](https://cran.r-project.org/package=roxygen2), with
[Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html),
for documentation.
*  ggeffects uses [testthat](https://cran.r-project.org/package=testthat). Adding tests to the PR makes it easier for me to merge your PR into the code base.
*  If your PR is a user-visible change, you may add a bullet to the top of `NEWS.md` describing the changes made. You may optionally add your GitHub username, and links to relevant issue(s)/PR(s).

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to
abide by its terms.
