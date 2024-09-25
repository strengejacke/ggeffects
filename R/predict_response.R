#' @title Adjusted predictions and estimated marginal means from regression models
#' @name predict_response
#'
#' @description
#' After fitting a model, it is useful generate model-based estimates (expected
#' values, or _adjusted predictions_) of the response variable for different
#' combinations of predictor values. Such estimates can be used to make
#' inferences about relationships between variables.
#'
#' The **ggeffects** package computes marginal means and adjusted predicted
#' values for the response, at the margin of specific values or levels from
#' certain model terms. The package is built around three core functions:
#' `predict_response()` (understanding results), `test_predictions()` (importance
#' of results) and `plot()` (communicate results).
#'
#' By default, adjusted predictions or marginal means are returned on the
#' *response* scale, which is the easiest and most intuitive scale to interpret
#' the results. There are other options for specific models as well, e.g. with
#' zero-inflation component (see documentation of the `type`-argument). The
#' result is returned as structured data frame, which is nicely printed by
#' default. `plot()` can be used to easily create figures.
#'
#' The main function to calculate marginal means and adjusted predictions is
#' `predict_response()`, which returns adjusted predictions, marginal means
#' or averaged counterfactual predictions depending on value of the
#' `margin`-argument.
#'
#' In previous versions of **ggeffects**, the functions `ggpredict()`, `ggemmeans()`,
#' `ggeffect()` and `ggaverage()` were used to calculate marginal means and
#' adjusted predictions. These functions are still available, but `predict_response()`
#' as a "wrapper" around these functions is the preferred way to calculate marginal
#' means and adjusted predictions now.
#'
#' @param model A model object.
#' @param terms Names of those terms from `model`, for which predictions should
#' be displayed (so called _focal terms_). Can be:
#'   - A character vector, specifying the names of the focal terms. This is the
#'     preferred and probably most flexible way to specify focal terms, e.g.
#'     `terms = "x [40:60]"`, to calculate predictions for the values 40 to 60.
#'   - A list, where each element is a named vector, specifying the focal terms
#'     and their values. This is the "classical" R way to specify focal terms,
#'     e.g. `list(x = 40:60)`.
#'   - A formula, e.g. `terms = ~ x + z`, which is internally converted to a
#'     character vector. This is probably the least flexible way, as you cannot
#'     specify representative values for the focal terms.
#'   - A data frame representing a "data grid" or "reference grid". Predictions
#'     are then made for all combinations of the variables in the data frame.
#'
#' `terms` at least requires one variable name. The maximum length is four terms,
#' where the second to fourth term indicate the groups, i.e. predictions of the first
#' term are grouped at meaningful values or levels of the remaining terms (see
#' [`values_at()`]). It is also possible to define specific values for focal
#' terms, at which adjusted predictions should be calculated (see details below).
#' All remaining covariates that are not specified in `terms` are "marginalized",
#' see the `margin` argument in `?predict_response`. See also argument `condition`
#' to fix non-focal terms to specific values, and argument `typical` for
#' `ggpredict()` or `ggemmeans()`.
#' @param ci_level Numeric, the level of the confidence intervals. Use
#' `ci_level = NA` if confidence intervals should not be calculated
#' (for instance, due to computation time). Typically, confidence intervals are
#' based on the returned standard errors for the predictions, assuming a t- or
#' normal distribution (based on the model and the available degrees of freedom,
#' i.e. roughly `+/- 1.96 * SE`). See introduction of
#' [this vignette](https://strengejacke.github.io/ggeffects/articles/ggeffects.html)
#' for more details.
#' @param type Character, indicating whether predictions should be conditioned
#' on specific model components or not. Consequently, most options only apply
#' for survival models, mixed effects models and/or models with zero-inflation
#' (and their Bayesian counter-parts); only exception is `type = "simulate"`,
#' which is available for some other model classes as well (which respond to
#' `simulate()`).
#'
#' **Note 1:** For `brmsfit`-models with zero-inflation component, there is no
#' `type = "zero_inflated"` nor `type = "zi_random"`; predicted values for these
#' models *always* condition on the zero-inflation part of the model. The same
#' is true for `MixMod`-models from **GLMMadaptive** with zero-inflation
#' component (see 'Details').
#'
#' **Note 2:** If `margin = "empirical"`, or when calling `ggaverage()` respectively,
#' (i.e. counterfactual predictions), the `type` argument is handled differently.
#' It is set to `"response"` by default, but usually accepts all possible options
#' from the `type`-argument of the model's respective `predict()` method. E.g.,
#' passing a `glm` object would allow the options `"response"`, `"link"`, and
#' `"terms"`. For models with zero-inflation component, the below mentioned
#' options `"fixed"`, `"zero_inflated"` and `"zi_prob"` can also be used and will
#' be "translated" into the corresponding `type` option of the model's respective
#' `predict()`-method.
#'
#'   - `"fixed"` (or `"count"`)
#'
#'     Predicted values are conditioned on the fixed effects or conditional
#'     model only (for mixed models: predicted values are on the population-level
#'     and *confidence intervals* are returned, i.e. `re.form = NA` when calling
#'     `predict()`). For instance, for models fitted with `zeroinfl` from **pscl**,
#'     this would return the predicted mean from the count component (without
#'     zero-inflation). For models with zero-inflation component, this type calls
#'     `predict(..., type = "link")` (however, predicted values are
#'     back-transformed to the response scale, i.e. the conditional mean of the
#'     response).
#'
#'   - `"random"`
#'
#'     This only applies to mixed models, and `type = "random"` does not condition
#'     on the zero-inflation component of the model. `type = "random"` still
#'     returns population-level predictions, however, conditioned on random effects
#'     and considering individual level predictions, i.e. `re.form = NULL` when
#'     calling `predict()`. This may affect the returned predicted values, depending
#'     on whether `REML = TRUE` or `REML = FALSE` was used for model fitting.
#'     Furthermore, unlike `type = "fixed"`, intervals also consider the uncertainty
#'     in the variance parameters (the mean random effect variance, see *Johnson
#'     et al. 2014* for details) and hence can be considered as *prediction intervals*.
#'     For models with zero-inflation component, this type calls
#'     `predict(..., type = "link")` (however, predicted values are back-transformed
#'     to the response scale).
#'
#'     To get predicted values for each level of the random effects groups, add the
#'     name of the related random effect term to the `terms`-argument
#'     (for more details, see
#'     [this vignette](https://strengejacke.github.io/ggeffects/articles/introduction_effectsatvalues.html)).
#'
#'   - `"zero_inflated"` (or `"zi"`)
#'
#'     Predicted values are conditioned on the fixed effects and the zero-inflation
#'     component. For instance, for models fitted with `zeroinfl` from **pscl**,
#'     this would return the predicted (or expected) response (`mu*(1-p)`),
#'     and for **glmmTMB**, this would return the expected response `mu*(1-p)`
#'     *without* conditioning on random effects (i.e. random effect variances
#'     are not taken into account for the confidence intervals). For models with
#'     zero-inflation component, this type calls `predict(..., type = "response")`.
#'     See 'Details'.
#'
#'   - `"zi_random"` (or `"zero_inflated_random"`)
#'
#'     Predicted values are conditioned on the zero-inflation component and
#'     take the random effects uncertainty into account. For models fitted with
#'     `glmmTMB()`, `hurdle()` or `zeroinfl()`, this would return the
#'     expected value `mu*(1-p)`. For **glmmTMB**, prediction intervals
#'     also consider the uncertainty in the random effects variances. This
#'     type calls `predict(..., type = "response")`. See 'Details'.
#'
#'   - `"zi_prob"`
#'
#'     Predicted zero-inflation probability. For **glmmTMB** models with
#'     zero-inflation component, this type calls `predict(..., type = "zlink")`;
#'     models from **pscl** call `predict(..., type = "zero")` and for
#'     **GLMMadaptive**, `predict(..., type = "zero_part")` is called.
#'
#'   - `"simulate"`
#'
#'     Predicted values and confidence resp. prediction intervals are
#'     based on simulations, i.e. calls to `simulate()`. This type
#'     of prediction takes all model uncertainty into account, including
#'     random effects variances. Currently supported models are objects of
#'     class `lm`, `glm`, `glmmTMB`, `wbm`, `MixMod` and `merMod`.
#'     See `...` for details on number of simulations.
#'
#'   - `"survival"` and `"cumulative_hazard"`
#'
#'     Applies only to `coxph`-objects from the **survial**-package and
#'     calculates the survival probability or the cumulative hazard of an event.
#'
#' When `margin = "empirical"` (or when calling `ggaverage()`), the `type`
#' argument accepts all values from the `type`-argument of the model's respective
#' `predict()`-method.
#' @param margin Character string, indicating how to marginalize over the
#' *non-focal* predictors, i.e. those variables that are *not* specified in
#' `terms`. Possible values are `"mean_reference"`, `"mean_mode"`,
#' `"marginalmeans"` and `"empirical"` (or `"counterfactual"`, aka average
#' "counterfactual" predictions). You can set a default-option for the `margin`
#' argument via `options()`, e.g. `options(ggeffects_margin = "empirical")`,
#' so you don't have to specify your preferred marginalization method each time
#' you call `predict_response()`. See details in the documentation below.
#' @param back_transform Logical, if `TRUE` (the default), predicted values for
#' log-, log-log, exp, sqrt and similar transformed responses will be
#' back-transformed to original response-scale. See
#' [`insight::find_transformation()`] for more details.
#' @param bias_correction Logical, if `TRUE`, adjusts for bias-correction in
#' back-transforming the predicted values for non-Gaussian models. This requires
#' a valid value of sigma to exist, which is extracted by default (using
#' [`insight::get_sigma()`]) when `ggpredict()` is called or
#' `margin = "mean_reference"` (the default). If `ggemmeans()` is called or
#' `margin = "marginalmeans"`, an additional argument `sigma` is _required_
#' to apply bias-correction. Bias correction is not yet supported for
#' `ggaverage()`. For `ggpredict()` (or `margin = "mean_reference"`), defining
#' own values for `sigma` is optional. This can be particular helpful for mixed
#' models, where extracting the random effects variances (e.g., using the
#' `sqrt()` of [`insight::get_variance_residual()`] for `sigma`) is probably a
#' more accurate value for `sigma` then the default value extracted by
#' [`insight::get_sigma()`]. The here discussed bias occurs when predictions
#' from non-Gaussian models (e.g. logistic regression) are back-transformed to
#' their response scale (in the case of logistic regression, back-transformed to
#' predicted probabilites), which relates to _Jensen's inequality_: the
#' transformation of an averaged prediction is not identical to the average of
#' transformed predictions. If the goal is not to compare adjusted predictions,
#' but rather to get accurate estimates of the adjusted predictions, using
#' bias-correction is recommended.
#' @param condition Named character vector, which indicates covariates that
#' should be held constant at specific values. Unlike `typical`, which
#' applies a function to the covariates to determine the value that is used
#' to hold these covariates constant, `condition` can be used to define
#' exact values, for instance `condition = c(covariate1 = 20, covariate2 = 5)`.
#' See 'Examples'.
#' @param interval Type of interval calculation, can either be `"confidence"`
#' (default) or `"prediction"`. May be abbreviated. Unlike *confidence intervals*,
#' *prediction intervals* include the residual variance (sigma^2) to account for
#' the uncertainty of predicted values. For mixed models, `interval = "prediction"`
#' is the default for `type = "random"`. When `type = "fixed"`, the default is
#' `interval = "confidence"`. Note that prediction intervals are not available
#' for all models, but only for models that work with [`insight::get_sigma()`].
#' For Bayesian models, when `interval = "confidence"`, predictions are based on
#' posterior draws of the linear predictor [`rstantools::posterior_epred()`].
#' If `interval = "prediction"`, [`rstantools::posterior_predict()`] is called.
#' @param vcov_fun Variance-covariance matrix used to compute uncertainty
#' estimates (e.g., for confidence intervals based on robust standard errors).
#' This argument accepts a covariance matrix, a function which returns a
#' covariance matrix, or a string which identifies the function to be used to
#' compute the covariance matrix.
#' - A (variance-covariance) matrix
#' - A function which returns a covariance matrix (e.g., `stats::vcov()`)
#' - A string which indicates the estimation type for the heteroscedasticity-consistent
#'   variance-covariance matrix, e.g. `vcov_fun = "HC0"`. Possible values are
#'   `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`, `"HC4m"`, and `"HC5"`, which
#'   will then call the `vcovHC()`-function from the **sandwich** package, using
#'   the specified type. Further possible values are `"CR0"`, `"CR1"`, `"CR1p"`,
#'   `"CR1S"`, `"CR2"`, and `"CR3"`, which will call the `vcovCR()`-function from
#'   the **clubSandwich** package.
#' - A string which indicates the name of the `vcov*()`-function from the
#'   **sandwich** or **clubSandwich** packages, e.g. `vcov_fun = "vcovCL"`,
#'   which is used to compute (cluster) robust standard errors for predictions.
#'
#' If `NULL`, standard errors (and confidence intervals) for predictions are
#' based on the standard errors as returned by the `predict()`-function.
#' **Note** that probably not all model objects that work with `predict_response()`
#' are also supported by the **sandwich** or **clubSandwich** packages.
#'
#' See details in [this vignette](https://strengejacke.github.io/ggeffects/articles/practical_robustestimation.html).
#' @param vcov_type Character vector, specifying the estimation type for the
#' robust covariance matrix estimation (see `?sandwich::vcovHC`
#' or `?clubSandwich::vcovCR` for details). Only used when `vcov_fun` is a
#' character string indicating one of the functions from those packages.
#' When `vcov_fun` is a function, a possible `type` argument _must_ be provided
#' via the `vcov_args` argument.
#' @param vcov_args List of named vectors, used as additional arguments that
#' are passed down to `vcov_fun`.
#' @param weights This argument is used in two different ways, depending on the
#' `margin` argument.
#' - When `margin = "empirical"`, `weights` can either be a character vector,
#'   naming the weigthing variable in the data, or a vector of weights (of same
#'   length as the number of observations in the data). This variable will be
#'   used to weight adjusted predictions.
#' - When `margin = "marginalmeans"`, `weights` must be a character vector and
#'   is passed to [`emmeans::emmeans()`], specifying weights to use in averaging
#'   non-focal categorical predictors. See https://rvlenth.github.io/emmeans/reference/emmeans.html
#'   for details.
#' @param verbose Toggle messages or warnings.
#' @param ... If `margin` is set to `"mean_reference"` or `"mean_mode"`, arguments
#' are passed down to `ggpredict()` (further down to `predict()`); for
#' `margin = "marginalmeans"`, further arguments passed down to `ggemmeans()` and
#' thereby to `emmeans::emmeans()`; if `margin = "empirical"`, further arguments are
#' passed down to `marginaleffects::avg_predictions()`. If `type = "simulate"`,
#' `...` may also be used to set the number of simulation, e.g. `nsim = 500`.
#' When calling `ggeffect()`, further arguments passed down to `effects::Effect()`.
#'
#' @section Supported Models:
#'
#' A list of supported models can be found at [the package website](https://github.com/strengejacke/ggeffects).
#' Support for models varies by marginalization method (the `margin` argument),
#' i.e. although `predict_response()` supports most models, some models are only
#' supported exclusively by one of the four downstream functions (`ggpredict()`,
#' `ggemmeans()`, `ggeffect()` or `ggaverage()`). This means that not all models
#' work for every `margin` option of `predict_response()`.
#'
#' @section Holding covariates at constant values, or how to marginalize over the *non-focal* predictors:
#'
#' `predict_response()` is a wrapper around `ggpredict()`, `ggemmeans()` and
#' `ggaverage()`. Depending on the value of the `margin` argument,
#' `predict_response()` calls one of those functions. The `margin` argument
#' indicates how to marginalize over the *non-focal* predictors, i.e. those
#' variables that are *not* specified in `terms`. Possible values are:
#'
#' - `"mean_reference"` and `"mean_mode"`: For `"mean_reference"`, non-focal
#'   predictors are set to their mean (numeric variables), reference level
#'   (factors), or "most common" value (mode) in case of character vectors.
#'   For `"mean_mode"`, non-focal predictors are set to their mean (numeric
#'   variables) or mode (factors, or "most common" value in case of character
#'   vectors).
#'
#'   These predictons represent a rather "theoretical" view on your data, which
#'   does not necessarily exactly reflect the characteristics of your sample. It
#'   helps answer the question, "What is the predicted (or: expected) value of
#'   the response at meaningful values or levels of my focal terms for a
#'   'typical' observation in my data?", where 'typical' refers to certain
#'   characteristics of the remaining predictors.
#'
#' - `"marginalmeans"`: non-focal predictors are set to their mean (numeric
#'   variables) or averaged over the levels or "values" for factors and
#'   character vectors. Averaging over the factor levels of non-focal terms
#'   computes a kind of "weighted average" for the values at which these terms
#'   are hold constant. Thus, non-focal categorical terms are conditioned on
#'   "weighted averages" of their levels. There are different weighting
#'   options that can be altered using the `weights` argument.
#'
#'   These predictions come closer to the sample, because all possible values
#'   and levels of the non-focal predictors are taken into account. It would
#'   answer the question, "What is the predicted (or: expected) value of the
#'   response at meaningful values or levels of my focal terms for an 'average'
#'   observation in my data?". It refers to randomly picking a subject of your
#'   sample and the result you get on average.
#'
#' - `"empirical"` (or `"counterfactual"`): non-focal predictors are averaged
#'   over the observations in the sample. The response is predicted for each
#'   subject in the data and predicted values are then averaged across all
#'   subjects, aggregated/grouped by the focal terms. In particular, averaging
#'   is applied to _counterfactual predictions_ (Dickerman and Hernan 2020).
#'   There is a more detailed description in
#'   [this vignette](https://strengejacke.github.io/ggeffects/articles/technical_differencepredictemmeans.html).
#'
#'   Counterfactual predictions are useful, insofar as the results can also be
#'   transferred to other contexts. It answers the question, "What is the
#'   predicted (or: expected) value of the response at meaningful values or
#'   levels of my focal terms for the 'average' observation in the population?".
#'   It does not only refer to the actual data in your sample, but also "what
#'   would be if" we had more data, or if we had data from a different
#'   population. This is where "counterfactual" refers to.
#'
#' You can set a default-option for the `margin` argument via `options()`, e.g.
#' `options(ggeffects_margin = "empirical")`, so you don't have to specify your
#' "default" marginalization method each time you call `predict_response()`.
#' Use `options(ggeffects_margin = NULL)` to remove that setting.
#'
#' The `condition` argument can be used to fix non-focal terms to specific
#' values.
#'
#' @section Marginal Means and Adjusted Predictions at Specific Values:
#'
#' Meaningful values of focal terms can be specified via the `terms` argument.
#' Specifying meaningful or representative values as string pattern is the
#' preferred way in the **ggeffects** package. However, it is also possible to
#' use a `list()` for the focal terms if prefer the "classical" R way. `terms`
#' can also be a data (or reference) grid provided as data frame. All options
#' are described in [this vignette](https://strengejacke.github.io/ggeffects/articles/introduction_effectsatvalues.html).
#'
#' Indicating levels in square brackets allows for selecting only certain
#' groups or values resp. value ranges. The term name and the start of the
#' levels in brackets must be separated by a whitespace character, e.g.
#' `terms = c("age", "education [1,3]")`. Numeric ranges, separated with colon,
#' are also allowed: `terms = c("education", "age [30:60]")`. The stepsize for
#' ranges can be adjusted using `by`, e.g. `terms = "age [30:60 by=5]"`.
#'
#' The `terms` argument also supports the same shortcuts as the `values` argument
#' in `values_at()`. So `terms = "age [meansd]"` would return predictions for
#' the values one standard deviation below the mean age, the mean age and one SD
#' above the mean age. `terms = "age [quart2]"` would calculate predictions at
#' the value of the lower, median and upper quartile of age.
#'
#' Furthermore, it is possible to specify a function name. Values for predictions
#' will then be transformed, e.g. `terms = "income [exp]"`. This is useful when
#' model predictors were transformed for fitting the model and should be
#' back-transformed to the original scale for predictions. It is also possible
#' to define own functions (see
#' [this vignette](https://strengejacke.github.io/ggeffects/articles/introduction_effectsatvalues.html)).
#'
#' Instead of a function, it is also possible to define the name of a variable
#' with specific values, e.g. to define a vector `v = c(1000, 2000, 3000)` and
#' then use `terms = "income [v]"`.
#'
#' You can take a random sample of any size with `sample=n`, e.g
#' `terms = "income [sample=8]"`, which will sample eight values from
#' all possible values of the variable `income`. This option is especially
#' useful for plotting predictions at certain levels of random effects
#' group levels, where the group factor has too many levels to be completely
#' plotted. For more details, see
#' [this vignette](https://strengejacke.github.io/ggeffects/articles/introduction_effectsatvalues.html).
#'
#' Finally, numeric vectors for which no specific values are given, a "pretty range"
#' is calculated (see [`pretty_range()`]), to avoid memory allocation problems
#' for vectors with many unique values. If a numeric vector is specified as
#' second or third term (i.e. if this focal term is used for "stratification"),
#' representative values (see [`values_at()`]) are chosen (unless other values
#' are specified), which are typically the mean value, as well as one standard
#' deviation below and above the mean. If all values for a numeric vector should
#' be used to compute predictions, you may use e.g. `terms = "age [all]"`. See
#' also package vignettes.
#'
#' To create a pretty range that should be smaller or larger than the default
#' range (i.e. if no specific values would be given), use the `n` tag, e.g.
#' `terms="age [n=5]"` or `terms="age [n=12]"`. Larger values for `n` return a
#' larger range of predicted values.
#'
#' @section Bayesian Regression Models:
#'
#' `predict_response()` also works with **Stan**-models from the **rstanarm** or
#' **brms**-packages. The predicted values are the median value of all drawn
#' posterior samples. Standard errors are the median absolute deviation of the
#' posterior samples. The confidence intervals for Stan-models are Bayesian
#' predictive intervals. By default, the predictions are based on
#' [`rstantools::posterior_epred()`] and hence have the limitations that the
#' uncertainty of the error term (residual variance) is not taken into account.
#' The recommendation is to use the posterior predictive distribution
#' ([`rstantools::posterior_predict()`]), i.e. setting `interval = "prediction"`.
#'
#' @section Zero-Inflated and Zero-Inflated Mixed Models with brms:
#'
#' Models of class `brmsfit` always condition on the zero-inflation component,
#' if the model has such a component. Hence, there is no `type = "zero_inflated"`
#' nor `type = "zi_random"` for `brmsfit`-models, because predictions are based
#' on draws of the posterior distribution, which already account for the
#' zero-inflation part of the model.
#'
#' **Zero-Inflated and Zero-Inflated Mixed Models with glmmTMB**
#'
#' If `model` is of class `glmmTMB`, `hurdle`, `zeroinfl` or `zerotrunc`, and
#' `margin` is _not_ set to `"empirical`,  simulations from a multivariate
#' normal distribution (see `?MASS::mvrnorm`) are drawn to calculate `mu*(1-p)`.
#' Confidence intervals are then based on quantiles of these results.
#' For `type = "zi_random"`, prediction intervals also take the uncertainty in
#' the random-effect paramters into account (see also _Brooks et al. 2017_,
#' pp.391-392 for details).
#'
#' An alternative for models fitted with **glmmTMB** that take all model
#' uncertainties into account are simulations based on `simulate()`, which
#' is used when `type = "simulate"` (see _Brooks et al. 2017_, pp.392-393 for
#' details).
#'
#' Finally, if `margin = "empirical"`, the returned predictions are already
#' conditioned on the zero-inflation part (and possible random effects) of the
#' model, thus these are most comparable to the `type = "simulate"` option. In
#' other words, if all model components should be taken into account for
#' predictions, you should consider using `margin = "empirical"`.
#'
#' @section MixMod-models from GLMMadaptive:
#'
#' Predicted values for the fixed effects component (`type = "fixed"` or
#' `type = "zero_inflated"`) are based on `predict(..., type = "mean_subject")`,
#' while predicted values for random effects components (`type = "random"` or
#' `type = "zi_random"`) are calculated with `predict(..., type = "subject_specific")`
#' (see `?GLMMadaptive::predict.MixMod` for details). The latter option
#' requires the response variable to be defined in the `newdata`-argument
#' of `predict()`, which will be set to its typical value (see
#' [`values_at()`]).
#'
#' @section Multinomial Models:
#'
#' `polr`, `clm` models, or more generally speaking, models with ordinal or
#' multinominal outcomes, have an additional column `response.level`, which
#' indicates with which level of the response variable the predicted values are
#' associated.
#'
#' @section Averaged model predictions (package **MuMIn**):
#'
#' For averaged model predictions, i.e. when the input model is an object of
#' class `"averaging"` (`MuMIn::model.avg()`), predictions are made with the
#' full averaged coefficients.
#'
#' @references
#' - Brooks ME, Kristensen K, Benthem KJ van, Magnusson A, Berg CW, Nielsen A,
#'   et al. glmmTMB Balances Speed and Flexibility Among Packages for Zero-inflated
#'   Generalized Linear Mixed Modeling. The R Journal. 2017;9: 378-400.
#' - Johnson PC. 2014. Extension of Nakagawa & Schielzeth's R2GLMM to random
#'   slopes models. Methods Ecol Evol, 5: 944-946.
#' - Dickerman BA, Hernan, MA. Counterfactual prediction is not only for causal
#'   inference. Eur J Epidemiol 35, 615–617 (2020).
#'
#' @note
#' **Printing Results**
#'
#' The `print()` method gives a clean output (especially for predictions by
#' groups), and indicates at which values covariates were held constant.
#' Furthermore, the `print()` method has several arguments to customize the
#' output. See [this vignette](https://strengejacke.github.io/ggeffects/articles/introduction_print.html)
#' for details.
#'
#' **Limitations**
#'
#' The support for some models, for example from package **MCMCglmm**, is not
#' fully tested and may fail for certain models. If you encounter any errors,
#' please file an issue [at Github](https://github.com/strengejacke/ggeffects/issues).
#'
#' @return A data frame (with `ggeffects` class attribute) with consistent data columns:
#'
#' - `"x"`: the values of the first term in `terms`, used as x-position in plots.
#' - `"predicted"`: the predicted values of the response, used as y-position in plots.
#' - `"std.error"`: the standard error of the predictions. *Note that the standard
#'    errors are always on the link-scale, and not back-transformed for non-Gaussian
#'    models!*
#' - `"conf.low"`: the lower bound of the confidence interval for the predicted values.
#' - `"conf.high"`: the upper bound of the confidence interval for the predicted values.
#' - `"group"`: the grouping level from the second term in `terms`, used as
#'     grouping-aesthetics in plots.
#' - `"facet"`: the grouping level from the third term in `terms`, used to indicate
#'     facets in plots.
#'
#'   The estimated marginal means (or predicted values) are always on the
#'   response scale!
#'
#'   For proportional odds logistic regression (see `?MASS::polr`)
#'   resp. cumulative link models (e.g., see `?ordinal::clm`),
#'   an additional column `"response.level"` is returned, which indicates
#'   the grouping of predictions based on the level of the model's response.
#'
#'   Note that for convenience reasons, the columns for the intervals
#'   are always named `"conf.low"` and `"conf.high"`, even though
#'   for Bayesian models credible or highest posterior density intervals
#'   are returned.
#'
#'   There is an [`as.data.frame()`] method for objects of class `ggeffects`,
#'   which has an `terms_to_colnames` argument, to use the term names as column
#'   names instead of the standardized names `"x"` etc.
#'
#' @examplesIf requireNamespace("sjlabelled") && requireNamespace("ggplot2")
#' library(sjlabelled)
#' data(efc)
#' fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
#'
#' predict_response(fit, terms = "c12hour")
#' predict_response(fit, terms = c("c12hour", "c172code"))
#' # more compact table layout for printing
#' out <- predict_response(fit, terms = c("c12hour", "c172code", "c161sex"))
#' print(out, collapse_table = TRUE)
#'
#' # specified as formula
#' predict_response(fit, terms = ~ c12hour + c172code + c161sex)
#'
#' # only range of 40 to 60 for variable 'c12hour'
#' predict_response(fit, terms = "c12hour [40:60]")
#'
#' # terms as named list
#' predict_response(fit, terms = list(c12hour = 40:60))
#'
#' # covariate "neg_c_7" is held constant at a value of 11.84 (its mean value).
#' # To use a different value, use "condition"
#' predict_response(fit, terms = "c12hour [40:60]", condition = c(neg_c_7 = 20))
#'
#' # to plot ggeffects-objects, you can use the 'plot()'-function.
#' # the following examples show how to build your ggplot by hand.
#'
#' \donttest{
#' # plot predicted values, remaining covariates held constant
#' library(ggplot2)
#' mydf <- predict_response(fit, terms = "c12hour")
#' ggplot(mydf, aes(x, predicted)) +
#'   geom_line() +
#'   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)
#'
#' # three variables, so we can use facets and groups
#' mydf <- predict_response(fit, terms = c("c12hour", "c161sex", "c172code"))
#' ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
#'   stat_smooth(method = "lm", se = FALSE) +
#'   facet_wrap(~facet, ncol = 2)
#'
#' # select specific levels for grouping terms
#' mydf <- predict_response(fit, terms = c("c12hour", "c172code [1,3]", "c161sex"))
#' ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
#'   stat_smooth(method = "lm", se = FALSE) +
#'   facet_wrap(~facet) +
#'   labs(
#'     y = get_y_title(mydf),
#'     x = get_x_title(mydf),
#'     colour = get_legend_title(mydf)
#'   )
#'
#' # level indication also works for factors with non-numeric levels
#' # and in combination with numeric levels for other variables
#' data(efc)
#' efc$c172code <- sjlabelled::as_label(efc$c172code)
#' fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
#' predict_response(fit, terms = c("c12hour",
#'   "c172code [low level of education, high level of education]",
#'   "c161sex [1]"))
#'
#' # when "terms" is a named list
#' predict_response(fit, terms = list(
#'   c12hour = seq(0, 170, 30),
#'   c172code = c("low level of education", "high level of education"),
#'   c161sex = 1)
#' )
#'
#' # use categorical value on x-axis, use axis-labels, add error bars
#' dat <- predict_response(fit, terms = c("c172code", "c161sex"))
#' ggplot(dat, aes(x, predicted, colour = group)) +
#'   geom_point(position = position_dodge(0.1)) +
#'   geom_errorbar(
#'     aes(ymin = conf.low, ymax = conf.high),
#'     position = position_dodge(0.1)
#'   ) +
#'   scale_x_discrete(breaks = 1:3, labels = get_x_labels(dat))
#'
#' # 3-way-interaction with 2 continuous variables
#' data(efc)
#' # make categorical
#' efc$c161sex <- as_factor(efc$c161sex)
#' fit <- lm(neg_c_7 ~ c12hour * barthtot * c161sex, data = efc)
#' # select only levels 30, 50 and 70 from continuous variable Barthel-Index
#' dat <- predict_response(fit, terms = c("c12hour", "barthtot [30,50,70]", "c161sex"))
#' ggplot(dat, aes(x = x, y = predicted, colour = group)) +
#'   stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
#'   facet_wrap(~facet) +
#'   labs(
#'     colour = get_legend_title(dat),
#'     x = get_x_title(dat),
#'     y = get_y_title(dat),
#'     title = get_title(dat)
#'   )
#'
#' # or with ggeffects' plot-method
#' plot(dat, show_ci = FALSE)
#' }
#'
#' # predictions for polynomial terms
#' data(efc)
#' fit <- glm(
#'   tot_sc_e ~ c12hour + e42dep + e17age + I(e17age^2) + I(e17age^3),
#'   data = efc,
#'   family = poisson()
#' )
#' predict_response(fit, terms = "e17age")
#' @export
predict_response <- function(model,
                             terms,
                             margin = "mean_reference",
                             ci_level = 0.95,
                             type = "fixed",
                             condition = NULL,
                             back_transform = TRUE,
                             vcov_fun = NULL,
                             vcov_type = NULL,
                             vcov_args = NULL,
                             weights = NULL,
                             interval,
                             bias_correction = FALSE,
                             verbose = TRUE,
                             ...) {
  # default for "margin" argument?
  margin <- getOption("ggeffects_margin", margin)
  # validate "margin" argument
  margin <- match.arg(
    margin,
    c("mean_reference", "mean_mode", "marginalmeans", "empirical",
      "counterfactual", "full_data", "ame", "marginaleffects")
  )

  # save name, so it can later be retrieved from environment
  model_name <- insight::safe_deparse(substitute(model))

  # validate type arguments
  type <- .validate_type_argument(
    model,
    type,
    # check for aliases for "empirical" margin
    marginaleffects = margin %in% c("empirical", "counterfactual", "ame", "marginaleffects")
  )

  if (missing(interval)) {
    if (type %in% c("random", "zero_inflated_random")) {
      interval <- "prediction"
    } else {
      interval <- "confidence"
    }
  } else if (is.null(interval)) {
    interval <- "confidence"
  }

  # make sure we have valid values
  interval <- match.arg(interval, c("confidence", "prediction"))

  out <- switch(margin,
    mean_reference = ggpredict(
      model,
      terms = terms,
      ci_level = ci_level,
      type = type,
      typical = "mean",
      condition = condition,
      back_transform = back_transform,
      vcov_fun = vcov_fun,
      vcov_type = vcov_type,
      vcov_args = vcov_args,
      interval = interval,
      bias_correction = bias_correction,
      verbose = verbose,
      ...
    ),
    mean_mode = ggpredict(
      model,
      terms = terms,
      ci_level = ci_level,
      type = type,
      typical = c(numeric = "mean", factor = "mode"),
      condition = condition,
      back_transform = back_transform,
      vcov_fun = vcov_fun,
      vcov_type = vcov_type,
      vcov_args = vcov_args,
      interval = interval,
      bias_correction = bias_correction,
      verbose = verbose,
      ...
    ),
    marginalmeans = ggemmeans(
      model,
      terms = terms,
      ci_level = ci_level,
      type = type,
      typical = "mean",
      condition = condition,
      back_transform = back_transform,
      vcov_fun = vcov_fun,
      vcov_type = vcov_type,
      vcov_args = vcov_args,
      interval = interval,
      bias_correction = bias_correction,
      verbose = verbose,
      ...
    ),
    ame = ,
    counterfactual = ,
    marginaleffects = ,
    empirical = ggaverage(
      model,
      terms = terms,
      ci_level = ci_level,
      type = type,
      typical = "mean",
      condition = condition,
      back_transform = back_transform,
      vcov_fun = vcov_fun,
      vcov_type = vcov_type,
      vcov_args = vcov_args,
      weights = weights,
      verbose = verbose,
      ...
    ),
    full_data = {
      ## TODO: implement
      # should be:
      # marginaleffects::predictions(
      #   model,
      #   newdata = insight::get_data(model),
      #   by = terms
      # )
    }
  )

  attr(out, "model.name") <- model_name
  out
}
