#' @title Marginal effects, adjusted predictions and estimated marginal means from regression models
#' @name ggpredict
#'
#' @description
#' The **ggeffects** package computes estimated marginal means (predicted values)
#' for the response, at the margin of specific values or levels from certain 
#' model terms, i.e. it generates predictions by a model by holding the
#' non-focal variables constant and varying the focal variable(s).
#'
#' Adjusted predictions or estimated marginal means are always calculated on
#' the *response* scale, which is the easiest and most intuitive scale to
#' interpret the results.
#'
#' `ggpredict()` uses [`predict()`] for generating predictions, while
#' `ggeffect()` computes marginal effects by internally calling
#' [`effects::Effect()`] and `ggemmeans()` uses [`emmeans::emmeans()`].
#' `ggaverage()` uses [`marginaleffects::avg_predictions()`]. The result is
#' returned as consistent data frame.
#'
#' @param model A fitted model object, or a list of model objects. Any model
#' that supports common methods like `predict()`, `family()` or `model.frame()`
#' should work. For `ggeffect()`, any model that is supported by **effects**
#' should work, and for `ggemmeans()`, all models supported by **emmeans**
#' should work.
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
#'   - A data frame representig a "data grid" or "reference grid". Predictions
#'     are then made for all combinations of the variables in the data frame.
#'
#' At least one term is required to calculate effects for certain terms,
#' maximum length is four terms, where the second to fourth term indicate the
#' groups, i.e. predictions of first term are grouped at meaningful values or
#' levels of the remaining terms (see [`values_at()`]). If `terms` is missing
#' or `NULL`, adjusted predictions for each model term are calculated (i.e.
#' each model term is used as single focal term). It is also possible to define
#' specific values for focal terms, at which adjusted predictions should be
#' calculated (see 'Details'). All remaining covariates that are not specified
#' in `terms` are held constant (see 'Details'). See also arguments `condition`
#' and `typical`.
#' @param ci_level Numeric, the level of the confidence intervals. For `ggpredict()`,
#' use `ci_level = NA`, if confidence intervals should not be calculated
#' (for instance, due to computation time). Typically, confidence intervals
#' based on the standard errors as returned by the `predict()` function
#' are returned, assuming normal distribution (i.e. `+/- 1.96 * SE`).
#' See introduction of [this vignette](https://strengejacke.github.io/ggeffects/articles/ggeffects.html)
#' for more details.
#' @param type Character, indicating whether predictions should be conditioned
#' on specific model components or not. Consequently, most options only apply
#' for survival models, mixed effects models and/or models with zero-inflation
#' (and their Bayesian counter-parts); only exeption is `type = "simulate"`,
#' which is available for some other model classes as well (which respond to
#' `simulate()`). **Note:** For `brmsfit`-models with zero-inflation component,
#' there is no `type = "zero_inflated"` nor `type = "zi_random"`; predicted
#' values for `MixMod`-models from **GLMMadaptive** with zero-inflation
#' component *always* condition on the zero-inflation part of the model (see
#' 'Details').
#'
#'   - `"fixed"` (or `"fe"` or `"count"`)
#'
#'     Predicted values are conditioned on the fixed effects or conditional
#'     model only (for mixed models: predicted values are on the population-level
#'     and *confidence intervals* are returned, i.e. `re.form = NA` when calling
#'     `predict()`). For instance, for models fitted with `zeroinfl` from **pscl**,
#'     this would return the predicted mean from the count component (without
#'     zero-inflation). For models with zero-inflation component, this type calls
#'     `predict(..., type = "link")` (however, predicted values are
#'     back-transformed to the response scale).
#'
#'   - `"fixed_ppd"`
#'
#'     Only applies to `ggpredict()`, and only for Bayesian models of class
#'     `stanreg` or `brmsfit`. Computes the posterior predictive distribution.
#'     It is the same as setting `type = "fixed"` in combination with
#'     `ppd = TRUE`.
#'
#'   - `"random"` (or `"re"`)
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
#'   - `"random_ppd"`
#'
#'     Only applies to `ggpredict()`, and only for Bayesian models of class
#'     `stanreg` or `brmsfit`. Computes the posterior predictive distribution.
#'     It is the same as setting `type = "random"` in combination with
#'     `ppd = TRUE`.
#'
#'   - `"zero_inflated"` (or `"fe.zi"` or `"zi"`)
#'
#'     Predicted values are conditioned on the fixed effects and the zero-inflation
#'     component. For instance, for models fitted with `zeroinfl`
#'     from **pscl**, this would return the predicted response (`mu*(1-p)`)
#'     and for **glmmTMB**, this would return the expected value `mu*(1-p)`
#'     *without* conditioning on random effects (i.e. random effect variances
#'     are not taken into account for the confidence intervals). For models with
#'     zero-inflation component, this type calls `predict(..., type = "response")`.
#'     See 'Details'.
#'
#'   - `"zi_random"` (or `"re.zi"` or `"zero_inflated_random"`)
#'
#'     Predicted values are conditioned on the zero-inflation component and
#'     take the random effects uncertainty into account. For models fitted with
#'     `glmmTMB()`, `hurdle()` or `zeroinfl()`, this would return the
#'     expected value `mu*(1-p)`. For **glmmTMB**, prediction intervals
#'     also consider the uncertainty in the random effects variances. This
#'     type calls `predict(..., type = "response")`. See 'Details'.
#'
#'   - `"zi_prob"` (or `"zi.prob"`)
#'
#'     Predicted zero-inflation probability. For **glmmTMB** models with
#'     zero-inflation component, this type calls `predict(..., type = "zlink")`;
#'     models from **pscl** call `predict(..., type = "zero")` and for
#'     **GLMMadaptive**, `predict(..., type = "zero_part")` is called.
#'
#'   - `"simulate"` (or `"sim"`)
#'
#'     Predicted values and confidence resp. prediction intervals are
#'     based on simulations, i.e. calls to `simulate()`. This type
#'     of prediction takes all model uncertainty into account, including
#'     random effects variances. Currently supported models are objects of
#'     class `lm`, `glm`, `glmmTMB`, `wbm`, `MixMod`
#'     and `merMod`. See `...` for details on number of simulations.
#'
#'   - `"survival"` and `"cumulative_hazard"` (or `"surv"` and `"cumhaz"`)
#'
#'     Applies only to `coxph`-objects from the **survial**-package and
#'     calculates the survival probability or the cumulative hazard of an event.
#'
#' @param typical Character vector, naming the function to be applied to the
#' covariates (non-focal terms) over which the effect is "averaged". The
#' default is `"mean"`. Can be `"mean"`, "`weighted.mean`", `"median"`, `"mode"`
#' or `"zero"`, which call the corresponding R functions (except `"mode"`,
#' which calls an internal function to compute the most common value); `"zero"`
#' simply returns 0. By default, if the covariate is a factor, only `"mode"` is
#' applicable; for all other values (including the default, `"mean"`) the
#' reference level is returned. For character vectors, only the mode is returned.
#' You can use a named vector to apply different functions to integer, numeric and
#' categorical covariates, e.g. `typical = c(numeric = "median", factor = "mode")`.
#' If `typical` is `"weighted.mean"`, weights from the model are used. If no
#' weights are available, the function falls back to `"mean"`.
#' @param back_transform Logical, if `TRUE` (the default), predicted values
#' for log- or log-log transformed responses will be back-transformed to
#' original response-scale.
#' @param ppd Logical, if `TRUE`, predictions for Stan-models are based on the
#' posterior predictive distribution [`rstantools::posterior_predict()`]. If
#' `FALSE` (the default), predictions are based on posterior draws of the linear
#' predictor [`rstantools::posterior_linpred()`].
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
#' @param vcov_fun Variance-covariance matrix used to compute uncertainty
#' estimates (e.g., for confidence intervals based on robust standard errors).
#' This argument accepts a covariance matrix, a function which returns a
#' covariance matrix, or a string which identifies the function to be used to
#' compute the covariance matrix.
#' * A (variance-covariance) matrix
#' * A function which returns a covariance matrix (e.g., `stats::vcov()`)
#' * A string which indicates the name of the `vcov*()`-function from the
#'   **sandwich** or **clubSandwich** packages, e.g. `vcov_fun = "vcovCL"`,
#'   which is used to compute (cluster) robust standard errors for predictions.
#'   If `NULL`, standard errors (and confidence intervals) for predictions are
#'   based on the standard errors as returned by the `predict()`-function.
#'   **Note** that probably not all model objects that work with `ggpredict()`
#'   are also supported by the **sandwich** or **clubSandwich** packages.
#'
#' See details in [this vignette](https://strengejacke.github.io/ggeffects/articles/practical_robustestimation.html).
#' @param vcov_type Character vector, specifying the estimation type for the
#' robust covariance matrix estimation (see `?sandwich::vcovHC`
#' or `?clubSandwich::vcovCR` for details). Only used when `vcov_fun` is a
#' character string indicating on of the function from those packages.
#' @param vcov_args List of named vectors, used as additional arguments that
#' are passed down to `vcov_fun`.
#' @param verbose Toggle messages or warnings.
#' @param ci.lvl,vcov.fun,vcov.type,vcov.args,back.transform Deprecated arguments.
#' Please use `ci_level`, `vcov_fun`, `vcov_type`, `vcov_args` and `back_transform`
#' instead.
#' @param ... For `ggpredict()`, further arguments passed down to `predict()`;
#' for `ggeffect()`, further arguments passed down to `effects::Effect()`; for
#' `ggemmeans()`, further arguments passed down to `emmeans::emmeans()`; and
#' for `ggaverage()`, further arguments passed down to
#' `marginaleffects::avg_predictions()`.  If `type = "simulate"`, `...` may
#' also be used to set the number of simulation, e.g. `nsim = 500`.
#'
#' @details
#' **Supported Models**
#'
#' A list of supported models can be found at [the package website](https://github.com/strengejacke/ggeffects).
#' Support for models varies by function, i.e. although `ggpredict()`,
#' `ggemmeans()` and `ggeffect()` support most models, some models
#' are only supported exclusively by one of the three functions.
#'
#' **Difference between `ggpredict()` and `ggeffect()` or `ggemmeans()`**
#'
#' `ggpredict()` calls `predict()`, while `ggeffect()` calls `effects::Effect()`
#' and `ggemmeans()` calls `emmeans::emmeans()` to compute predicted values.
#' Thus, effects returned by `ggpredict()` can be described as *conditional effects*
#' (i.e. these are conditioned on certain (reference) levels of factors), while
#' `ggemmeans()` and `ggeffect()` return *marginal means*, since
#' the effects are "marginalized" (or "averaged") over the levels of factors
#' (or values of character vectors). Therefore, `ggpredict()` and `ggeffect()`
#' resp. `ggemmeans()` differ in how factors and character vectors are held
#' constant: `ggpredict()` uses the reference level (or "lowest" value in case
#' of character vectors), while `ggeffect()` and `ggemmeans()` compute a
#' kind of "average" value, which represents the proportions of each factor's
#' category. Use `condition` to set a specific level for factors in
#' `ggemmeans()`, so factors are not averaged over their categories,
#' but held constant at a given level.
#'
#' **Difference between `ggemmeans()` and `ggaverage()`**
#'
#' Estimated marginal means, as computed by `ggemmeans()` or `ggeffect()`, are a
#' special case of predictions, made on a perfectly balanced grid of categorical
#' predictors, with numeric predictors held at their means, and marginalized with
#' respect to some focal variables. `ggaverage()` calculates predicted values
#' for each observation in the data multiple times, each time fixing all values
#' or levels of the focal terms to and then takes the average of these predicted
#' values (aggregated/grouped by the focal terms). There is no rule of thumb
#' which approach is better; it depends on the characteristics of the sample and
#' the population to which should be generalized. Consulting the
#' [marginaleffects-website](https://marginaleffects.com/) might help to decide
#' which approach is more appropriate. The most apparent difference is how
#' *non-focal* categorical predictors affect the predicted values. `ggpredict()`
#' will condition on a certain level of the non-focal factors (usually, the reference
#' level), `ggemmeans()` will "average" over the levels of non-focal factors,
#' while `ggaverage()` will average over the observations in your sample. See also
#' [this vignette](https://strengejacke.github.io/ggeffects/articles/technical_differencepredictemmeans.html)
#' for details and examples.
#'
#' **Marginal Effects and Adjusted Predictions at Specific Values**
#'
#' Meaningful values of focal terms can be specified via the `terms` argument.
#' Specifying meaningful or representative values as string pattern is the
#' preferred way in the **ggeffects** package. However, it is also possible to
#' use a `list()` for the focal terms if prefer the "classical" R way, which is
#' described in [this vignette](https://strengejacke.github.io/ggeffects/articles/introduction_effectsatvalues.html).
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
#' group levels, where the group factor has many levels that can be completely
#' plotted. For more details, see
#' [this vignette](https://strengejacke.github.io/ggeffects/articles/introduction_effectsatvalues.html).
#'
#' Finally, numeric vectors for which no specific values are given, a "pretty range"
#' is calculated (see [`pretty_range()`]), to avoid memory allocation problems
#' for vectors with many unique values. If a numeric vector is specified as
#' second or third term (i.e. if this vector represents a grouping structure),
#' representative values (see [`values_at()`]) are chosen (unless other values
#' are specified). If all values for a numeric vector should be used to compute
#' predictions, you may use e.g. `terms = "age [all]"`. See also package vignettes.
#'
#' To create a pretty range that should be smaller or larger than the default
#' range (i.e. if no specific values would be given), use the `n` tag, e.g.
#' `terms="age [n=5]"` or `terms="age [n=12]"`. Larger values for `n` return a
#' larger range of predicted values.
#'
#' **Holding covariates at constant values**
#'
#' For `ggpredict()`, a data grid is constructed, roughly comparable to
#' `expand.grid()` on all unique combinations of `model.frame(model)[, terms]`.
#' This data grid (see [`data_grid()`]) as `newdata` argument for `predict()`.
#' In this case, all remaining covariates that are not specified in `terms` are
#' held constant: Numeric values are set to the mean (unless changed with
#' the `condition` or `typical` argument), integer values are set to their
#' median, factors are set to their reference level (may also be changed with
#' `condition`) and character vectors to their mode (most common element).
#'
#' `ggeffect()` and `ggemmeans()`, by default, set remaining numeric covariates
#' to their mean value, while for factors, a kind of "average" value, which
#' represents the proportions of each factor's category, is used. The same
#' applies to character vectors: `ggemmeans()` averages over the distribution
#' of unique values in a character vector, similar to how factors are treated.
#' Thus, *non-focal categorical terms* in `ggemmeans()` and `ggeffect()` are
#' conditioned on "weighted averages" of their levels. For `ggemmeans()`, use
#' `condition` to set a specific level for factors so that these are not
#' averaged over their categories, but held constant at the given level.
#'
#' Finally, `ggaverage()` calculates *average predicted values*, which are
#' averaged over the full sample and aggregated by (representative values of)
#' the focal terms. For further details, see
#' [this vignette](https://strengejacke.github.io/ggeffects/articles/technical_differencepredictemmeans.html).
#'
#' **Bayesian Regression Models**
#'
#' `ggpredict()` also works with **Stan**-models from the **rstanarm** or
#' **brms**-packages. The predicted values are the median value of all drawn
#' posterior samples. The confidence intervals for Stan-models are Bayesian
#' predictive intervals. By default (i.e. `ppd = FALSE`), the predictions are
#' based on [`rstantools::posterior_linpred()`] and hence have some limitations:
#' the uncertainty of the error term is not taken into account. The recommendation
#' is to use the posterior predictive distribution ([`rstantools::posterior_predict()`]).
#'
#' **Zero-Inflated and Zero-Inflated Mixed Models with brms**
#'
#' Models of class `brmsfit` always condition on the zero-inflation component,
#' if the model has such a component. Hence, there is no `type = "zero_inflated"`
#' nor `type = "zi_random"` for `brmsfit`-models, because predictions are based
#' on draws of the posterior distribution, which already account for the
#' zero-inflation part of the model.
#'
#' **Zero-Inflated and Zero-Inflated Mixed Models with glmmTMB**
#'
#' If `model` is of class `glmmTMB`, `hurdle`, `zeroinfl` or `zerotrunc`,
#' simulations from a multivariate normal distribution (see `?MASS::mvrnorm`)
#' are drawn to calculate `mu*(1-p)`. Confidence intervals are then based on
#' quantiles of these results. For `type = "zi_random"`, prediction intervals
#' also take the uncertainty in the random-effect paramters into account (see
#' also _Brooks et al. 2017_, pp.391-392 for details).
#'
#' An alternative for models fitted with **glmmTMB** that take all model
#' uncertainties into account are simulations based on `simulate()`, which
#' is used when `type = "simulate"` (see _Brooks et al. 2017_, pp.392-393 for
#' details).
#'
#' **MixMod-models from GLMMadaptive**
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
#' @references
#' - Brooks ME, Kristensen K, Benthem KJ van, Magnusson A, Berg CW, Nielsen A,
#'   et al. glmmTMB Balances Speed and Flexibility Among Packages for Zero-inflated
#'   Generalized Linear Mixed Modeling. The R Journal. 2017;9: 378-400.
#' - Johnson PC, O'Hara RB. 2014. Extension of Nakagawa & Schielzeth's R2GLMM
#'   to random slopes models. Methods Ecol Evol, 5: 944-946.
#'
#' @note
#' **Multinomial Models**
#'
#' `polr`, `clm` models, or more generally speaking, models with ordinal or
#' multinominal outcomes, have an additional column `response.level`, which
#' indicates with which level of the response variable the predicted values are
#' associated.
#'
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
#' The support for some models, for example from package **MCMCglmm**, is
#' rather experimental and may fail for certain models. If you encounter
#' any errors, please file an issue [at Github](https://github.com/strengejacke/ggeffects/issues).
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
#' ggpredict(fit, terms = "c12hour")
#' ggpredict(fit, terms = c("c12hour", "c172code"))
#' ggpredict(fit, terms = c("c12hour", "c172code", "c161sex"))
#'
#' # specified as formula
#' ggpredict(fit, terms = ~ c12hour + c172code + c161sex)
#'
#' # only range of 40 to 60 for variable 'c12hour'
#' ggpredict(fit, terms = "c12hour [40:60]")
#'
#' # terms as named list
#' ggpredict(fit, terms = list(c12hour = 40:60))
#'
#' # covariate "neg_c_7" is held constant at a value of 11.84 (its mean value).
#' # To use a different value, use "condition"
#' ggpredict(fit, terms = "c12hour [40:60]", condition = c(neg_c_7 = 20))
#'
#' # to plot ggeffects-objects, you can use the 'plot()'-function.
#' # the following examples show how to build your ggplot by hand.
#'
#' \donttest{
#' # plot predicted values, remaining covariates held constant
#' library(ggplot2)
#' mydf <- ggpredict(fit, terms = "c12hour")
#' ggplot(mydf, aes(x, predicted)) +
#'   geom_line() +
#'   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)
#'
#' # three variables, so we can use facets and groups
#' mydf <- ggpredict(fit, terms = c("c12hour", "c161sex", "c172code"))
#' ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
#'   stat_smooth(method = "lm", se = FALSE) +
#'   facet_wrap(~facet, ncol = 2)
#'
#' # select specific levels for grouping terms
#' mydf <- ggpredict(fit, terms = c("c12hour", "c172code [1,3]", "c161sex"))
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
#' ggpredict(fit, terms = c("c12hour",
#'   "c172code [low level of education, high level of education]",
#'   "c161sex [1]"))
#'
#' # when "terms" is a named list
#' ggpredict(fit, terms = list(
#'   c12hour = seq(0, 170, 30),
#'   c172code = c("low level of education", "high level of education"),
#'   c161sex = 1)
#' )
#'
#' # use categorical value on x-axis, use axis-labels, add error bars
#' dat <- ggpredict(fit, terms = c("c172code", "c161sex"))
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
#' dat <- ggpredict(fit, terms = c("c12hour", "barthtot [30,50,70]", "c161sex"))
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
#' plot(dat, ci = FALSE)
#' }
#'
#' # predictions for polynomial terms
#' data(efc)
#' fit <- glm(
#'   tot_sc_e ~ c12hour + e42dep + e17age + I(e17age^2) + I(e17age^3),
#'   data = efc,
#'   family = poisson()
#' )
#' ggeffect(fit, terms = "e17age")
#' @export
ggpredict <- function(model,
                      terms,
                      ci_level = 0.95,
                      type = "fixed",
                      typical = "mean",
                      condition = NULL,
                      back_transform = TRUE,
                      ppd = FALSE,
                      vcov_fun = NULL,
                      vcov_type = NULL,
                      vcov_args = NULL,
                      interval,
                      verbose = TRUE,
                      ci.lvl = ci_level,
                      back.transform = back_transform,
                      vcov.fun = vcov_fun,
                      vcov.type = vcov_type,
                      vcov.args = vcov_args,
                      ...) {
  # check arguments
  type <- match.arg(type, choices = c("fe", "fixed", "count", "re", "random",
                                      "fe.zi", "zero_inflated", "re.zi", "zi_random",
                                      "zero_inflated_random", "zi.prob", "zi_prob",
                                      "sim", "simulate", "surv", "survival", "cumhaz",
                                      "cumulative_hazard", "sim_re", "simulate_random",
                                      "debug", "fixed_ppd", "random_ppd"))

  # handle Bayes exceptions for type with ppd
  if (type %in% c("fixed_ppd", "random_ppd")) {
    ppd <- TRUE
    type <- gsub("_ppd", "", type, fixed = TRUE)
  }

  type <- switch(
    type,
    fixed = ,
    count = "fe",
    random = "re",
    zi = ,
    zero_inflated = "fe.zi",
    zi_random = ,
    zero_inflated_random = "re.zi",
    zi_prob = "zi.prob",
    survival = "surv",
    cumulative_hazard = "cumhaz",
    simulate = "sim",
    simulate_random = "sim_re",
    type
  )

  if (missing(interval)) {
    if (type %in% c("re", "re.zi")) {
      interval <- "prediction"
    } else {
      interval <- "confidence"
    }
  }

  ## TODO: add warnings later

  # handle deprectated arguments
  if (!missing(ci.lvl)) {
    ci_level <- ci.lvl
  }
  if (!missing(back.transform)) {
    back_transform <- back.transform
  }
  if (!missing(vcov.fun)) {
    vcov_fun <- vcov.fun
  }
  if (!missing(vcov.type)) {
    vcov_type <- vcov.type
  }
  if (!missing(vcov.args)) {
    vcov_args <- vcov.args
  }

  interval <- match.arg(interval, choices = c("confidence", "prediction"))
  model.name <- deparse(substitute(model))

  # process "terms", so we have the default character format. Furthermore,
  # check terms argument, to make sure that terms were not misspelled and are
  # indeed existing in the data
  if (!missing(terms)) {
    terms <- .reconstruct_focal_terms(terms, model = NULL)
  }

  # tidymodels?
  if (inherits(model, "model_fit")) {
    model <- model$fit
  }

  # for gamm/gamm4 objects, we have a list with two items, mer and gam
  # extract just the gam-part then
  if (is.gamm(model) || is.gamm4(model)) {
    model <- model$gam
  }

  # for sdmTMB objects, delta/hurdle models have family lists
  if (.is_delta_sdmTMB(model)) {
    insight::format_error("`ggpredict()` does not yet work with `sdmTMB` delta models.")
  }

  # prepare common arguments, for do.cal()
  fun_args <- list(
    ci.lvl = ci_level,
    type = type,
    typical = typical,
    ppd = ppd,
    condition = condition,
    back.transform = back_transform,
    vcov.fun = vcov_fun,
    vcov.type = vcov_type,
    vcov.args = vcov_args,
    interval = interval,
    verbose = verbose
  )

  if (inherits(model, "list") && !inherits(model, c("bamlss", "maxLik"))) {
    # we have a list of multiple model objects here ------------------------------
    result <- lapply(model, function(model_object) {
      full_args <- c(list(model = model_object, terms = terms), fun_args, list(...))
      do.call(ggpredict_helper, full_args)
    })
    class(result) <- c("ggalleffects", class(result))
  } else if (missing(terms) || is.null(terms)) {
    # if no terms are specified, we try to find all predictors ---------------
    predictors <- insight::find_predictors(model, effects = "fixed", component = "conditional", flatten = TRUE)
    result <- lapply(
      predictors,
      function(focal_term) {
        full_args <- c(list(model = model, terms = focal_term), fun_args, list(...))
        tmp <- do.call(ggpredict_helper, full_args)
        tmp$group <- focal_term
        tmp
      }
    )
    names(result) <- predictors
    class(result) <- c("ggalleffects", class(result))
  } else {
    # if terms are specified, we compute predictions for these terms ---------
    full_args <- c(list(model = model, terms = terms), fun_args, list(...))
    result <- do.call(ggpredict_helper, full_args)
  }

  if (!is.null(result)) {
    attr(result, "model.name") <- model.name
  }
  result
}


# workhorse that computes the predictions
# and creates the tidy data frames
ggpredict_helper <- function(model,
                             terms,
                             ci.lvl,
                             type,
                             typical,
                             ppd,
                             condition,
                             back.transform,
                             vcov.fun,
                             vcov.type,
                             vcov.args,
                             interval,
                             verbose = TRUE,
                             ...) {

  # check class of fitted model, to make sure we have just one class-attribute
  # (while "inherits()" may return multiple attributes)
  model_class <- get_predict_function(model)

  # sanity check, if terms really exist in data
  terms <- .check_vars(terms, model)

  # clean "terms" from possible brackets
  cleaned_terms <- .clean_terms(terms)

  # check model family
  model_info <- .get_model_info(model)

  # survival models are binomial
  if (model_class == "coxph" && type == "surv") {
    model_info$is_binomial <- TRUE
  }

  # get model frame
  model_frame <- .get_model_data(model)

  # expand model frame to data grid of unique combinations
  data_grid <- .data_grid(
    model = model, model_frame = model_frame, terms = terms, value_adjustment = typical,
    condition = condition, show_pretty_message = verbose, verbose = verbose
  )

  # save original frame, for labels, and original terms
  original_model_frame <- model_frame
  original_terms <- terms

  # clear argument from brackets
  terms <- cleaned_terms

  # compute predictions here -----
  prediction_data <- select_prediction_method(
    model_class = model_class,
    model = model,
    data_grid = data_grid,
    ci.lvl = ci.lvl,
    type = type,
    model_info = model_info,
    ppd = ppd,
    terms = original_terms,
    value_adjustment = typical,
    vcov.fun = vcov.fun,
    vcov.type = vcov.type,
    vcov.args = vcov.args,
    condition = condition,
    interval = interval,
    verbose = verbose,
    ...
  )

  # return if no predicted values have been computed
  if (is.null(prediction_data)) {
    return(NULL)
  }

  # remember if grouping variable was numeric, possibly needed for plotting
  attr(prediction_data, "continuous.group") <- attr(data_grid, "continuous.group")

  # for survival probabilities or cumulative hazards, we need
  # the "time" variable
  if (model_class == "coxph" && type %in% c("surv", "cumhaz")) {
    terms <- c("time", terms)
    cleaned_terms <- c("time", cleaned_terms)
  }
  # special handling for rqs
  if (model_class == "rqs" && !"tau" %in% cleaned_terms) {
    cleaned_terms <- c(cleaned_terms, "tau")
  }

  result <- .post_processing_predictions(
    model = model,
    prediction_data = prediction_data,
    original_model_frame = original_model_frame,
    cleaned_terms = cleaned_terms
  )

  # check if outcome is log-transformed, and if so,
  # back-transform predicted values to response scale
  # but first, save original predicted values, to save as attribute
  if (back.transform) {
    untransformed.predictions <- result$predicted
    response.transform <- insight::find_terms(model)[["response"]]
  } else {
    untransformed.predictions <- response.transform <- NULL
  }
  result <- .back_transform_response(model, result, back.transform, verbose = verbose)

  # add raw data as well
  attr(result, "rawdata") <- .get_raw_data(model, original_model_frame, terms)

  # no adjustment for type = "simulate"
  if (type == "sim") {
    attributes(data_grid)$constant.values <- NULL
  }

  .post_processing_labels(
    model = model,
    result = result,
    original_model_frame = original_model_frame,
    data_grid = data_grid,
    cleaned_terms = cleaned_terms,
    original_terms = original_terms,
    model_info = model_info,
    type = type,
    prediction.interval = attr(prediction_data, "prediction.interval", exact = TRUE),
    at_list = .data_grid(
      model = model, model_frame = original_model_frame, terms = original_terms,
      value_adjustment = typical, condition = condition, show_pretty_message = FALSE,
      emmeans.only = TRUE, verbose = FALSE
    ),
    condition = condition,
    ci.lvl = ci.lvl,
    untransformed.predictions = untransformed.predictions,
    back.transform = back.transform,
    response.transform = response.transform,
    verbose = verbose
  )
}
