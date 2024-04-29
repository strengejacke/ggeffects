#' @title Adjusted predictions from regression models
#' @name ggpredict
#'
#' @description
#' The **ggeffects** package computes marginal means and adjusted predicted
#' values for the response, at the margin of specific values or levels from
#' certain model terms. The package is built around three core functions:
#' `predict_response()` (understanding results), `test_predictions()` (testing
#' results for statistically significant differences) and `plot()` (communicate
#' results).
#'
#' By default, adjusted predictions or marginal means are by returned on the
#' *response* scale, which is the easiest and most intuitive scale to interpret
#' the results. There are other options for specific models as well, e.g. with
#' zero-inflation component (see documentation of the `type`-argument). The
#' result is returned as consistent data frame, which is nicely printed by
#' default. `plot()` can be used to easily create figures.
#'
#' The main function to calculate marginal means and adjusted predictions is
#' `predict_response()`. In previous versions of **ggeffects**, the functions
#' `ggpredict()`, `ggemmeans()`, `ggeffect()` and `ggaverage()` were used to
#' calculate marginal means and adjusted predictions. These functions are still
#' available, but `predict_response()` as a "wrapper" around these functions is
#' the preferred way to do this now.
#'
#' @param model A model object, or a list of model objects.
#' @param type Character, indicating whether predictions should be conditioned
#' on specific model components or not. Consequently, most options only apply
#' for survival models, mixed effects models and/or models with zero-inflation
#' (and their Bayesian counter-parts); only exeption is `type = "simulate"`,
#' which is available for some other model classes as well (which respond to
#' `simulate()`).
#'
#' **Note:** For `brmsfit`-models with zero-inflation component, there is no
#' `type = "zero_inflated"` nor `type = "zi_random"`; predicted values for these
#' models *always* condition on the zero-inflation part of the model. The same
#' is true for `MixMod`-models from **GLMMadaptive** with zero-inflation component
#' (see 'Details').
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
#'     Only applies to `margin = "mean_reference"`, and only for Bayesian
#'     models of class `stanreg` or `brmsfit`. Computes the posterior predictive
#'     distribution. It is the same as setting `type = "fixed"` in combination with
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
#'     Only applies to `margin = "mean_reference"`,, and only for Bayesian
#'     models of class `stanreg` or `brmsfit`. Computes the posterior predictive
#'     distribution. It is the same as setting `type = "random"` in combination with
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
#' @param ppd Logical, if `TRUE`, predictions for Stan-models are based on the
#' posterior predictive distribution [`rstantools::posterior_predict()`]. If
#' `FALSE` (the default), predictions are based on posterior draws of the linear
#' predictor [`rstantools::posterior_epred()`].
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
#' weights are available, the function falls back to `"mean"`. **Note** that this
#' argument is ignored for `predict_response()`, because the `margin` argument
#' takes care of this.
#' @param interval Type of interval calculation, can either be `"confidence"`
#' (default) or `"prediction"`. May be abbreviated. Unlike *confidence intervals*,
#' *prediction intervals* include the residual variance (sigma^2) to account for
#' the uncertainty of predicted values. For mixed models, `interval = "prediction"`
#' is the default for `type = "random"`. When `type = "fixed"`, the default is
#' `interval = "confidence"`. Note that prediction intervals are not available
#' for all models, but only for models that work with [`insight::get_sigma()`].
#' @param ci.lvl,vcov.fun,vcov.type,vcov.args,back.transform Deprecated arguments.
#' Please use `ci_level`, `vcov_fun`, `vcov_type`, `vcov_args` and `back_transform`
#' instead.
#' @param ... Arguments are passed down to `ggpredict()` (further down to `predict()`)
#' or `ggemmeans()` (and thereby to `emmeans::emmeans()`), If `type = "simulate"`,
#' `...` may also be used to set the number of simulation, e.g. `nsim = 500`.
#' When calling `ggeffect()`, further arguments passed down to `effects::Effect()`.
#' @inheritParams predict_response
#'
#' @details
#' Please see `?predict_response` for details and examples.
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
  type_and_ppd <- .validate_type_argument(model, type, ppd)
  type <- type_and_ppd$type
  ppd <- type_and_ppd$ppd

  if (missing(interval)) {
    if (type %in% c("re", "re.zi")) {
      interval <- "prediction"
    } else {
      interval <- "confidence"
    }
  }

  ## TODO: remove deprecated later

  # handle deprectated arguments
  if (!missing(ci.lvl)) {
    ci_level <- ci.lvl
    insight::format_warning("Argument `ci.lvl` is deprecated and will be removed in the future. Please use `ci_level` instead.") # nolint
  }
  if (!missing(back.transform)) {
    back_transform <- back.transform
    insight::format_warning("Argument `back.transform` is deprecated and will be removed in the future. Please use `back_transform` instead.") # nolint
  }
  if (!missing(vcov.fun)) {
    vcov_fun <- vcov.fun
    insight::format_warning("Argument `vcov.fun` is deprecated and will be removed in the future. Please use `vcov_fun` instead.") # nolint
  }
  if (!missing(vcov.type)) {
    vcov_type <- vcov.type
    insight::format_warning("Argument `vcov.type` is deprecated and will be removed in the future. Please use `vcov_type` instead.") # nolint
  }
  if (!missing(vcov.args)) {
    vcov_args <- vcov.args
    insight::format_warning("Argument `vcov.args` is deprecated and will be removed in the future. Please use `vcov_args` instead.") # nolint
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
    vcov.args = .get_variance_covariance_matrix(model, vcov.fun, vcov.args, vcov.type, skip_if_null = TRUE, verbose = FALSE), # nolint
    margin = "mean_reference",
    verbose = verbose
  )
}
