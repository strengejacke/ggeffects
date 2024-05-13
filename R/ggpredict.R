#' @title Adjusted predictions from regression models
#' @name ggpredict
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
#' @inherit predict_response return
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
  ## TODO: remove deprecated later
  if (!missing(ppd) && isTRUE(ppd)) {
    insight::format_warning("Argument `ppd` is deprecated and will be removed in the future. Please use `interval` instead.") # nolint
  }

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
  } else if (is.null(interval)) {
    interval <- "confidence"
  }

  # make sure we have valid values
  interval <- match.arg(interval, c("confidence", "prediction"))

  ## TODO: remove when deprecated

  # update interval - if we have ppd = TRUE, we have prediction intervals
  if (isTRUE(ppd)) {
    interval <- "prediction"
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

  # check if we have random effects in the model, and if predictions should be
  # done for random effects only (i.e. all focal terms are specified as random
  # effects in the model). If so, we need to tell the user that they should
  # better to `margin = "empirical"`
  if (!type %in% c("re", "re.zi")) {
    .check_focal_for_random(model, terms, verbose)
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
  attr(result, "rawdata") <- .back_transform_data(
    model,
    mydf = .get_raw_data(model, original_model_frame, terms),
    back.transform = back.transform
  )

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


.check_focal_for_random <- function(model, terms, verbose) {
  random_pars <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)
  if (!is.null(random_pars) && all(.clean_terms(terms) %in% random_pars) && verbose) {
    insight::format_warning("All focal terms are included as random effects in the model. To calculate predictions for random effects, either use `margin = \"empirical\"` or set `type = \"random\"` (possibly together with `interval = \"confidence\"`) to get meaningful results.") # nolint
  }
}
