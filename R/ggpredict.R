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
                      interval = "confidence",
                      back_transform = TRUE,
                      vcov = NULL,
                      vcov_args = NULL,
                      bias_correction = FALSE,
                      verbose = TRUE,
                      ...) {
  # check arguments
  type <- .validate_type_argument(model, type)

  ## TODO: remove deprecated later
  vcov <- .prepare_vcov_args(vcov, ...)

  # check formula
  insight::formula_ok(model)

  # make sure we have valid values
  interval <- insight::validate_argument(interval, c("confidence", "prediction"))

  model.name <- deparse(substitute(model))

  # check if bias-correction is appropriate
  bias_correction <- .check_bias_correction(
    model,
    type = type,
    bias_correction = bias_correction,
    verbose = verbose
  )

  # process "terms", so we have the default character format. Furthermore,
  # check terms argument, to make sure that terms were not misspelled and are
  # indeed existing in the data
  if (!missing(terms)) {
    terms <- .reconstruct_focal_terms(terms, model = NULL)
  }

  # check model object, e.g. for gam's or class model_fit
  model <- .check_model_object(model)

  # prepare common arguments, for do.cal()
  fun_args <- list(
    ci_level = ci_level,
    type = type,
    typical = typical,
    condition = condition,
    back_transform = back_transform,
    vcov = vcov,
    vcov_args = vcov_args,
    interval = interval,
    bias_correction = bias_correction,
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
    predictors <- insight::find_predictors(
      model,
      effects = "fixed",
      component = "conditional",
      flatten = TRUE,
      verbose = FALSE
    )
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
                             ci_level,
                             type,
                             typical,
                             condition,
                             back_transform,
                             vcov,
                             vcov_args,
                             interval,
                             bias_correction = FALSE,
                             verbose = TRUE,
                             ...) {
  # sanity check, if terms really exist in data
  terms <- .check_vars(terms, model)

  # clean "terms" from possible brackets
  cleaned_terms <- .clean_terms(terms)

  # check model family
  model_info <- .get_model_info(model)

  # survival models are binomial
  if (inherits(model, "coxph") && type == "survival") {
    model_info$is_binomial <- TRUE
  }

  # check if we have random effects in the model, and if predictions should be
  # done for random effects only (i.e. all focal terms are specified as random
  # effects in the model). If so, we need to tell the user that they should
  # better to `margin = "empirical"`
  .check_focal_for_random(model, terms, type, verbose)

  # get model frame
  model_frame <- .get_model_data(model)

  # expand model frame to data grid of unique combinations
  data_grid <- .data_grid(
    model = model, model_frame = model_frame, terms = terms, typical = typical,
    condition = condition, show_pretty_message = verbose, verbose = verbose
  )

  # save original frame, for labels, and original terms
  original_model_frame <- model_frame
  original_terms <- terms

  # clear argument from brackets
  terms <- cleaned_terms

  linv <- .link_inverse(model, bias_correction = bias_correction, ...)
  if (is.null(linv)) linv <- function(x) x

  # compute predictions here -----
  prediction_data <- get_predictions(
    model,
    data_grid = data_grid,
    terms = original_terms,
    ci_level = ci_level,
    type = type,
    typical = typical,
    vcov = vcov,
    vcov_args = vcov_args,
    condition = condition,
    interval = interval,
    bias_correction = bias_correction,
    link_inverse = linv,
    model_info = model_info,
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
  if (inherits(model, "coxph") && type %in% c("survival", "cumulative_hazard")) {
    terms <- c("time", terms)
    cleaned_terms <- c("time", cleaned_terms)
  }
  # special handling for rqs
  if (inherits(model, "rqs") && !"tau" %in% cleaned_terms) {
    cleaned_terms <- c(cleaned_terms, "tau")
  }

  result <- .post_processing_predictions(
    model = model,
    prediction_data = prediction_data,
    original_model_frame = original_model_frame,
    cleaned_terms = cleaned_terms
  )

  # no adjustment for type = "simulate"
  if (type == "simulate") {
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
      typical = typical, condition = condition, show_pretty_message = FALSE,
      emmeans_only = TRUE, verbose = FALSE
    ),
    condition = condition,
    ci_level = ci_level,
    untransformed.predictions = untransformed.predictions,
    back_transform = back_transform,
    response.transform = response.transform,
    vcov_args = .get_variance_covariance_matrix(model, vcov, vcov_args, skip_if_null = TRUE, verbose = FALSE), # nolint
    margin = "mean_reference",
    model_name = NULL,
    bias_correction = bias_correction,
    verbose = verbose
  )
}


.check_focal_for_random <- function(model, terms, type, verbose) {
  random_pars <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)
  # check if focal terms are *only* random effects, but `type` is not `"random"`.
  # in this case, population level predictions is probably not what user wants
  if (verbose && !type %in% c("random", "zero_inflated_random") && !is.null(random_pars) && all(.clean_terms(terms) %in% random_pars)) {
    insight::format_alert("All focal terms are included as random effects in the model. To calculate predictions for random effects, either use `margin = \"empirical\"` or set `type = \"random\"` to get meaningful results.") # nolint
  }
  # check if *no* focal term is a random effect, but `type` *is* `"random"`.
  # in this case, user probably wants unit-level predictions
  if (verbose && type %in% c("random", "zero_inflated_random") && !is.null(random_pars) && !any(.clean_terms(terms) %in% random_pars)) {
    insight::format_alert("It seems that unit-level predictions are requested (`type = \"random\"`), but no random effects terms (grouping variables) are defined in the `terms` argument. Either add a random effects term to the `terms` argument, or set `type = \"fixed\"` to get meaningful results (in this case, population-level predictions).") # nolint
  }
}
