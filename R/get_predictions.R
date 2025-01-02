#' @title S3-class definition for the ggeffects package
#' @name get_predictions
#'
#' @description `get_predictions()` is the core function to return adjusted
#' predictions for a model, when calling `ggpredict()` or `predict_response()`
#' with `margin = "mean_reference"` (the default option for `margin`).
#' Basically, the input contains the model object and a data grid that is
#' typically used for the `newdata` argument of the `predict()` method.
#' `get_predictions()` can be used as S3-method for own classes, to add support
#' for new models in **ggeffects** and is only relevant for package developers.
#'
#' There are no S3-class definitions for `ggemmeans()` or `ggaverage()`, because
#' these functions simply call methods from the **emmeans** or **marginaleffects**
#' packages. Hence, methods should be written for those packages, too, if a
#' model-object should work with `ggemmeans()` or `ggaverage()`.
#'
#' @param model,terms,ci_level,type,typical,vcov,vcov_args,condition,parameter,interval,bias_correction,verbose Arguments
#' from the call to `predict_response()` that are passed down to `get_predictions()`.
#' Note that `bias_correction` is usally already processed in `predict_response()`
#' and thus doesn't need further handling in `get_predictions()`, unless you
#' need to re-calculate the link-inverse-function (argument `link_inverse`)
#' inside the `get_predictions()` method.
#' @param data_grid A data frame containing the data grid (or reference grid)
#' with all relevant values of predictors for which the adjusted predictions
#' should be made. Typically the data frame that is passed to the `newdata`
#' argument in `predict()`. A data grid can be created with functions like
#' [`data_grid()`] or [`insight::get_datagrid()`].
#' @param model_info An object returned by [`insight::model_info()`].
#' @param link_inverse The model's family link-inverse function. Can be retrieved
#' using `insight::link_inverse()`.
#' @param ... Further arguments, passed to `predict()` or other methods used
#' in `get_predictions()`.
#'
#' @details
#' Adding support for **ggeffects** is quite easy. The user-level function is
#' `predict_response()`, which either calls `ggpredict()`, `ggemmeans()` or
#' `ggaverage()`. These function, in turn, call `predict()`, `emmeans::emmeans()`
#' or `marginaleffects::avg_predictions()`. Following needs to be done to add
#' support for new model classes:
#' - **emmeans**: if your model is supported by emmeans, it is automatically
#'   supported by `ggemmeans()`. Thus, you need to add the corresponding methods
#'   to your package so that your model class is supported by **emmeans.
#' - **marginaleffects**: similar to **emmeans**, if your package is supported
#'   by the **marginaleffects** package, it works with `ggaverage()`.
#' - **predict**: in order to make your model class work with `ggpredict()`,
#'   you need to add a `get_predictions()` method. The here documented arguments
#'   are *all* passed from `predict_response()` to `get_predictions()`, no
#'   matter if they are required to calculate predictions or not. Thus, it is
#'   not necessary to process all of those arguments, but they can be used to
#'   modulate certain settings when calculating predictions. Note that if your
#'   method does not define all mentioned arguments, these are still passed via
#'   `...` - make sure that further methods in your `get_predictions()` method
#'   still work when they process the `...`. It is important that the function
#'   returns a data frame with a specific structure, namely the data grid and
#'   the columns `predicted`, `conf.low`, and `conf.high`. Predictions and
#'   intervals should be on the response scale.
#'
#' A simple example for an own class-implementation for Gaussian-alike models
#' could look like this:
#'
#' ```
#' get_predictions.own_class <- function(model, data_grid, ci_level = 0.95, ...) {
#'   predictions <- predict(
#'     model,
#'     newdata = data_grid,
#'     type = "response",
#'     se.fit = !is.na(ci_level),
#'     ...
#'   )
#'
#'   # do we have standard errors?
#'   if (is.na(ci_level)) {
#'     # copy predictions
#'     data_grid$predicted <- as.vector(predictions)
#'   } else {
#'     # copy predictions
#'     data_grid$predicted <- predictions$fit
#'
#'     # calculate CI
#'     data_grid$conf.low <- predictions$fit - qnorm(0.975) * predictions$se.fit
#'     data_grid$conf.high <- predictions$fit + qnorm(0.975) * predictions$se.fit
#'
#'     # optional: copy standard errors
#'     attr(data_grid, "std.error") <- predictions$se.fit
#'   }
#'
#'   data_grid
#' }
#' ```
#'
#' A simple example for an own class-implementation for non-Gaussian-alike models
#' could look like this (note the use of the link-inverse function `link_inverse()`,
#' which is passed to the `link_inverse` argument):
#'
#' ```
#' get_predictions.own_class <- function(model,
#'                                       data_grid,
#'                                       ci_level = 0.95,
#'                                       link_inverse = insight::link_inverse(model),
#'                                       ...) {
#'   predictions <- predict(
#'     model,
#'     newdata = data_grid,
#'     type = "link", # for non-Gaussian, return on link-scale
#'     se.fit = !is.na(ci_level),
#'     ...
#'   )
#'
#'   # do we have standard errors?
#'   if (is.na(ci_level)) {
#'     # copy predictions
#'     data_grid$predicted <- link_inverse(as.vector(predictions))
#'   } else {
#'     # copy predictions, use link-inverse to back-transform
#'     data_grid$predicted <- link_inverse(predictions$fit)
#'
#'     # calculate CI
#'     data_grid$conf.low <- link_inverse(
#'       predictions$fit - qnorm(0.975) * predictions$se.fit
#'     )
#'     data_grid$conf.high <- link_inverse(
#'       predictions$fit + qnorm(0.975) * predictions$se.fit
#'     )
#'
#'     # optional: copy standard errors
#'     attr(data_grid, "std.error") <- predictions$se.fit
#'   }
#'
#'   data_grid
#' }
#' ```
#'
#' @return
#' A data frame that contains
#' - the data grid (from the argument `data_grid`)
#' - the columns `predicted`, `conf.low`, and `conf.high`
#' - optionally, the attribute `"std.error"` with the standard errors.
#'
#' Note that predictions and confidence intervals should already be transformed
#' to the _response_ scale (e.g., by using `insight::link_inverse()`). The
#' *standard errors* are always on the link scale (not transformed).
#'
#' If values are not available (for example, confidence intervals), set their
#' value to `NA`.
#'
#' @export
get_predictions <- function(model, ...) {
  UseMethod("get_predictions")
}


#' @rdname get_predictions
#' @export
get_predictions.default <- function(model,
                                    data_grid = NULL,
                                    terms = NULL,
                                    ci_level = 0.95,
                                    type = NULL,
                                    typical = NULL,
                                    vcov = NULL,
                                    vcov_args = NULL,
                                    condition = NULL,
                                    interval = "confidence",
                                    bias_correction = FALSE,
                                    link_inverse = insight::link_inverse(model),
                                    model_info = NULL,
                                    verbose = TRUE,
                                    ...) {
  prdat <- as.data.frame(insight::get_predicted(
    model,
    data = data_grid,
    predict = "expectation",
    ...
  ))

  # copy predictions
  data_grid$predicted <- prdat$Predicted

  if (!is.null(prdat$CI_low) && !is.null(prdat$CI_high)) {
    # No CI
    data_grid$conf.low <- prdat$CI_low
    data_grid$conf.high <- prdat$CI_high
  } else {
    # No CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  data_grid
}


# helper ---------------------------------------------


.get_df <- function(model) {
  dof <- .safe(unique(insight::get_df(model, type = "wald", verbose = FALSE)), Inf)
  if (length(dof) > 1) {
    dof <- Inf
  }
  dof
}


.generic_prediction_data <- function(model,
                                     data_grid,
                                     link_inverse,
                                     prediction_data,
                                     se,
                                     ci_level,
                                     typical,
                                     terms,
                                     vcov,
                                     vcov_args,
                                     condition = NULL,
                                     interval = NULL) {
  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- (1 + ci_level) / 2
  } else {
    ci <- 0.975
  }

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  # copy predictions
  if (typeof(prediction_data) == "double") {
    .predicted <- prediction_data
  } else {
    .predicted <- prediction_data$fit
  }

  # get standard errors, if computed
  if (.obj_has_name(prediction_data, "se.fit")) {
    se.fit <- prediction_data$se.fit
    # reset interval, since we have normal confidence intervals already here
    if (interval == "confidence") interval <- NULL
  } else {
    se.fit <- NULL
  }

  # get predicted values, on link-scale
  data_grid$predicted <- .predicted

  # for poisson model, we need to compute prediction intervals in a different way
  info <- insight::model_info(model)
  if (info$is_poisson && (!is.null(interval) && interval == "prediction")) {
    pred_int <- .prediction_interval_glm(
      model,
      predictions = .predicted,
      info = info,
      ci = ci_level,
      linkinv = link_inverse
    )
    data_grid$conf.low <- pred_int$CI_low
    data_grid$conf.high <- pred_int$CI_high
  } else {
    # did user request robust standard errors?
    if (!is.null(vcov) || (!is.null(interval) && se)) {
      se.pred <- .standard_error_predictions(
        model = model,
        prediction_data = data_grid,
        typical = typical,
        terms = terms,
        vcov = vcov,
        vcov_args = vcov_args,
        condition = condition,
        interval = interval
      )
      if (.check_returned_se(se.pred)) {
        fitfram <- se.pred$prediction_data
        se.fit <- se.pred$se.fit
        se <- TRUE
      } else {
        se.fit <- NULL
        se <- FALSE
      }
    } else {
      se.pred <- NULL
    }

    # did user request standard errors? if yes, compute CI
    if (se && !is.null(se.fit)) {
      data_grid$conf.low <- link_inverse(data_grid$predicted - tcrit * se.fit)
      data_grid$conf.high <- link_inverse(data_grid$predicted + tcrit * se.fit)
      # copy standard errors
      attr(data_grid, "std.error") <- se.fit
      if (!is.null(se.pred) && length(se.pred) > 0) {
        attr(data_grid, "prediction.interval") <- attr(se.pred, "prediction_interval")
      }
    } else {
      # No CI
      data_grid$conf.low <- NA
      data_grid$conf.high <- NA
    }
  }

  # transform predicted values
  data_grid$predicted <- link_inverse(data_grid$predicted)

  data_grid
}


.prediction_interval_glm <- function(x, predictions, info, ci = 0.95, linkinv = NULL, ...) {
  linkfun <- insight::link_function(x)
  if (is.null(linkinv)) {
    linkinv <- insight::link_inverse(x)
  }
  alpha <- 1 - ci
  prob <- c(alpha / 2, 1 - alpha / 2)

  if (info$is_binomial) {
    p <- linkinv(predictions)
    ci_low <- stats::qbinom(prob[1], size = 1, prob = p)
    ci_high <- stats::qbinom(prob[2], size = 1, prob = p)
  } else if (info$is_poisson) {
    rate <- linkinv(predictions)
    ci_low <- stats::qpois(prob[1], lambda = rate)
    ci_high <- stats::qpois(prob[2], lambda = rate)
  }

  data.frame(CI_low = ci_low, CI_high = ci_high)
}
