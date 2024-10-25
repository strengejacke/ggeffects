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
#' @param model,terms,ci_level,type,typical,vcov,vcov_args,condition,interval,bias_correction,verbose Arguments
#' from the call to `predict_response()` that are passed down to `get_predictions()`.
#' @param data_grid A data frame containing the data grid (or reference grid)
#' with all relevant values of predictors for which the adjusted predictions
#' should be made. Typically the data frame that is passed to the `newdata`
#' argument in `predict()`. A data grid can be created with functions like
#' [`data_grid()`] or [`insight::get_datagrid()`].
#' @param model_info An object returned by [`insight::model_info()`].
#' @param ... Further arguments, passed to `predict()` or other methods used
#' in `get_predictions()`.
#'
#' @details
#' The above mentioned arguments are all passed from `predict_response()` to
#' `get_predictions()`, no matter if they are required to calculate predictions
#' or not. Thus, it is not necessary to accept or process all of those
#' arguments, but they can be used to modulate certain settings when calculating
#' predictions. It is important that the function returns a data frame with a
#' specific structure, namely the data grid and the columns `predicted`,
#' `conf.low`, and `conf.high`. Predictions and intervals should be on the
#' response scale.
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
#' could look like this (note the use of the link-inverse function `linv()`):
#'
#' ```
#' get_predictions.own_class <- function(model, data_grid, ci_level = 0.95, ...) {
#'   # get link-inverse-function
#'   linv <- insight::link_inverse(model)
#'   if (is.null(linv)) {
#'     linv <- function(x) x
#'   }
#'
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
#'     data_grid$predicted <- linv(as.vector(predictions))
#'   } else {
#'     # copy predictions
#'     data_grid$predicted <- linv(predictions$fit) # use link-inverse to back-transform
#'
#'     # calculate CI
#'     data_grid$conf.low <- linv(predictions$fit - qnorm(0.975) * predictions$se.fit)
#'     data_grid$conf.high <- linv(predictions$fit + qnorm(0.975) * predictions$se.fit)
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
#' - the data grid
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
                                    ci_level,
                                    type = NULL,
                                    typical = NULL,
                                    vcov = NULL,
                                    vcov_args = NULL,
                                    condition = NULL,
                                    interval = "confidence",
                                    bias_correction = FALSE,
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
