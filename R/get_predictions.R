#' @title S3-class definition for the ggeffects package
#' @name get_predictions
#'
#' @description `get_predictions()` is the core function to return adjusted
#' predictions for a model. Basically, the input contains the model object and
#' a data grid that is typically used for the `newdata` argument of the
#' `predict()` method. This can be used as S3-method for own classes, to add
#' support for new models in **ggeffects** and is only relevant for package
#' developers.
#'
#' @param model,terms,ci_level,type,typical,vcov,vcov_args,condition,interval,bias_correction,verbose Arguments
#' from the call to `predict_response()` that are passed down to `get_predictions()`.
#' @param data_grid A data frame containing the data grid (or reference grid)
#' with all relevant values of predictors for which the adjusted predictions
#' should be made. Typically the data frame that is passed to the `newdata`
#' argument in `predict()`. A data grid can be created with functions like
#' [`data_grid()`] or [`insight::get_datagrid()`].
#' @param model_info An object returned by [`insight::model_info()`].
#'
#' @details
#' The above mentioned arguments are all passed from `predict_response()` to
#' `get_predictions()`, no matter if they are required to calculate predictions
#' or not. Thus, it is not necessary to accept or process all of those
#' arguments, but they can be used to modulate certain settings when calculating
#' predictions. It is important that the function returns a data frame with a
#' specific structure:
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
