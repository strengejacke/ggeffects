#' @rdname ggpredict
#' @export
ggaverage <- function(model,
                      terms,
                      ci_level = 0.95,
                      typical = "mean",
                      condition = NULL,
                      back_transform = TRUE,
                      verbose = TRUE,
                      ...) {
  insight::check_if_installed("marginaleffects")
  # check class of fitted model, to make sure we have just one class-attribute
  # (while "inherits()" may return multiple attributes)
  model_class <- get_predict_function(model)

  # check terms argument, to make sure that terms were not misspelled
  # and are indeed existing in the data
  terms <- .check_vars(terms, model)
  cleaned_terms <- .clean_terms(terms)

  # check model family
  model_info <- .get_model_info(model)

  # get model frame
  model_frame <- .get_model_data(model)

  # expand model frame to data grid of unique combinations
  data_grid <- .data_grid(
    model = model, model_frame = model_frame, terms = terms, value_adjustment = typical,
    condition = condition, show_pretty_message = verbose
  )

  # save original frame, for labels, and original terms
  original_model_frame <- model_frame
  original_terms <- terms

  # clear argument from brackets
  terms <- cleaned_terms

  # calculate average predictions
  at_list <- lapply(data_grid, unique)
  prediction_data <- marginaleffects::avg_predictions(
    model,
    variables = at_list[terms],
    conf_level = ci_level,
    type = "response",
    df = .get_df(model),
    ...
  )

  # return if no predicted values have been computed
  if (is.null(prediction_data)) {
    return(NULL)
  }

  # we want a "clear" data frame here
  prediction_data <- as.data.frame(prediction_data)
  # rename variables, keep predictions and std.error
  prediction_data <- .var_rename(
    prediction_data,
    estimate = "predicted"
  )
  # select variables and merge predictions with data grid
  prediction_data <- prediction_data[c(terms, "predicted", "std.error", "conf.low", "conf.high")]
  prediction_data <- merge(data_grid, prediction_data, all.x = TRUE)

  # add attributes, needed for plotting etc.
  attributes(prediction_data) <- utils::modifyList(attributes(data_grid), attributes(prediction_data))
  attr(prediction_data, "std.error") <- prediction_data$std.error

  # for avg_predictions, we average over factor levels, so no adjustments here
  attributes(prediction_data)$constant.values <- NULL
  attributes(data_grid)$constant.values <- NULL

  result <- .post_processing_predictions(
    model = model,
    prediction_data = prediction_data,
    original_model_frame = original_model_frame,
    cleaned_terms = cleaned_terms
  )

  # check if outcome is log-transformed, and if so,
  # back-transform predicted values to response scale
  # but first, save original predicted values, to save as attribute
  if (back_transform) {
    untransformed.predictions <- result$predicted
    response.transform <- insight::find_terms(model)[["response"]]
  } else {
    untransformed.predictions <- response.transform <- NULL
  }
  result <- .back_transform_response(model, result, back_transform, verbose = verbose)

  # add raw data as well
  attr(result, "rawdata") <- .get_raw_data(model, original_model_frame, terms)

  .post_processing_labels(
    model = model,
    result = result,
    original_model_frame = original_model_frame,
    data_grid = data_grid,
    cleaned_terms = cleaned_terms,
    original_terms = original_terms,
    model_info = model_info,
    type = "fixed",
    prediction.interval = FALSE,
    at_list = at_list,
    condition = condition,
    ci.lvl = ci_level,
    untransformed.predictions = untransformed.predictions,
    back.transform = back_transform,
    response.transform = response.transform,
    verbose = verbose
  )
}
