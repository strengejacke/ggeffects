.post_processing_labels <- function(model,
                                    result,
                                    original_model_frame,
                                    data_grid,
                                    cleaned_terms,
                                    original_terms,
                                    model_info,
                                    type,
                                    prediction.interval,
                                    at_list,
                                    condition = NULL,
                                    ci_level = 0.95,
                                    untransformed.predictions = NULL,
                                    back_transform = FALSE,
                                    response.transform = NULL,
                                    vcov_args = NULL,
                                    margin = NULL,
                                    model_name = NULL,
                                    bias_correction = FALSE,
                                    verbose = TRUE) {
  # check if outcome is log-transformed, and if so,
  # back-transform predicted values to response scale
  # but first, save original predicted values, to save as attribute
  if (back_transform) {
    untransformed.predictions <- result$predicted
    response.transform <- insight::find_terms(model, verbose = FALSE)[["response"]]
  } else {
    untransformed.predictions <- response.transform <- NULL
  }
  result <- .back_transform_response(model, result, back_transform, verbose = verbose)
  attr(result, "model.name") <- model_name

  # add raw data as well
  attr(result, "rawdata") <- .back_transform_data(
    model,
    mydf = .get_raw_data(model, original_model_frame, cleaned_terms),
    back_transform = back_transform
  )

  # get axis titles and labels
  all.labels <- .get_axis_titles_and_labels(
    model,
    original_model_frame = original_model_frame,
    terms = cleaned_terms,
    fun = .get_model_function(model),
    model_info = model_info,
    no.transform = FALSE,
    type = type,
    at_list = at_list,
    averaged_predictions = isTRUE(attr(result, "averaged_predictions", exact = TRUE))
  )

  # set attributes with necessary information
  .set_attributes_and_class(
    data = result,
    model = model,
    t.title = all.labels$t.title,
    x.title = all.labels$x.title,
    y.title = all.labels$y.title,
    l.title = all.labels$l.title,
    legend.labels = attr(result, "legend.labels"),
    x.axis.labels = all.labels$axis.labels,
    model_info = model_info,
    constant.values = attr(data_grid, "constant.values", exact = TRUE),
    terms = cleaned_terms,
    original_terms = original_terms,
    at_list = at_list,
    n.trials = attr(data_grid, "n.trials", exact = TRUE),
    prediction.interval = prediction.interval,
    condition = condition,
    ci_level = ci_level,
    type = type,
    untransformed.predictions = untransformed.predictions,
    back_transform = back_transform,
    response.transform = response.transform,
    original_model_frame = original_model_frame,
    vcov_args = vcov_args,
    margin = margin,
    bias_correction = bias_correction,
    verbose = verbose
  )
}
