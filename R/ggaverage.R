#' @rdname ggpredict
#' @export
ggaverage <- function(model,
                      terms,
                      ci_level = 0.95,
                      type = "fixed",
                      typical = "mean",
                      condition = NULL,
                      parameter = NULL,
                      back_transform = TRUE,
                      vcov = NULL,
                      vcov_args = NULL,
                      weights = NULL,
                      verbose = TRUE,
                      ...) {
  insight::check_if_installed("marginaleffects")

  # check arguments
  type <- .validate_type_argument(model, type, marginaleffects = TRUE)
  dot_args <- list(...)

  # check formula
  insight::formula_ok(model, verbose = verbose)

  # process "terms", so we have the default character format. Furthermore,
  # check terms argument, to make sure that terms were not misspelled and are
  # indeed existing in the data
  if (!missing(terms)) {
    terms <- .reconstruct_focal_terms(terms, model)
  }

  # clean "terms" from possible brackets
  cleaned_terms <- .clean_terms(terms)

  # check model family
  model_info <- .get_model_info(model)

  # get model frame
  model_frame <- .get_model_data(model)

  # model name, for later use in test_predictions
  model_name <- deparse(substitute(model))

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

  # if we have "vcov" arguments, and `margin` is "empirical", we
  # need to prepare the `vcov` argument for "marginaleffects".
  if (!is.null(vcov)) {
    vcov_arg <- .get_variance_covariance_matrix(model, vcov, vcov_args, skip_if_null = TRUE)
  } else {
    vcov_arg <- TRUE
  }

  # ci_level = NA should prevent the computation of confidence intervals
  # in marginaleffects, this is equivalent to setting vcov = FALSE
  if (is.na(ci_level)) {
    ci_level <- 0.95
    vcov_arg <- FALSE
  }

  # new policy for glmmTMB models
  if (inherits(model, "glmmTMB") && is.null(vcov_arg)) {
    vcov_arg <- TRUE
  }

  # variables used in at-list
  at_list_vars <- terms
  if (!is.null(condition)) {
    # if we condition on certain values, we need to include these
    # in the "variables" argument as well.
    at_list_vars <- unique(c(at_list_vars, names(condition)))
  }

  # handle dpar argument
  if ("dpar" %in% names(dot_args)) {
    parameter <- dpar
    dot_args$dpar <- NULL
  }

  # calculate average predictions
  at_list <- lapply(data_grid, unique)
  me_args <- list(
    model,
    variables = at_list[at_list_vars],
    conf_level = ci_level,
    type = type,
    df = .get_df(model),
    vcov = vcov_arg,
    wts = weights,
    dpar = parameter
  )
  prediction_data <- .call_me(
    "avg_predictions",
    me_args,
    dot_args,
    include_random = insight::is_mixed_model(model)
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
    estimate = "predicted",
    group = "response.level"
  )
  # select variables and merge predictions with data grid
  ordered_columns <- intersect(
    colnames(prediction_data),
    c(terms, "predicted", "std.error", "conf.low", "conf.high", "response.level")
  )
  prediction_data <- prediction_data[ordered_columns]
  # if we have a "response.level", save the row order
  if ("response.level" %in% colnames(prediction_data)) {
    prediction_data$.row_id <- seq_len(nrow(prediction_data))
  }
  # merge data grid (values of focal terms) to predictions
  prediction_data <- merge(data_grid, prediction_data, all.x = TRUE)
  # restore row order
  if (!is.null(prediction_data$.row_id)) {
    prediction_data <- prediction_data[order(prediction_data$.row_id), ]
    prediction_data$.row_id <- NULL
  }

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
    cleaned_terms = cleaned_terms,
    averaged_predictions = TRUE
  )

  .post_processing_labels_and_data(
    model = model,
    result = result,
    original_model_frame = original_model_frame,
    data_grid = data_grid,
    cleaned_terms = cleaned_terms,
    original_terms = original_terms,
    model_info = model_info,
    type = type,
    prediction.interval = FALSE,
    at_list = at_list,
    condition = condition,
    ci_level = ci_level,
    back_transform = back_transform,
    vcov_args = if (isTRUE(vcov_arg)) NULL else vcov_arg,
    margin = "empirical",
    model_name = model_name,
    parameter = parameter,
    verbose = verbose
  )
}
