.post_processing_predictions <- function(model, prediction_data, original_model_frame, cleaned_terms) {
  # check for correct terms specification
  if (!all(cleaned_terms %in% colnames(prediction_data))) {
    stop("At least one term specified in `terms` is no valid model term.", call. = FALSE)
  }

  # copy standard errors
  if (!.obj_has_name(prediction_data, "std.error")) {
    prediction_data$std.error <- attr(prediction_data, "std.error")
  } else {
    attr(prediction_data, "std.error") <- prediction_data$std.error
  }

  # now select only relevant variables: the predictors on the x-axis,
  # the predictions and the originial response vector (needed for scatter plot)
  columns_to_keep <- c(cleaned_terms, "predicted", "std.error", "conf.low", "conf.high", "response.level")
  result <- prediction_data[, intersect(columns_to_keep, colnames(prediction_data))]

  # name and sort columns, depending on groups, facet and panel
  result <- .prepare_columns(result, cleaned_terms)

  # grouping variable may not be labelled
  # do this here, so we convert to labelled factor later
  result <- .add_labels_to_groupvariable(result, original_model_frame, cleaned_terms)

  # convert grouping variable to factor, for proper legend
  result <- .groupvariable_to_labelled_factor(result)

  # check if we have legend labels
  legend.labels <- sjlabelled::get_labels(result$group)

  # if we had numeric variable w/o labels, these still might be numeric
  # make sure we have factors here for our grouping and facet variables
  if (is.numeric(result$group)) {
    result$group <- as.factor(result$group)
  }

  # remember if x was a factor
  x.is.factor <- ifelse(is.factor(result$x), "1", "0")

  # sort values
  result <- result[order(result$x, result$group), ]
  empty_columns <- which(colSums(is.na(result)) == nrow(result))
  if (length(empty_columns)) result <- result[, -empty_columns]

  if (.obj_has_name(result, "facet") && is.numeric(result$facet)) {
    result$facet <- as.factor(result$facet)
    attr(result, "numeric.facet") <- TRUE
  }

  attr(result, "legend.labels") <- legend.labels
  attr(result, "x.is.factor") <- x.is.factor
  attr(result, "continuous.group") <- attr(prediction_data, "continuous.group") & is.null(attr(original_model_frame[[cleaned_terms[2]]], "labels"))


  result
}






# name and sort columns, depending on groups, facet and panel
.prepare_columns <- function(result, cleaned_terms) {
  columns <- c("x", "predicted", "std.error", "conf.low", "conf.high", "response.level", "group", "facet", "panel")

  # with or w/o grouping factor?
  if (length(cleaned_terms) == 1) {
    colnames(result)[1] <- "x"
    # convert to factor for proper legend
    result$group <- as.factor(1)
  } else if (length(cleaned_terms) == 2) {
    colnames(result)[1:2] <- c("x", "group")
  } else if (length(cleaned_terms) == 3) {
    colnames(result)[1:3] <- c("x", "group", "facet")
  } else if (length(cleaned_terms) == 4) {
    colnames(result)[1:4] <- c("x", "group", "facet", "panel")
  }

  # sort columns
  result[, columns[columns %in% colnames(result)]]
}
