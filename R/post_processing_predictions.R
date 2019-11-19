.post_processing_predictions <- function(model, prediction_data, original_model_frame, cleaned_terms, x.as.factor) {
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
  mydf <- prediction_data[, stats::na.omit(match(
    c(cleaned_terms, "predicted", "std.error", "conf.low", "conf.high", "response.level"),
    colnames(prediction_data)
  ))]

  # name and sort columns, depending on groups, facet and panel
  mydf <- .prepare_columns(mydf, cleaned_terms)

  # grouping variable may not be labelled
  # do this here, so we convert to labelled factor later
  mydf <- .add_labels_to_groupvariable(mydf, original_model_frame, cleaned_terms)

  # convert grouping variable to factor, for proper legend
  mydf <- .groupvariable_to_labelled_factor(mydf)

  # check if we have legend labels
  legend.labels <- sjlabelled::get_labels(mydf$group)

  # if we had numeric variable w/o labels, these still might be numeric
  # make sure we have factors here for our grouping and facet variables
  if (is.numeric(mydf$group))
    mydf$group <- sjmisc::to_factor(mydf$group)

  # remember if x was a factor
  x.is.factor <- ifelse(is.factor(mydf$x), "1", "0")

  # x needs to be numeric
  if (!x.as.factor) mydf$x <- sjlabelled::as_numeric(mydf$x)

  # sort values
  mydf <- sjmisc::remove_empty_cols(mydf[order(mydf$x, mydf$group), ])

  if (.obj_has_name(mydf, "facet") && is.numeric(mydf$facet)) {
    mydf$facet <- sjmisc::to_factor(mydf$facet)
    attr(mydf, "numeric.facet") <- TRUE
  }

  attr(mydf, "legend.labels") <- legend.labels
  attr(mydf, "x.is.factor") <- x.is.factor

  mydf
}






# name and sort columns, depending on groups, facet and panel
.prepare_columns <- function(mydf, cleaned_terms) {
  columns <- c("x", "predicted", "std.error", "conf.low", "conf.high", "response.level", "group", "facet", "panel")

  # with or w/o grouping factor?
  if (length(cleaned_terms) == 1) {
    colnames(mydf)[1] <- "x"
    # convert to factor for proper legend
    mydf$group <- sjmisc::to_factor(1)
  } else if (length(cleaned_terms) == 2) {
    colnames(mydf)[1:2] <- c("x", "group")
  } else if (length(cleaned_terms) == 3) {
    colnames(mydf)[1:3] <- c("x", "group", "facet")
  } else if (length(cleaned_terms) == 4) {
    colnames(mydf)[1:4] <- c("x", "group", "facet", "panel")
  }

  # sort columns
  mydf[, columns[columns %in% colnames(mydf)]]
}
