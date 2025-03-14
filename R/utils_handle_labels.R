# add labels to grouping and facet variables, if these
# variables come from labelled data
.add_labels_to_groupvariable <- function(mydf, original_model_frame, terms) {
  # required for labelling
  if (!insight::check_if_installed("haven", quietly = TRUE)) {
    return(mydf)
  }

  grp.lbl <- .get_labels(
    original_model_frame[[terms[2]]],
    non.labelled = TRUE,
    values = "n",
    drop.unused = TRUE
  )

  # no new labels for labelled factors
  if (is.factor(mydf$group) && !.is_numeric_factor(mydf$group)) {
    grp.lbl <- NULL
  }

  # drop levels, if necessary
  if (is.factor(mydf$group) && .n_distinct(mydf$group) < nlevels(mydf$group)) {
    mydf$group <- droplevels(mydf$group)
  }

  # check if vector has any labels
  if (!is.null(grp.lbl) &&
      !is.null(names(grp.lbl)) &&
      insight::check_if_installed("sjlabelled", quietly = TRUE)) {
    # get unique levels, and match levels with group labels
    # might be necessary, if user only wants to calculate effects
    # for specific factor levels - unused labels must be removed then
    values <- as.numeric(as.vector(unique(stats::na.omit(mydf$group))))
    if (min(values) < 1) values <- round(.recode_to(values, lowest = 1))
    grp.lbl <- grp.lbl[values]
    mydf$group <- sjlabelled::set_labels(mydf$group, labels = grp.lbl)
    # make sure values of labels match actual values in vector
    if (!all(mydf$group %in% sjlabelled::get_values(mydf$group)))
      attr(mydf$group, "labels") <- NULL
  }

  if (.obj_has_name(mydf, "facet")) {
    facet.lbl <- .get_labels(
      original_model_frame[[terms[3]]],
      non.labelled = TRUE,
      values = "n",
      drop.unused = TRUE
    )

    # no new labels for labelled factors
    if (is.factor(mydf$facet) && !.is_numeric_factor(mydf$facet)) {
      facet.lbl <- NULL
    }

    # drop levels, if necessary
    if (is.factor(mydf$facet) && .n_distinct(mydf$facet) < nlevels(mydf$facet)) {
      mydf$facet <- droplevels(mydf$facet)
    }

    # check if vector has any labels
    if (!is.null(facet.lbl) &&
        !is.null(names(facet.lbl)) &&
        insight::check_if_installed("sjlabelled", quietly = TRUE)) {
      # get unique levels, and match levels with group labels
      # might be necessary, if user only wants to calculate effects
      # for specific factor levels - unused labels must be removed then
      values <- as.numeric(as.vector(unique(stats::na.omit(mydf$facet))))
      if (min(values) < 1) values <- .recode_to(values, lowest = 1)
      facet.lbl <- facet.lbl[values]
      mydf$facet <- sjlabelled::set_labels(mydf$facet, labels = facet.lbl)
      # make sure values of labels match actual values in vector
      if (!all(mydf$facet %in% sjlabelled::get_values(mydf$facet))) {
        attr(mydf$facet, "labels") <- NULL
      }
    }
  }

  mydf
}


# this method converts lavelled group variables
# into factors with labelled levels
.groupvariable_to_labelled_factor <- function(mydf) {
  mydf$group <- .as_label(
    mydf$group,
    prefix = FALSE,
    drop.na = TRUE,
    drop.levels = !is.numeric(mydf$group)
  )

  # make sure we have a facet-column at all
  if (.obj_has_name(mydf, "facet")) {
    # convert to factor
    mydf$facet <- .as_label(
      mydf$facet,
      prefix = TRUE,
      drop.na = TRUE,
      drop.levels = !is.numeric(mydf$facet)
    )
  }

  mydf
}


# get labels from labelled data for axis titles and labels
.get_axis_titles_and_labels <- function(model,
                                        original_model_frame,
                                        terms,
                                        fun,
                                        model_info,
                                        no.transform,
                                        type,
                                        at_list = NULL,
                                        averaged_predictions = FALSE) {
  # Retrieve response for automatic title
  resp.col <- insight::find_response(model)

  # check for family, and set appropriate scale-title
  # if we have transformation through effects-package,
  # check if data is on original or transformed scale
  ysc <- .get_title_labels(fun, model_info, no.transform, type)

  # fix title, depending on whether we called `ggaverage()` or not
  # for zero-inflated model, we emphasize whether we have the conditional
  # or the expected values
  if (averaged_predictions) {
    if (model_info$is_zero_inflated && type %in% c("zero_inflated", "response")) {
      avg_title <- "Average expected"
    } else {
      avg_title <- "Average predicted"
    }
  } else if (model_info$is_zero_inflated && type %in% c("zero_inflated", "simulate")) {
    avg_title <- "Expected"
  } else {
    avg_title <- "Predicted"
  }

  # set plot-title
  t.title <- paste(
    sprintf("%s %s of", avg_title, ysc),
    .get_label(original_model_frame[[1]], default = resp.col)
  )

  # axis titles
  x.title <- .get_label(original_model_frame[[terms[1]]], default = terms[1])
  y.title <- .get_label(original_model_frame[[1]], default = resp.col)


  if (fun == "coxph") {
    if (!is.null(type) && type == "survival") {
      t.title <- y.title <- "Probability of Survival"
    } else if (!is.null(type) && type == "cumulative_hazard") {
      t.title <- y.title <- "Cumulative Hazard"
    } else {
      t.title <- paste(avg_title, "risk scores")
      y.title <- "Risk Score"
    }
  }


  # legend title
  l.title <- .get_label(original_model_frame[[terms[2]]], default = terms[2])

  # check if we have a categorical variable with value labels at the x-axis.
  # If so extract labels. But be careful with character vectors. the correct
  # order of  values is saved in the "at_list" attribute, which we recover here.
  if (!is.null(at_list) && terms[1] %in% names(at_list)) {
    char_values <- .safe(at_list[[terms[1]]])
  }
  axis.labels <- .get_labels(
    original_model_frame[[terms[1]]],
    non.labelled = TRUE,
    drop.unused = TRUE,
    char_values = char_values
  )

  list(
    t.title = t.title,
    x.title = x.title,
    y.title = y.title,
    l.title = l.title,
    axis.labels = axis.labels
  )
}


.get_title_labels <- function(fun, model_info, no.transform, type) {
  ysc <- "values"

  if (is.null(model_info)) {
    return(ysc)
  }
  if (!is.null(type) && type %in% c("zi_prob", "zprob", "zero")) {
    ysc <- "zero-inflation probabilities"
  } else if (fun == "glm") {
    if (model_info$is_brms_trial) {
      ysc <- "successes"
    } else if (model_info$is_binomial || model_info$is_ordinal || model_info$is_multinomial) {
      ysc <- ifelse(isTRUE(no.transform), "log-odds", "probabilities")
    } else if (model_info$is_count) {
      if ((model_info$is_zero_inflated || model_info$is_hurdle) && !type %in% c("zero_inflated", "response", "simulate")) { # nolint
        ysc <- ifelse(isTRUE(no.transform), "(conditional) log-mean", "(conditional) counts")
      } else {
        ysc <- ifelse(isTRUE(no.transform), "log-mean", "counts")
      }
    } else if (model_info$is_beta || model_info$is_orderedbeta) {
      ysc <- "proportions"
    }
  } else if (model_info$is_beta || model_info$is_orderedbeta) {
    ysc <- "proportions"
  } else if (fun == "coxph") {
    if (!is.null(type) && type == "survival") {
      ysc <- "survival probabilities"
    } else if (!is.null(type) && type == "cumulative_hazard") {
      ysc <- "cumulative hazard"
    } else {
      ysc <- "risk scores"
    }
  }

  ysc
}


.recode_to <- function(x, lowest, highest = -1) {
  if (is.factor(x)) {
    x <- as.numeric(as.character(x))
  }

  minval <- min(x, na.rm = TRUE)
  downsize <- minval - lowest
  x <- x - downsize

  if (highest > lowest) x[x > highest] <- NA
  x
}
