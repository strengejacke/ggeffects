# add labels to grouping and facet variables, if these
# variables come from labelled data
#' @importFrom sjlabelled get_labels set_labels
#' @importFrom stats na.omit
.add_labels_to_groupvariable <- function(mydf, original_model_frame, terms) {
  grp.lbl <- sjlabelled::get_labels(
    original_model_frame[[terms[2]]],
    non.labelled = TRUE,
    values = "n",
    drop.unused = TRUE
  )

  # no new labels for labelled factors
  if (is.factor(mydf$group) && !.is_numeric_factor(mydf$group))
    grp.lbl <- NULL

  # drop levels, if necessary
  if (is.factor(mydf$group) && .n_distinct(mydf$group) < nlevels(mydf$group))
    mydf$group <- droplevels(mydf$group)

  # check if vector has any labels
  if (!is.null(grp.lbl) && !is.null(names(grp.lbl))) {
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
    facet.lbl <- sjlabelled::get_labels(
      original_model_frame[[terms[3]]],
      non.labelled = TRUE,
      values = "n",
      drop.unused = TRUE
    )

    # no new labels for labelled factors
    if (is.factor(mydf$facet) && !.is_numeric_factor(mydf$facet))
      facet.lbl <- NULL

    # drop levels, if necessary
    if (is.factor(mydf$facet) && .n_distinct(mydf$facet) < nlevels(mydf$facet))
      mydf$facet <- droplevels(mydf$facet)

    # check if vector has any labels
    if (!is.null(facet.lbl) && !is.null(names(facet.lbl))) {
      # get unique levels, and match levels with group labels
      # might be necessary, if user only wants to calculate effects
      # for specific factor levels - unused labels must be removed then
      values <- as.numeric(as.vector(unique(stats::na.omit(mydf$facet))))
      if (min(values) < 1) values <- .recode_to(values, lowest = 1)
      facet.lbl <- facet.lbl[values]
      mydf$facet <- sjlabelled::set_labels(mydf$facet, labels = facet.lbl)
      # make sure values of labels match actual values in vector
      if (!all(mydf$facet %in% sjlabelled::get_values(mydf$facet)))
        attr(mydf$facet, "labels") <- NULL
    }
  }

  mydf
}


# this method converts lavelled group variables
# into factors with labelled levels
#' @importFrom sjlabelled as_label
.groupvariable_to_labelled_factor <- function(mydf) {
  mydf$group <-
    sjlabelled::as_label(
      mydf$group,
      prefix = FALSE,
      drop.na = TRUE,
      drop.levels = !is.numeric(mydf$group)
    )

  # make sure we have a facet-column at all
  if (.obj_has_name(mydf, "facet")) {
    # convert to factor
    mydf$facet <-
      sjlabelled::as_label(
        mydf$facet,
        prefix = TRUE,
        drop.na = TRUE,
        drop.levels = !is.numeric(mydf$facet)
      )
  }

  mydf
}


# get labels from labelled data for axis titles and labels
#' @importFrom sjlabelled get_label
.get_axis_titles_and_labels <- function(original_model_frame, terms, fun, model_info, no.transform, type) {
  # Retrieve response for automatic title
  resp.col <- colnames(original_model_frame)[1]

  # check for family, and set appropriate scale-title
  # if we have transformation through effects-package,
  # check if data is on original or transformed scale
  ysc <- .get_title_labels(fun, model_info, no.transform, type)

  # set plot-title
  t.title <-
    paste(sprintf("Predicted %s of", ysc),
          sjlabelled::get_label(original_model_frame[[1]], def.value = resp.col))


  # axis titles
  x.title <- sjlabelled::get_label(original_model_frame[[terms[1]]], def.value = terms[1])
  y.title <- sjlabelled::get_label(original_model_frame[[1]], def.value = resp.col)


  if (fun == "coxph") {
    if (!is.null(type) && type == "surv") {
      t.title <- y.title <- "Probability of Survival"
    } else if (!is.null(type) && type == "cumhaz") {
      t.title <- y.title <- "Cumulative Hazard"
    } else {
      t.title <- "Predicted risk scores"
      y.title <- "Risk Score"
    }
  }


  # legend title
  l.title <- sjlabelled::get_label(original_model_frame[[terms[2]]], def.value = terms[2])

  # check if we have a categorical variable with value
  # labels at the x-axis.
  axis.labels <- sjlabelled::get_labels(
    original_model_frame[[terms[1]]],
    non.labelled = TRUE,
    drop.unused = TRUE
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

  if (!is.null(type) && type == "zi.prob") {
    ysc <- "zero-inflation probabilities"
  } else if (fun == "glm") {
    if (model_info$is_brms_trial)
      ysc <- "successes"
    else if (model_info$is_binomial || model_info$is_ordinal || model_info$is_multinomial)
      ysc <- ifelse(isTRUE(no.transform), "log-odds", "probabilities")
    else if (model_info$is_count)
      ysc <- ifelse(isTRUE(no.transform), "log-mean", "counts")
  } else if (model_info$is_beta) {
    ysc <- "proportion"
  } else if (fun == "coxph") {
    if (!is.null(type) && type == "surv")
      ysc <- "survival probabilities"
    else if (!is.null(type) && type == "cumhaz")
      ysc <- "cumulative hazard"
    else
      ysc <- "risk scores"
  }

  ysc
}





.recode_to <- function(x, lowest, highest = -1) {
  if (is.factor(x)) {
    x <- as.numeric(as.character(x))
  }

  minval <- min(x, na.rm = TRUE)
  downsize <- minval - lowest

  x <- sapply(x, function(y) y - downsize)

  if (highest > lowest) x[x > highest] <- NA
  x
}
