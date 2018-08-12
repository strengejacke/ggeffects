# add labels to grouping and facet variables, if these
# variables come from labelled data
#' @importFrom dplyr n_distinct
#' @importFrom sjmisc recode_to is_num_fac
#' @importFrom sjlabelled get_labels set_labels
#' @importFrom tibble has_name
#' @importFrom stats na.omit
add_groupvar_labels <- function(mydf, ori.mf, terms) {
  grp.lbl <- sjlabelled::get_labels(
    ori.mf[[terms[2]]],
    non.labelled = TRUE,
    values = "n",
    drop.unused = TRUE
  )

  # no new labels for labelled factors
  if (is.factor(mydf$group) && !sjmisc::is_num_fac(mydf$group))
    grp.lbl <- NULL

  # drop levels, if necessary
  if (is.factor(mydf$group) && dplyr::n_distinct(mydf$group, na.rm = TRUE) < nlevels(mydf$group))
    mydf$group <- droplevels(mydf$group)

  # check if vector has any labels
  if (!is.null(grp.lbl) && !is.null(names(grp.lbl))) {
    # get unique levels, and match levels with group labels
    # might be necessary, if user only wants to calculate effects
    # for specific factor levels - unused labels must be removed then
    values <- as.numeric(as.vector(unique(stats::na.omit(mydf$group))))
    if (min(values) < 1) values <- sjmisc::recode_to(values, lowest = 1, append = FALSE)
    grp.lbl <- grp.lbl[values]
    mydf$group <- sjlabelled::set_labels(mydf$group, labels = grp.lbl)
  }

  if (tibble::has_name(mydf, "facet")) {
    facet.lbl <- sjlabelled::get_labels(
      ori.mf[[terms[3]]],
      non.labelled = TRUE,
      values = "n",
      drop.unused = TRUE
    )

    # no new labels for labelled factors
    if (is.factor(mydf$facet) && !sjmisc::is_num_fac(mydf$facet))
      facet.lbl <- NULL

    # drop levels, if necessary
    if (is.factor(mydf$facet) && dplyr::n_distinct(mydf$facet, na.rm = TRUE) < nlevels(mydf$facet))
      mydf$facet <- droplevels(mydf$facet)

    # check if vector has any labels
    if (!is.null(facet.lbl) && !is.null(names(facet.lbl))) {
      # get unique levels, and match levels with group labels
      # might be necessary, if user only wants to calculate effects
      # for specific factor levels - unused labels must be removed then
      values <- as.numeric(as.vector(unique(stats::na.omit(mydf$facet))))
      if (min(values) < 1) values <- sjmisc::recode_to(values, lowest = 1, append = FALSE)
      facet.lbl <- facet.lbl[values]
      mydf$facet <- sjlabelled::set_labels(mydf$facet, labels = facet.lbl)
    }
  }

  mydf
}


# this method converts lavelled group variables
# into factors with labelled levels
#' @importFrom sjlabelled as_label
#' @importFrom tibble has_name
groupvar_to_label <- function(mydf) {
  mydf$group <-
    sjlabelled::as_label(
      mydf$group,
      prefix = FALSE,
      drop.na = TRUE,
      drop.levels = !is.numeric(mydf$group)
    )

  # make sure we have a facet-column at all
  if (tibble::has_name(mydf, "facet")) {
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
get_all_labels <- function(fitfram, terms, fun, binom_fam, poisson_fam, no.transform) {
  # Retrieve response for automatic title
  resp.col <- colnames(fitfram)[1]

  # check for family, and set appropriate scale-title
  # if we have transformation through effects-package,
  # check if data is on original or transformed scale
  ysc <- get_title_labels(fun, binom_fam, poisson_fam, no.transform)

  # set plot-title
  t.title <-
    paste(sprintf("Predicted %s for", ysc),
          sjlabelled::get_label(fitfram[[1]], def.value = resp.col))


  # axis titles
  x.title <- sjlabelled::get_label(fitfram[[terms[1]]], def.value = terms[1])
  y.title <- sjlabelled::get_label(fitfram[[1]], def.value = resp.col)

  # legend title
  l.title <- sjlabelled::get_label(fitfram[[terms[2]]], def.value = terms[2])

  # check if we have a categorical variable with value
  # labels at the x-axis.
  axis.labels <- sjlabelled::get_labels(
    fitfram[[terms[1]]],
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


#' @importFrom dplyr if_else
get_title_labels <- function(fun, binom_fam, poisson_fam, no.transform) {
  ysc <- "values"

  if (fun == "glm") {
    if (binom_fam)
      ysc <-
        dplyr::if_else(
          isTRUE(no.transform),
          true = "log-odds",
          false = "probabilities",
          missing = "values"
        )
    else if (poisson_fam)
      ysc <-
        dplyr::if_else(
          isTRUE(no.transform),
          true = "log-mean",
          false = "incidents",
          missing = "values"
        )
  } else if (fun == "betareg") {
    ysc <- "proportion"
  } else if (fun == "coxph") {
    ysc <- "risk scores"
  }

  ysc
}
