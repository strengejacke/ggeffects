#' @title Create Johnson-Neyman plot
#' @name johnson_neyman
#'
#' @description Function to create so-called Johnson-Neyman plots, which are
#' used to visualize the results of a Johnson-Neyman test.
#'
#' @param x An object of class `ggeffects`, as returned by the functions
#' from this package.
#' @param ... Currently not used.
#' @export
johnson_neyman <- function(x, ...) {
  insight::check_if_installed("ggplot2")

  # we need the model data to check whether we have numeric focal terms
  model <- .safe(.get_model_object(x))
  model_data <- .safe(.get_model_data(model))
  if (is.null(model_data)) {
    insight::format_error("No model data found.")
  }

  # extract focal terms
  focal_terms <- attributes(x)$terms
  original_terms <- attributes(x)$original.terms

  # check whether we have numeric focal terms in our model data
  numeric_focal <- .safe(vapply(model_data[focal_terms], is.numeric, logical(1)))

  # if we don't have at least two numeric focal terms, we can't create a Johnson-Neyman plot
  if (sum(numeric_focal) < 2) {
    insight::format_error("At least two numeric focal terms are required.")
  }

  # first and last element of numeric_focal must be TRUE
  if (!numeric_focal[1] && !numeric_focal[length(numeric_focal)]) {
    insight::format_error("First and last focal term must be numeric.")
  }

  # now compute contrasts. we first need to make sure to have enough data points
  pr <- pretty_range(model_data[[focal_terms[length(focal_terms)]]], n = 200)

  # modify "terms" argument
  original_terms[length(original_terms)] <- paste0(focal_terms[length(focal_terms)], " [", toString(pr), "]")

  # calculate contrasts of slopes
  jn_contrasts <- hypothesis_test(model, original_terms, test = NULL)

  # we need a "Slope" column in jn_contrasts
  if (!"Slope" %in% colnames(jn_contrasts)) {
    insight::format_error("No slope information found.")
  }

  # remove first element from "focal_terms" and "numeric_focal"
  # the first element is "Slope"
  focal_terms <- focal_terms[-1]
  numeric_focal <- numeric_focal[-1]

  # if we still have two focal terms, check if all are numeric
  if (length(numeric_focal) == 2 && all(numeric_focal)) {
    # if so, convert first to factor
    jn_contrasts[[focal_terms[1]]] <- as.factor(jn_contrasts[[focal_terms[1]]])
    levels(jn_contrasts[[focal_terms[1]]]) <- paste(focal_terms[1], "=", levels(jn_contrasts[[focal_terms[1]]]))
  }

  # add a new column to jn_contrasts, which indicates whether confidence intervals
  # cover zero
  jn_contrasts$significant <- ifelse(jn_contrasts$conf.low > 0 | jn_contrasts$conf.high < 0, "yes", "no")

  # create plot
  p <- ggplot2::ggplot(
    data = jn_contrasts,
    ggplot2::aes(
      x = .data[[focal_terms[length(focal_terms)]]],
      y = .data$Slope,
      ymin = .data$conf.low,
      ymax = .data$conf.high,
      fill = .data$significant,
      color = .data$significant
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_ribbon(alpha = 0.2, color = NA) +
    ggplot2::geom_line() +
    see::scale_fill_material() +
    see::scale_color_material()

  # if we have more than two focal terms, we need to facet
  if (length(focal_terms) > 1) {
    p <- p + ggplot2::facet_wrap(focal_terms[1])
  }

  graphics::plot(p)
}
