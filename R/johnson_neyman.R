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
  model_data <- .safe(.get_model_data(.get_model_object(x)))
  if (is.null(model_data)) {
    insight::format_error("No model data found.")
  }

  # extract focal terms
  focal_terms <- attributes(x)$terms

  # check whether we have numeric focal terms in our model data
  numeric_focal <- .safe(vapply(model_data[focal_terms], is.numeric, logical(1)))

  # if we don't have at least two numeric focal terms, we can't create a Johnson-Neyman plot
  if (sum(numeric_focal) < 2) {
    insight::format_error("At least two numeric focal terms are required.")
  }

  # now compute contrasts
  jn_contrasts <- hypothesis_test(x, test = NULL)

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

  # create plot
  p <- ggplot2::ggplot(
    data = jn_contrasts,
    ggplot2::aes(
      x = .data[[focal_terms[length(focal_terms)]]],
      y = .data$Slope,
      ymin = .data$conf.low,
      ymax = .data$conf.high
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_ribbon(alpha = 0.2, color = NA) +
    ggplot2::geom_line()

  # if we have more than two focal terms, we need to facet
  if (length(focal_terms) > 1) {
    p <- p + ggplot2::facet_wrap(focal_terms[1])
  }

  graphics::plot(p)
}
