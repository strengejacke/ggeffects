#' @title Create Johnson-Neyman confidence intervals and plots
#' @name johnson_neyman
#'
#' @description Function to create so-called Johnson-Neyman intervals. The
#' `plot()` method can be used to visualize the results of the Johnson-Neyman
#' test.
#'
#' @param x An object of class `ggeffects`, as returned by the functions
#' from this package.
#' @param colors Colors used for the plot. Must be a vector with two color
#' values.
#' @param ... Arguments passed down to `hypothesis_test()` (and then probably
#' further to [`marginaleffects::slopes()`]).
#'
#' @return A Johnson-Neyman plot.
#'
#' @details
#' The Johnson-Neyman intervals help to understand where slopes are significant
#' in the context of interactions in regression models. Thus, the interval is only
#' useful if the model contains at least one interaction term. The function
#' accepts the results of a call to `ggpredict()`, `ggeffect()` or `ggemmeans()`.
#' The _first_ and the _last_ focal term used in the `terms` argument of
#' `ggpredict()` etc. must be numeric. The function will then test the slopes of
#' the first focal terms against zero, for different moderator values of the
#' last focal term. Use `plot()` to create a plot of the results.
#'
#' @references
#' Bauer, D. J., & Curran, P. J. (2005). Probing interactions in fixed and
#' multilevel regression: Inferential and graphical techniques. Multivariate
#' Behavioral Research, 40(3), 373-400. doi: 10.1207/s15327906mbr4003_5
#'
#' Esarey, J., & Sumner, J. L. (2017). Marginal effects in interaction models:
#' Determining and controlling the false positive rate. Comparative Political
#' Studies, 1–33. Advance online publication. doi: 10.1177/0010414017730080
#'
#' Johnson, P.O. & Fay, L.C. (1950). The Johnson-Neyman technique, its theory
#' and application. Psychometrika, 15, 349-367. doi: 10.1007/BF02288864
#'
#' McCabe CJ, Kim DS, King KM. Improving Present Practices in the Visual Display
#' of Interactions. Advances in Methods and Practices in Psychological Science.
#' 2018;1(2):147-165. doi:10.1177/2515245917746792
#'
#' @examples
#' data(efc)
#' efc$c172code <- as.factor(efc$c172code)
#' m <- lm(neg_c_7 ~ c12hour * barthtot * c172code, data = efc)
#'
#' if (requireNamespace("ggplot2") && requireNamespace("marginaleffects")) {
#'   pr <- ggpredict(m, c("c12hour", "barthtot"))
#'   johnson_neyman(pr)
#'   plot(johnson_neyman(pr))
#'
#'   pr <- ggpredict(m, c("c12hour", "c172code", "barthtot"))
#'   johnson_neyman(pr)
#'   plot(johnson_neyman(pr))
#'
#'   # robust standard errors
#'   if (requireNamespace("sandwich")) {
#'     johnson_neyman(pr, vcov = sandwich::vcovHC)
#'     plot(johnson_neyman(pr))
#'   }
#' }
#' @export
johnson_neyman <- function(x, ...) {
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
  pr <- pretty(model_data[[focal_terms[length(focal_terms)]]], n = 200)

  # modify "terms" argument
  original_terms[length(original_terms)] <- paste0(focal_terms[length(focal_terms)], " [", toString(pr), "]")

  # calculate contrasts of slopes
  jn_slopes <- hypothesis_test(model, original_terms, test = NULL, ...)

  # we need a "Slope" column in jn_slopes
  if (!"Slope" %in% colnames(jn_slopes)) {
    insight::format_error("No slope information found.")
  }

  # remove first element from "focal_terms" and "numeric_focal"
  # the first element is "Slope"
  focal_terms <- focal_terms[-1]
  numeric_focal <- numeric_focal[-1]

  # if we still have two focal terms, check if all are numeric
  if (length(numeric_focal) == 2 && all(numeric_focal)) {
    # if so, convert first to factor
    jn_slopes[[focal_terms[1]]] <- as.factor(jn_slopes[[focal_terms[1]]])
  }
  # now add variable name to factor levels, as "heading" in facets
  if (is.factor(jn_slopes[[focal_terms[1]]])) {
    levels(jn_slopes[[focal_terms[1]]]) <- paste(focal_terms[1], "=", levels(jn_slopes[[focal_terms[1]]]))
  }

  # add a new column to jn_slopes, which indicates whether confidence intervals
  # cover zero
  jn_slopes$significant <- ifelse(jn_slopes$conf.low > 0 | jn_slopes$conf.high < 0, "yes", "no")

  # find x-position where significant changes to not-significant
  pos_lower <- pos_upper <- NA
  if (!all(jn_slopes$significant == "yes") && !all(jn_slopes$significant == "no")) {
    for (i in 1:(nrow(jn_slopes) - 1)) {
      if (jn_slopes$significant[i] != jn_slopes$significant[i + 1]) {
        if (is.na(pos_lower)) {
          pos_lower <- jn_slopes[[focal_terms[length(focal_terms)]]][i]
        } else if (is.na(pos_upper)) {
          pos_upper <- jn_slopes[[focal_terms[length(focal_terms)]]][i]
        } else {
          break
        }
      }
    }
  }

  # add additional information
  attr(jn_slopes, "focal_terms") <- focal_terms
  attr(jn_slopes, "lower_bound") <- pos_lower
  attr(jn_slopes, "upper_bound") <- pos_upper

  class(jn_slopes) <- c("ggjohnson_neyman", "data.frame")
  jn_slopes
}



# methods ---------------------------------------------------------------------


#' @export
print.ggjohnson_neyman <- function(x, ...) {
  # extract attributes
  focal_terms <- attributes(x)$focal_terms
  pos_lower <- attributes(x)$lower_bound
  pos_upper <- attributes(x)$upper_bound

  # check which values are significant for the slope
  if (is.na(pos_lower) && is.na(pos_upper)) {
    # is everything non-significant?
    msg <- sprintf(
      "There are no significant slopes of `%s` for any value of `%s`.",
      focal_terms[length(focal_terms)],
      colnames(x)[1]
    )
  } else if (is.na(pos_lower)) {
    # only one change from significant to non-significant
    msg <- sprintf(
      "For values of `%s` larger than %s, the slope of `%s` is p < 0.05.",
      focal_terms[length(focal_terms)],
      insight::format_value(pos_upper, protect_integers = TRUE),
      colnames(x)[1]
    )
  } else if (is.na(pos_upper)) {
    # only one change from significant to non-significant
    msg <- sprintf(
      "For values of `%s` lower than %s, the slope of `%s` is p < 0.05.",
      focal_terms[length(focal_terms)],
      insight::format_value(pos_lower, protect_integers = TRUE),
      colnames(x)[1]
    )
  } else {
    # J-N interval
    msg <- sprintf(
      "For values of `%s` that are outside the interval %s, the slope of `%s` is p < 0.05.",
      focal_terms[length(focal_terms)],
      insight::format_ci(pos_lower, pos_upper, ci = NULL),
      colnames(x)[1]
    )
  }

  cat(msg, "\n")
}


#' @rdname johnson_neyman
#' @export
plot.ggjohnson_neyman <- function(x, colors = c("#f44336", "#2196F3"), ...) {
  insight::check_if_installed("ggplot2")

  # extract attributes
  focal_terms <- attributes(x)$focal_terms
  pos_lower <- attributes(x)$lower_bound
  pos_upper <- attributes(x)$upper_bound

  # need a group for segments in geom_ribbon
  x$group <- gr <- 1
  if (!all(x$significant == "yes") && !all(x$significant == "no")) {
    for (i in 2:(nrow(x))) {
      if (x$significant[i] != x$significant[i - 1]) {
        gr <- gr + 1
      }
      x$group[i] <- gr
    }
  }

  # create plot
  p <- ggplot2::ggplot(
    data = x,
    ggplot2::aes(
      x = .data[[focal_terms[length(focal_terms)]]],
      y = .data$Slope,
      ymin = .data$conf.low,
      ymax = .data$conf.high,
      color = .data$significant,
      fill = .data$significant,
      group = .data$group
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
    ggplot2::geom_ribbon(alpha = 0.2, color = NA) +
    ggplot2::geom_line() +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_color_manual(values = colors) +
    theme_ggeffects() +
    ggplot2::labs(y = paste0("Slope of ", colnames(x)[1]))

  # add thresholds were significance changes to non-significance and vice versa
  if (!is.na(pos_lower)) {
    p <- p + ggplot2::geom_vline(xintercept = pos_lower, linetype = "dashed", alpha = 0.5, color = colors[2])
  }
  if (!is.na(pos_upper)) {
    p <- p + ggplot2::geom_vline(xintercept = pos_upper, linetype = "dashed", alpha = 0.5, color = colors[2])
  }

  # if we have more than two focal terms, we need to facet
  if (length(focal_terms) > 1) {
    p <- p + ggplot2::facet_wrap(focal_terms[1])
  }

  graphics::plot(p)
}
