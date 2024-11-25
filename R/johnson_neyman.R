#' @title Spotlight-analysis: Create Johnson-Neyman confidence intervals and plots
#' @name johnson_neyman
#'
#' @description Function conduct a spotlight-analysis to create so-called
#' Johnson-Neyman intervals. The `plot()` method can be used to visualize the
#' results of the Johnson-Neyman test.
#'
#' @param x An object of class `ggeffects`, as returned by the functions
#' from this package.
#' @param precision Number of values used for the range of the moderator variable
#' to calculate the Johnson-Neyman interval. This argument is passed down to
#' `pretty(..., n = precision)`. Usually, the default value of 500 is sufficient.
#' Increasing this value will result in a smoother plot and more accurate values
#' for the interval bounds, but can also slightly increase the computation time.
#' @param colors Colors used for the plot. Must be a vector with two color
#' values. Only used if `show_association = TRUE`.
#' @param show_association Logical, if `TRUE`, highlights the range where values
#' of the moderator are positively or negtatively associated with the outcome.
#' @param show_rug Logical, if `TRUE`, adds a rug with raw data of the moderator
#' variable to the plot. This helps visualizing its distribution.
#' @param verbose Show/hide printed message for plots.
#' @param ... Arguments passed down to [`test_predictions()`] (and then probably
#' further to [`marginaleffects::slopes()`]). See `?test_predictions` for further
#' details.
#'
#' @inheritParams test_predictions
#'
#' @return A data frame including contrasts of the [`test_predictions()`] for the
#' given interaction terms; for `plot()`, returns a Johnson-Neyman plot.
#'
#' @details
#' The Johnson-Neyman intervals help to understand where slopes are significant
#' in the context of interactions in regression models. Thus, the interval is only
#' useful if the model contains at least one interaction term. The function
#' accepts the results of a call to `predict_response()`. The _first_ and the
#' _last_ focal term used in the `terms` argument of `predict_response()` must
#' be numeric. The function will then test the slopes of the first focal terms
#' against zero, for different moderator values of the last focal term. If only
#' one numeric focal term is given, the function will create contrasts by levels
#' of the categorical focal term. Use `plot()` to create a plot of the results.
#'
#' To avoid misleading interpretations of the plot, we speak of "positive" and
#' "negative" associations, respectively, and "no clear" associations (instead
#' of "significant" or "non-significant"). This should prevent the user from
#' considering a non-significant range of values of the moderator as "accepting
#' the null hypothesis".
#'
#' @inheritSection test_predictions P-value adjustment for multiple comparisons
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
#' Spiller, S. A., Fitzsimons, G. J., Lynch, J. G., & McClelland, G. H. (2013).
#' Spotlights, Floodlights, and the Magic Number Zero: Simple Effects Tests in
#' Moderated Regression. Journal of Marketing Research, 50(2), 277–288.
#' doi:10.1509/jmr.12.0420
#'
#' @examplesIf requireNamespace("ggplot2") && requireNamespace("marginaleffects")
#' \dontrun{
#' data(efc, package = "ggeffects")
#' efc$c172code <- as.factor(efc$c172code)
#' m <- lm(neg_c_7 ~ c12hour * barthtot * c172code, data = efc)
#'
#' pr <- predict_response(m, c("c12hour", "barthtot"))
#' johnson_neyman(pr)
#' plot(johnson_neyman(pr))
#'
#' pr <- predict_response(m, c("c12hour", "c172code", "barthtot"))
#' johnson_neyman(pr)
#' plot(johnson_neyman(pr))
#'
#' # robust standard errors
#' if (requireNamespace("sandwich")) {
#'   johnson_neyman(pr, vcov = sandwich::vcovHC)
#' }
#' }
#' @export
johnson_neyman <- function(x, precision = 500, p_adjust = NULL, ...) {
  # we need the model data to check whether we have numeric focal terms
  model <- .safe(.get_model_object(x))
  model_data <- .safe(.get_model_data(model))
  if (is.null(model_data)) {
    insight::format_error("No model data found.")
  }

  # check arguments
  if (!is.null(p_adjust)) {
    p_adjust <- .validate_argument(p_adjust, c("esarey", "es", "fdr", "bh"))
    # just keep one shortcut
    p_adjust <- switch(p_adjust,
      esarey = "es",
      bh = "fdr",
      p_adjust
    )
  }

  # extract focal terms
  focal_terms <- attributes(x)$terms
  original_terms <- attributes(x)$original.terms

  dot_args <- list(...)
  # information about vcov-matrix
  vcov_matrix <- attributes(x)$vcov
  # set default for marginaleffects
  if (is.null(vcov_matrix)) {
    vcov_matrix <- TRUE
  }

  # make sure we have a valid vcov-argument when user supplies "standard" vcov-arguments
  # from ggpredict, like "vcov" etc. - then remove vcov_-arguments
  if (!is.null(dot_args$vcov)) {
    dot_args$vcov <- .get_variance_covariance_matrix(model, dot_args$vcov, dot_args$vcov_args)
    # remove non supported args
    dot_args$vcov_args <- NULL
  } else if (is.null(dot_args$vcov)) {
    dot_args$vcov <- vcov_matrix
  }

  # check whether we have numeric focal terms in our model data
  numeric_focal <- .safe(vapply(model_data[focal_terms], is.numeric, logical(1)))

  # if we don't have at least one numeric focal term, we can't create a Johnson-Neyman plot
  if (sum(numeric_focal) < 1) {
    insight::format_error("At least one numeric focal term is required.")
  }

  # if we have only one numeric focal term, we create contrasts
  # by levels of categorical focal term
  if (sum(numeric_focal) < 2) {
    return(johnson_neyman_numcat(
      x,
      model = model,
      model_data = model_data,
      focal_terms = focal_terms,
      original_terms = original_terms,
      numeric_focal = numeric_focal,
      dot_args = dot_args,
      p_adjust = p_adjust,
      precision = round(precision / 3)
    ))
  }

  # first and last element of numeric_focal must be TRUE
  if (!numeric_focal[1] && !numeric_focal[length(numeric_focal)]) {
    insight::format_error("First and last focal term must be numeric.")
  }

  # now compute contrasts. we first need to make sure to have enough data points
  pr <- pretty(model_data[[focal_terms[length(focal_terms)]]], n = precision)

  # modify "terms" argument
  original_terms[length(original_terms)] <- paste0(focal_terms[length(focal_terms)], " [", toString(pr), "]")

  # calculate contrasts of slopes
  fun_args <- list(model, terms = original_terms, test = NULL)
  if (identical(p_adjust, "fdr")) {
    fun_args$p_adjust <- "fdr"
  }
  jn_slopes <- do.call("test_predictions", c(fun_args, dot_args))

  # we need a "Slope" column in jn_slopes
  if (!"Slope" %in% colnames(jn_slopes)) {
    insight::format_error("No slope information found.")
  }

  # p-adjustment based on Esarey and Sumner?
  if (identical(p_adjust, "es")) {
    jn_slopes <- .fdr_interaction(jn_slopes, focal_terms, model)
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

  # p-adjustment based on fdr? we then need to update the significant-column here
  if (identical(p_adjust, "fdr")) {
    jn_slopes$significant[jn_slopes$p.value >= 0.05] <- "no"
  }

  # find groups, if we have three focal terms
  if (length(focal_terms) > 1) {
    groups <- split(jn_slopes, jn_slopes[[focal_terms[1]]])
  } else {
    jn_slopes$group <- "jn_no_group"
    groups <- list(jn_slopes)
    names(groups) <- "jn_no_group"
  }

  # find x-position where significant changes to not-significant
  interval_data <- .find_jn_intervals(groups, focal_term = focal_terms[length(focal_terms)])

  # add additional information
  attr(jn_slopes, "focal_terms") <- focal_terms
  attr(jn_slopes, "intervals") <- interval_data
  attr(jn_slopes, "p_adjust") <- p_adjust
  attr(jn_slopes, "response") <- insight::find_response(model)
  attr(jn_slopes, "rug_data") <- .safe(model_data[[focal_terms[length(focal_terms)]]])

  class(jn_slopes) <- c("ggjohnson_neyman", "data.frame")
  jn_slopes
}


#' @rdname johnson_neyman
#' @export
spotlight_analysis <- johnson_neyman



# methods ---------------------------------------------------------------------


#' @export
print.ggjohnson_neyman <- function(x, ...) {
  # extract attributes
  focal_terms <- attributes(x)$focal_terms
  intervals <- attributes(x)$intervals
  response <- attributes(x)$response
  p_adjust <- attributes(x)$p_adjust

  # iterate all intervals
  for (group in intervals$group) {
    # add "header" for groups
    if (group != "jn_no_group") {
      insight::print_color(sprintf("# Level `%s`\n", group), color = "blue")
    }

    # slice data, extract only for specific group
    d <- intervals[intervals$group == group, ]

    # get bound
    pos_lower <- d$pos_lower
    pos_upper <- d$pos_upper

    # get current focal term
    current_focal <- focal_terms[length(focal_terms)]

    # check which values are significant for the slope
    if (is.na(pos_lower) && is.na(pos_upper)) {
      # is everything non-significant?
      msg <- sprintf(
        "There are no clear negative or positive associations between `%s` and `%s` for any value of `%s`.",
        colnames(x)[1],
        response,
        current_focal
      )
    } else if (is.na(pos_upper)) {
      # only one change from significant to non-significant
      direction <- ifelse(d$significant == "yes", "lower", "higher")
      association <- ifelse(d$slope_lower > 0, "positive", "negative")
      msg <- sprintf(
        "The association between `%s` and `%s` is %s for values of `%s` %s than %s.",
        colnames(x)[1],
        response,
        association,
        current_focal,
        direction,
        insight::format_value(pos_lower, protect_integers = TRUE)
      )

      unclear_direction <- ifelse(d$significant != "yes", "lower", "higher")
      msg <- paste(msg, sprintf(
        "There were no clear associations for values of `%s` %s than %s.",
        current_focal,
        unclear_direction,
        insight::format_value(pos_lower, protect_integers = TRUE)
      ))
    } else {
      # J-N interval
      direction_lower <- ifelse(d$significant == "yes", "lower", "higher")
      direction_higher <- ifelse(d$significant != "yes", "lower", "higher")
      association_lower <- ifelse(d$slope_lower > 0, "positive", "negative")
      association_higher <- ifelse(d$slope_upper > 0, "positive", "negative")

      # check whether significant range is inside or outside of that interval
      if (direction_lower == "higher") {
        # positive or negative associations *inside* of an interval
        msg <- sprintf(
          "The association between `%s` and `%s` is %s for values of `%s` that range from %s to %s.",
          colnames(x)[1],
          response,
          association_lower,
          current_focal,
          insight::format_value(pos_lower, protect_integers = TRUE),
          insight::format_value(pos_upper, protect_integers = TRUE)
        )
        msg <- paste(msg, "Outside of this interval, there were no clear associations.")
      } else {
        # positive or negative associations *outside* of an interval
        msg <- sprintf(
          "The association between `%s` and `%s` is %s for values of `%s` %s than %s and %s for values %s than %s.",
          colnames(x)[1],
          response,
          association_lower,
          current_focal,
          direction_lower,
          insight::format_value(pos_lower, protect_integers = TRUE),
          association_higher,
          direction_higher,
          insight::format_value(pos_upper, protect_integers = TRUE)
        )
        msg <- paste(msg, sprintf(
          "Inside the interval of %s, there were no clear associations.",
          insight::format_ci(pos_lower, pos_upper, ci = NULL)
        ))
      }
    }

    cat(insight::format_message(msg), "\n", sep = "")
    if (group != "jn_no_group" && group != intervals$group[length(intervals$group)]) {
      cat("\n")
    }
  }

  if (!is.null(p_adjust)) {
    cat("\n", .format_p_adjust(p_adjust), "\n", sep = "")
  }
}


#' @rdname johnson_neyman
#' @export
plot.ggjohnson_neyman <- function(x,
                                  colors = c("#f44336", "#2196F3"),
                                  show_association = TRUE,
                                  show_rug = FALSE,
                                  verbose = TRUE, ...) {
  # print results, to make it obvious that we talk about associations, not significanse
  if (verbose) {
    print(x)
  }
  insight::check_if_installed("ggplot2")
  .data <- NULL # avoid global variable warning

  # extract attributes
  focal_terms <- attributes(x)$focal_terms
  intervals <- attributes(x)$intervals
  response <- attributes(x)$response
  rug_data <- attributes(x)$rug_data

  # rename to association
  x$Association <- x$significant
  x$Association[x$Association == "yes"] <- "positive/negative"
  x$Association[x$Association == "no"] <- "inconsistent"

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

  # create data frame for rug data
  if (!is.null(rug_data)) {
    rug_data <- data.frame(x = rug_data, group = 1)
    if (!all(is.na(intervals$pos_lower)) && !all(is.na(intervals$pos_lower))) {
      rug_data$group[rug_data$x >= intervals$pos_lower & rug_data$x <= intervals$pos_upper] <- 2
      rug_data$group[rug_data$x > intervals$pos_upper] <- 3
    } else if (!all(is.na(intervals$pos_lower))) {
      rug_data$group[rug_data$x > intervals$pos_lower] <- 2
    }
  }

  # create plot
  if (show_association) {
    p <- ggplot2::ggplot(
      data = x,
      ggplot2::aes(
        x = .data[[focal_terms[length(focal_terms)]]],
        y = .data$Slope,
        ymin = .data$conf.low,
        ymax = .data$conf.high,
        color = .data$Association,
        fill = .data$Association,
        group = .data$group
      )
    ) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::scale_color_manual(values = colors)
  } else {
    colors <- c("black", "black")
    p <- ggplot2::ggplot(
      data = x,
      ggplot2::aes(
        x = .data[[focal_terms[length(focal_terms)]]],
        y = .data$Slope,
        ymin = .data$conf.low,
        ymax = .data$conf.high
      )
    )
  }

  # add remaining geoms
  p <- p +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
    ggplot2::geom_ribbon(alpha = 0.2, color = NA) +
    ggplot2::geom_line() +
    theme_ggeffects() +
    ggplot2::labs(
      y = paste0("Slope of ", colnames(x)[1]),
      title = paste0("Association between ", colnames(x)[1], " and ", response)
    )

  # to make facets work
  names(intervals)[names(intervals) == "group"] <- focal_terms[1]

  p <- p +
    ggplot2::geom_vline(
      data = intervals,
      ggplot2::aes(xintercept = .data$pos_lower),
      linetype = "dashed",
      alpha = 0.6,
      color = colors[2]
    ) +
    ggplot2::geom_vline(
      data = intervals,
      ggplot2::aes(xintercept = .data$pos_upper),
      linetype = "dashed",
      alpha = 0.6,
      color = colors[2]
    )

  # if we have more than two focal terms, we need to facet
  if (length(focal_terms) > 1) {
    p <- p + ggplot2::facet_wrap(focal_terms[1])
  }

  # add rug data?
  if (!is.null(rug_data) && show_rug) {
    p <- p + ggplot2::geom_rug(
      data = rug_data,
      ggplot2::aes(x = .data$x, group = .data$group),
      sides = "b",
      length = ggplot2::unit(0.02, "npc"),
      inherit.aes = FALSE
    )
  }

  suppressWarnings(graphics::plot(p))
}


# helper ----------------------------------------------------------------------


.find_jn_intervals <- function(groups, focal_term, comparison = "Slope") {
  # find x-position where significant changes to not-significant
  do.call(rbind, lapply(names(groups), function(g) {
    pos_lower <- pos_upper <- NA_real_
    slope_lower <- slope_upper <- NA_real_
    significant <- NA_character_
    gr_data <- groups[[g]]
    if (!all(gr_data$significant == "yes") && !all(gr_data$significant == "no")) {
      for (i in 1:(nrow(gr_data) - 1)) {
        if (gr_data$significant[i] != gr_data$significant[i + 1]) {
          if (is.na(pos_lower)) {
            pos_lower <- gr_data[[focal_term]][i]
            slope_lower <- gr_data[[comparison]][i]
            if (is.na(significant)) {
              significant <- gr_data$significant[i]
            }
          } else if (is.na(pos_upper)) {
            pos_upper <- gr_data[[focal_term]][i]
            slope_upper <- gr_data[[comparison]][i]
          } else {
            break
          }
        }
      }
    }
    data.frame(
      pos_lower = pos_lower,
      pos_upper = pos_upper,
      slope_lower = slope_lower,
      slope_upper = slope_upper,
      group = g,
      significant = significant,
      stringsAsFactors = FALSE
    )
  }))
}


.format_p_adjust <- function(method) {
  method <- tolower(method)
  method <- switch(method,
    holm = "Holm (1979)",
    hochberg = "Hochberg (1988)",
    hommel = "Hommel (1988)",
    bonferroni = "Bonferroni",
    fdr = ,
    bh = "Benjamini & Hochberg (1995)",
    by = "Benjamini & Yekutieli (2001)",
    tukey = "Tukey",
    scheffe = "Scheffe",
    sidak = "Sidak",
    es = ,
    esarey = "Esarey & Sumner (2017)",
    method
  )
  insight::format_message(sprintf("P-values were adjusted using the %s method.", method))
}


.fdr_interaction <- function(x, focal_terms, model) {
  # get names of interaction terms
  pred <- focal_terms[1]
  mod <- focal_terms[length(focal_terms)]
  int <- paste0(pred, ":", mod)

  # variance-covariance matrix, to adjust p-values
  varcov <- insight::get_varcov(model)
  # Predictor variances
  vcov_pred <- varcov[pred, pred]
  vcov_int <- varcov[int, int]
  vcov_pred_int <- varcov[pred, int]

  # Generate sequence of numbers along range of moderator
  range_sequence <- seq(
    from = min(x[[mod]], na.rm = TRUE),
    to = max(x[[mod]], na.rm = TRUE),
    by = diff(range(x[[mod]], na.rm = TRUE)) / 1000
  )

  # get parameters, to manually calculate marginal effects
  params <- insight::get_parameters(model)
  beta_pred <- params$Estimate[params$Parameter == pred]
  beta_int <- params$Estimate[params$Parameter == int]

  # produces a sequence of marginal effects
  marginal_effects <- beta_pred + beta_int * range_sequence
  # SEs of those marginal effects
  me_ses <- sqrt(vcov_pred + (range_sequence^2) * vcov_int + 2 * range_sequence * vcov_pred_int)

  # t-values across range of marginal effects
  statistic <- marginal_effects / me_ses
  # degrees of freedom
  dof <- attributes(x)$df
  # Get the minimum p values used in the adjustment
  pvalues <- 2 * pmin(stats::pt(statistic, df = dof), (1 - stats::pt(statistic, df = dof)))
  # Multipliers
  multipliers <- seq_along(marginal_effects) / length(marginal_effects)
  # Order the pvals
  ordered_pvalues <- order(pvalues)

  # Adapted from interactionTest package function fdrInteraction
  test <- 0
  i <- 1 + length(marginal_effects)
  alpha <- (1 - attributes(x)$ci_level) / 2

  while (test == 0 && i > 1) {
    i <- i - 1
    test <- min(pvalues[ordered_pvalues][1:i] <= multipliers[i] * (alpha * 2))
  }

  # updates test statistic
  tcrit <- abs(stats::qt(multipliers[i] * alpha, dof))
  # update confidence intervals
  standard_errors <- attributes(x)$standard_error
  x$conf.low <- x$Slope - tcrit * standard_errors
  x$conf.high <- x$Slope + tcrit * standard_errors

  # update p-values - we need to ensure that length of "statisic" matches number
  # of rows, so we pick just as many values from "statistic" as required
  range_mod <- .split_vector(range_sequence, nrow(x))
  if (length(range_mod) == length(statistic)) {
    statistic <- statistic[range_mod]
    # update p-values
    x$p.value <- 2 * stats::pt(abs(statistic), df = dof, lower.tail = FALSE)
  }
  x
}


.split_vector <- function(x, by) {
  by <- length(x) / by
  r <- diff(range(x))
  out <- seq(0, abs(r - by - 1), by = by)
  out <- c(round(min(x) + c(0, out - 0.51 + (max(x) - max(out)) / 2), 0), max(x))
  if (length(out) > by) {
    out <- out[-sample(seq_along(out), 1)]
  }
  out
}
