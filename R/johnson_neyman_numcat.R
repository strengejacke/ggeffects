johnson_neyman_numcat <- function(x,
                                  model = NULL,
                                  model_data = NULL,
                                  focal_terms = NULL,
                                  original_terms = NULL,
                                  numeric_focal = NULL,
                                  dot_args = NULL,
                                  p_adjust = NULL,
                                  precision = 500) {
  # pkg installed?
  insight::check_if_installed("marginaleffects")

  # find out where we have *non* numerical focal terms
  categorical_focal <- .safe(vapply(model_data[focal_terms], is.factor, logical(1)))

  # now compute contrasts. we first need to make sure to have enough data points
  pr <- pretty(model_data[[focal_terms[numeric_focal]]], n = precision)

  jn_slopes <- do.call(rbind, lapply(sort(unique(pr)), function(i) {
    # modify "terms" argument. We want the categorical focal term,
    # and the numerical focal term at one(!) specific value
    new_terms <- c(
      focal_terms[categorical_focal][1],
      paste0(focal_terms[numeric_focal], " [", toString(i), "]")
    )
    # directly calling marginaleffects might be faster, but we need to
    # make sure that columns / values are properly formatted
    # fun_args <- list(model, newdata = new_data(m, new_terms))
    # do.call(
    #   marginaleffects::predictions,
    #   c(fun_args, dot_args)
    # )
    # calculate contrasts of slopes
    fun_args <- list(model, terms = new_terms)
    if (identical(p_adjust, "fdr")) {
      fun_args$p_adjust <- "fdr"
    }
    do.call("test_predictions", c(fun_args, dot_args))
  }))

  # we have now contrasts for each value of the numerical focal term
  # make the values of the numeric focal term the x-axis, convert to numeric
  jn_slopes[[focal_terms[numeric_focal]]] <- as.numeric(gsub("(\\d.*)-(\\d.*)", "\\1", jn_slopes[[focal_terms[numeric_focal]]])) # nolint

  # add a new column to jn_slopes, which indicates whether confidence intervals
  # cover zero
  jn_slopes$significant <- ifelse(jn_slopes$conf.low > 0 | jn_slopes$conf.high < 0, "yes", "no")

  # p-adjustment based on fdr? we then need to update the significant-column here
  if (identical(p_adjust, "fdr")) {
    jn_slopes$significant[jn_slopes$p.value >= 0.05] <- "no"
  }

  # split data at each level of categorical term, so we can find the interval
  # boundaries for each group
  groups <- split(jn_slopes, jn_slopes[[focal_terms[categorical_focal][1]]])

  # find x-position where significant changes to not-significant
  interval_data <- .find_jn_intervals(
    groups,
    focal_term = focal_terms[numeric_focal],
    comparison = "Contrast"
  )

  # add additional information
  attr(jn_slopes, "focal_terms") <- focal_terms
  attr(jn_slopes, "focal_cat") <- focal_terms[categorical_focal][1]
  attr(jn_slopes, "focal_num") <- focal_terms[numeric_focal]
  attr(jn_slopes, "intervals") <- interval_data
  attr(jn_slopes, "p_adjust") <- p_adjust
  attr(jn_slopes, "response") <- insight::find_response(model)
  attr(jn_slopes, "rug_data") <- .safe(model_data[[focal_terms[numeric_focal]]])

  class(jn_slopes) <- c("ggjohnson_neyman_numcat", "data.frame")
  jn_slopes
}


# methods --------------


#' @export
print.ggjohnson_neyman_numcat <- function(x, ...) {
  # extract attributes
  focal_terms <- attributes(x)$focal_terms
  intervals <- attributes(x)$intervals
  response <- attributes(x)$response
  p_adjust <- attributes(x)$p_adjust
  numeric_focal <- attributes(x)$focal_num
  categorical_focal <- attributes(x)$focal_cat

  # iterate all intervals
  for (group in intervals$group) {
    # add "header" for groups
    if (group != "jn_no_group") {
      insight::print_color(sprintf(
        "# Difference between levels `%s` of %s\n",
        group,
        categorical_focal
      ), color = "blue")
    }

    # slice data, extract only for specific group
    d <- intervals[intervals$group == group, ]

    # get bound
    pos_lower <- d$pos_lower
    pos_upper <- d$pos_upper

    # get current focal term
    current_focal <- numeric_focal

    # check which values are significant for the slope
    if (is.na(pos_lower) && is.na(pos_upper)) {
      # is everything non-significant?
      msg <- sprintf(
        "There are no statistically significant differencens between the levels `%s` of `%s` for any value of `%s`.",
        group,
        categorical_focal,
        current_focal
      )
    } else if (is.na(pos_upper)) {
      # only one change from significant to non-significant
      direction <- ifelse(d$significant == "yes", "lower", "higher")
      msg <- sprintf(
        "The difference between levels `%s` of `%s` is statistically significant for values of `%s` %s than %s.",
        group,
        categorical_focal,
        current_focal,
        direction,
        insight::format_value(pos_lower, protect_integers = TRUE)
      )

      unclear_direction <- ifelse(d$significant != "yes", "lower", "higher")
      msg <- paste(msg, sprintf(
        "There were no statistically significant differencens for values of `%s` %s than %s.",
        current_focal,
        unclear_direction,
        insight::format_value(pos_lower, protect_integers = TRUE)
      ))
    } else {
      # J-N interval
      direction_lower <- ifelse(d$significant == "yes", "lower", "higher")
      direction_higher <- ifelse(d$significant != "yes", "lower", "higher")

      # check whether significant range is inside or outside of that interval
      if (direction_lower == "higher") {
        # positive or negative associations *inside* of an interval
        msg <- sprintf(
          "The difference between levels `%s` of `%s` is statistically significant for values of `%s` that range from %s to %s.", # nolint
          group,
          categorical_focal,
          current_focal,
          insight::format_value(pos_lower, protect_integers = TRUE),
          insight::format_value(pos_upper, protect_integers = TRUE)
        )
        msg <- paste(msg, "Outside of this interval, there were no statistically significant differences.")
      } else {
        # positive or negative associations *outside* of an interval
        msg <- sprintf(
          "The difference between levels `%s` of `%s` is statistically significant for values of `%s` %s than %s and %s than %s.", # nolint
          group,
          categorical_focal,
          current_focal,
          direction_lower,
          insight::format_value(pos_lower, protect_integers = TRUE),
          direction_higher,
          insight::format_value(pos_upper, protect_integers = TRUE)
        )
        msg <- paste(msg, sprintf(
          "Inside the interval of %s, there were no statistically significant differences.",
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


#' @export
plot.ggjohnson_neyman_numcat <- function(x,
                                         colors = c("#f44336", "#2196F3"),
                                         show_association = TRUE,
                                         show_rug = FALSE,
                                         verbose = TRUE, ...) {
  # print results, to make it obvious that we talk about associations, not significanse
  if (verbose) {
    print(x)
  }
  insight::check_if_installed(c("ggplot2", "see"))
  .data <- NULL # avoid global variable warning

  # extract attributes
  focal_terms <- attributes(x)$focal_terms
  focal_cat <- attributes(x)$focal_cat
  focal_num <- attributes(x)$focal_num
  intervals <- attributes(x)$intervals
  response <- attributes(x)$response
  rug_data <- attributes(x)$rug_data

  # rename to difference
  x$Difference <- x$significant
  x$Difference[x$Difference == "yes"] <- "significant"
  x$Difference[x$Difference == "no"] <- "not significant"

  split_data <- split(x, x[[focal_cat]])
  plots <- lapply(seq_along(split_data), function(split_index) {
    jndata <- split_data[[split_index]]
    # need a group for segments in geom_ribbon
    jndata$group <- gr <- 1
    if (!all(jndata$significant == "yes") && !all(jndata$significant == "no")) {
      for (i in 2:(nrow(jndata))) {
        if (jndata$significant[i] != jndata$significant[i - 1]) {
          gr <- gr + 1
        }
        jndata$group[i] <- gr
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
        data = jndata,
        ggplot2::aes(
          x = .data[[focal_num]],
          y = .data$Contrast,
          ymin = .data$conf.low,
          ymax = .data$conf.high,
          color = .data$Difference,
          fill = .data$Difference,
          group = .data$group
        )
      ) +
        ggplot2::scale_fill_manual(values = colors) +
        ggplot2::scale_color_manual(values = colors)
    } else {
      colors <- c("black", "black")
      p <- ggplot2::ggplot(
        data = jndata,
        ggplot2::aes(
          x = .data[[focal_num]],
          y = .data$Contrast,
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
      theme_ggeffects()

    # first plot gets titles
    if (split_index == 1) {
      p <- p + ggplot2::labs(
        y = paste("Difference between", colnames(jndata)[1], jndata[[1]][1]),
        title = paste0("Comparison of ", colnames(jndata)[1], "-levels across ", focal_num)
      )
    } else {
      p <- p + ggplot2::labs(
        y = paste("Difference between", colnames(jndata)[1], jndata[[1]][1])
      )
    }

    # split interval data
    interval_data <- intervals[intervals$group == jndata[[1]][1], ]

    suppressWarnings({
      p <- p +
        ggplot2::geom_vline(
          data = interval_data,
          ggplot2::aes(xintercept = .data$pos_lower),
          linetype = "dashed",
          alpha = 0.6,
          color = colors[2]
        ) +
        ggplot2::geom_vline(
          data = interval_data,
          ggplot2::aes(xintercept = .data$pos_upper),
          linetype = "dashed",
          alpha = 0.6,
          color = colors[2]
        )
    })

    # add rug data?
    if (!is.null(rug_data) && show_rug) {
      p <- p + ggplot2::geom_rug(
        data = rug_data,
        ggplot2::aes(x = .data$jndata, group = .data$group),
        sides = "b",
        length = ggplot2::unit(0.02, "npc"),
        inherit.aes = FALSE
      )
    }

    # no legends except for last plot
    if (split_index < length(split_data)) {
      p <- p + ggplot2::theme(legend.position = "none")
    } else {
      p <- p + ggplot2::theme(legend.position = "bottom")
    }

    # return final plot
    suppressWarnings(p)
  })

  suppressWarnings(see::plots(plots))
}


# library(ggeffects)
#   library(ggplot2)
#   ggplot(out, aes(x = x, y = Contrast, ymin = conf.low, ymax = conf.high, colour = color, fill = color)) +
#     geom_ribbon(alpha = 0.2, colour = NA) +
#     geom_line() +
#     facet_wrap(~c172code)

# data(efc, package = "ggeffects")
# efc$c172code <- as.factor(efc$c172code)
# m <- lm(neg_c_7 ~ c12hour * c172code, data = efc)
# ggpredict(m, terms = c("c12hour", "c172code")) |> plot()
# test_predictions(m, terms = c("c172code", "c12hour"))
# test_predictions(m, terms = c("c12hour [105]", "c172code")) |> as.data.frame()

# head(unique(sort(efc$c12hour)))
# library(marginaleffects)
# nd <- data_grid(m, c("c172code"))
# marginaleffects::predictions(m, newdata = nd, variables = "c12hour", hypothesis = "pairwise")


# test_predictions(m, terms = c("c172code", "c12hour [5]"))


#         model,
#         by = by_arg,
#         newdata = datagrid,
#         hypothesis = test,
#         df = df,
#         conf_level = ci_level
#       )


# m2 <- lm(neg_c_7 ~ c12hour * barthtot, data = efc)
# pr <- predict_response(m2, c("c12hour", "barthtot"))
# out2 <- johnson_neyman(pr)
# head(as.data.frame(out2))
# head(as.data.frame(out))
