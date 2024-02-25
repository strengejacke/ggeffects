johnson_neyman_numcat <- function(x,
                                  model = NULL,
                                  model_data = NULL,
                                  focal_terms = NULL,
                                  original_terms = NULL,
                                  numeric_focal = NULL,
                                  dot_args = NULL,
                                  precision = 500) {
  # pkg installed?
  insight::check_if_installed("marginaleffects")

  # find out where we have *non* numerical focal terms
  categorical_focal <- .safe(vapply(model_data[focal_terms], !is.numeric, logical(1)))

  # now compute contrasts. we first need to make sure to have enough data points
  pr <- pretty(model_data[[focal_terms[numeric_focal]]], n = precision)

  jn_slopes <- do.call(rbind, lapply(sort(unique(pr)), function(i) {
    # modify "terms" argument. We want the categorical focal term,
    # and the numerical focal term at one(!) specific value
    new_terms <- c(
      focal_terms[categorical_focal[1]],
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
  groups <- split(jn_slopes, jn_slopes[[focal_terms[categorical_focal[1]]]])

  # find x-position where significant changes to not-significant
  interval_data <- .find_jn_intervals(groups)

  # add additional information
  attr(jn_slopes, "focal_terms") <- focal_terms
  attr(jn_slopes, "intervals") <- interval_data
  attr(jn_slopes, "p_adjust") <- p_adjust
  attr(jn_slopes, "response") <- insight::find_response(model)
  attr(jn_slopes, "rug_data") <- .safe(model_data[[focal_terms[numeric_focal]]])

  class(jn_slopes) <- c("ggjohnson_neyman_numcat", "data.frame")
  jn_slopes
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
