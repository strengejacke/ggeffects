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
#' @examplesIf all(insight::check_if_installed(c("ggplot2", "marginaleffects", "modelbased"), quietly = TRUE))
#' \dontrun{
#' data(efc, package = "ggeffects")
#' m <- lm(neg_c_7 ~ c12hour * barthtot, data = efc)
#'
#' pr <- predict_response(m, c("c12hour", "barthtot"))
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
    p_adjust <- insight::validate_argument(p_adjust, c("esarey", "es", "fdr", "bh"))
    # just keep one shortcut
    p_adjust <- switch(p_adjust,
      esarey = "es",
      bh = "fdr",
      p_adjust
    )
  }

  # extract focal terms
  focal_terms <- attributes(x)$terms

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

  # calculate contrasts of slopes
  fun_args <- list(
    model,
    trend = focal_terms[1],
    by = focal_terms[2:length(focal_terms)],
    length = precision
  )
  # if (identical(p_adjust, "fdr")) {
  #   fun_args$p_adjust <- "fdr"
  # }
  out <- do.call(modelbased::estimate_slopes, c(fun_args, dot_args))
  out <- .summarize_slopes(out)
  class(out) <- c("ggjohnson_neyman", "data.frame")

  out
}


#' @rdname johnson_neyman
#' @export
spotlight_analysis <- johnson_neyman


# helper ----------------------------------------------------------------------


#' @export
print.ggjohnson_neyman <- function(x, ...) {
  # extract attributes
  model_data <- insight::get_data(attributes(x)$model, source = "mf", verbose = FALSE)
  trend <- attributes(x)$trend
  current_focal <- attributes(x)$by[1]
  response <- attributes(x)$response
  group_variable <- attributes(x)$group
  p_adjust <- attributes(x)$p_adjust

  if ("Group" %in% colnames(x)) {
    groups <- split(x, x$Group)
  } else {
    groups <- list(x)
  }

  for (gr in groups) {
    # add "header" for groups
    if ("Group" %in% colnames(gr)) {
      insight::print_color(sprintf("# Level `%s`\n", gr$Group[1]), color = "blue")
    }

    # find significant rows
    sig_rows <- which(gr$Confidence == "Significant")

    # get bound
    pos_lower <- gr$Start[sig_rows]
    pos_upper <- gr$End[sig_rows]

    # check which values are significant for the slope
    if (!length(sig_rows)) {
      # is everything non-significant?
      msg <- sprintf(
        "There are no clear negative or positive associations between `%s` and `%s` for any value of `%s`.",
        trend,
        response,
        current_focal
      )
    } else if (all(sig_rows == 1) || all(sig_rows == nrow(gr))) {
      # only one change from significant to non-significant
      direction <- ifelse(all(sig_rows == nrow(gr)), "higher", "lower")
      cut_off <- ifelse(all(sig_rows == nrow(gr)), pos_lower, pos_upper)

      msg <- sprintf(
        "The association between `%s` and `%s` is %s for values of `%s` %s than %s.",
        trend,
        response,
        gr$Direction[sig_rows],
        current_focal,
        direction,
        insight::format_value(cut_off, protect_integers = TRUE)
      )

      unclear_direction <- ifelse(all(sig_rows == nrow(gr)), "lower", "higher")
      cut_off <- ifelse(all(sig_rows == nrow(gr)), pos_lower, pos_upper)

      msg <- paste(msg, sprintf(
        "There were no clear associations for values of `%s` %s than %s.",
        current_focal,
        unclear_direction,
        insight::format_value(cut_off, protect_integers = TRUE)
      ))
    } else if (length(sig_rows) == 1) {
      # positive or negative associations *inside* of an interval
      msg <- sprintf(
        "The association between `%s` and `%s` is %s for values of `%s` that range from %s to %s.",
        trend,
        response,
        gr$Direction[sig_rows],
        current_focal,
        insight::format_value(pos_lower, protect_integers = TRUE),
        insight::format_value(pos_upper, protect_integers = TRUE)
      )
      msg <- paste(msg, "Outside of this interval, there were no clear associations.")
    } else {
      # positive or negative associations *outside* of an interval
      msg <- sprintf(
        "The association between `%s` and `%s` is %s for values of `%s` lower than %s and %s for values higher than %s.",
        colnames(x)[1],
        response,
        gr$Direction[sig_rows[1]],
        current_focal,
        insight::format_value(pos_upper[1], protect_integers = TRUE),
        gr$Direction[sig_rows[2]],
        insight::format_value(pos_lower[2], protect_integers = TRUE)
      )
      msg <- paste(msg, sprintf(
        "Inside the interval of %s, there were no clear associations.",
        insight::format_ci(pos_upper[1], pos_lower[2], ci = NULL)
      ))
    }

    cat(insight::format_message(msg, ...), "\n\n", sep = "")
  }

  if (!is.null(p_adjust)) {
    cat("\n", .format_p_adjust(p_adjust), "\n", sep = "")
  }
}


.summarize_slopes <- function(object, verbose = TRUE, ...) {
  out <- as.data.frame(object)
  by <- attributes(object)$by

  if (verbose && nrow(out) < 50) {
    insight::format_alert("There might be too few data to accurately determine intervals. Consider setting `length = 100` (or larger) in your call to `estimate_slopes()`.") # nolint
  }

  # Add "Confidence" col based on the sig index present in the data
  out$Confidence <- .estimate_slopes_significance(out, ...)
  out$Direction <- .estimate_slopes_direction(out, ...)

  # if we have more than one variable in `by`, group result table and
  # add group name as separate column
  if (length(by) > 1) {
    parts <- split(out, out[[by[2]]])
    out <- do.call(rbind, lapply(parts, .estimate_slope_parts, by = by[1]))
    out <- datawizard::rownames_as_column(out, "Group")
    out$Group <- gsub("\\.\\d+$", "", out$Group)
    attr(out, "group") <- by[2]
  } else {
    out <- .estimate_slope_parts(out, by)
  }

  attributes(out) <- utils::modifyList(attributes(object), attributes(out))
  out
}


.estimate_slope_parts <- function(out, by) {
  # mark all "changes" from negative to positive and vice versa
  index <- 1
  out$switch <- index
  index <- index + 1

  for (i in 2:nrow(out)) {
    if (out$Direction[i] != out$Direction[i - 1] || out$Confidence[i] != out$Confidence[i - 1]) {
      out$switch[i:nrow(out)] <- index
      index <- index + 1
    }
  }

  # split into "switches"
  parts <- split(out, out$switch)

  do.call(rbind, lapply(parts, function(i) {
    data.frame(
      Start = i[[by]][1],
      End  = i[[by]][nrow(i)],
      Direction = i$Direction[1],
      Confidence = i$Confidence[1]
    )
  }))
}


.estimate_slopes_direction <- function(data, ...) {
  centrality_columns <- datawizard::extract_column_names(
    data,
    c("Coefficient", "Slope", "Median", "Mean", "MAP_Estimate"),
    verbose = FALSE
  )
  ifelse(data[[centrality_columns]] < 0, "negative", "positive")
}


.estimate_slopes_significance <- function(x, confidence = "auto", ...) {
  insight::check_if_installed("effectsize")

  if (confidence == "auto") {
    # TODO: make sure all of these work
    if ("BF" %in% names(x)) confidence <- "BF"
    if ("p" %in% names(x)) confidence <- "p"
    if ("pd" %in% names(x)) confidence <- "pd"
  }

  switch(confidence,
    p = tools::toTitleCase(effectsize::interpret_p(x$p, ...)),
    BF = tools::toTitleCase(effectsize::interpret_bf(x$BF, ...)),
    pd = tools::toTitleCase(effectsize::interpret_pd(x$pd, ...)),
    {
      # Based on CI
      out <- ifelse((x$CI_high < 0 & x$CI_low < 0) | (x$CI_high > 0 & x$CI_low > 0), "Significant", "Uncertain")
      factor(out, levels = c("Uncertain", "Significant"))
    }
  )
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
