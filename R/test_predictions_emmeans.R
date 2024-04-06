.test_predictions_emmeans <- function(model,
                                      terms = NULL,
                                      by = NULL,
                                      test = "pairwise",
                                      equivalence = NULL,
                                      scale = "response",
                                      p_adjust = NULL,
                                      df = NULL,
                                      ci_level = 0.95,
                                      collapse_levels = FALSE,
                                      margin = "marginalmeans",
                                      verbose = TRUE,
                                      dot_args = NULL,
                                      ...) {
  insight::check_if_installed("emmeans")

  # model information
  minfo <- insight::model_info(model, verbose = FALSE)
  model_data <- insight::get_data(model)

  # focal terms, representative values
  focal <- .clean_terms(terms)

  at_list <- .data_grid(
    model,
    model_frame = model_data,
    terms = terms,
    value_adjustment = "mean",
    emmeans.only = TRUE
  )

  # identify numeric and other focal terms
  focal_numeric <- vapply(model_data[focal], is.numeric, TRUE)
  focal_other <- !focal_numeric

  if (identical(margin, "empirical")) {
    counterfactuals <- focal[focal_other]
  } else {
    counterfactuals <- NULL
  }

  # check for valid by-variable
  if (!is.null(by)) {
    # all by-terms need to be in data grid
    if (!all(by %in% colnames(model_data))) {
      insight::format_error(
        paste0("Variable(s) `", toString(by[!by %in% colnames(model_data)]), "` not found in data grid.")
      )
    }
    # by-terms must be categorical
    by_factors <- vapply(model_data[by], is.factor, TRUE)
    if (!all(by_factors)) {
      insight::format_error(
        "All variables in `by` must be categorical.",
        paste0(
          "The following variables in `by` are not categorical: ",
          toString(paste0("`", by[!by_factors], "`"))
        )
      )
    }
  }

  ## TODO: how must args look like for "emtrends"?
  my_args <- list(
    specs = focal,
    at = at_list,
    by = by,
    counterfactuals = counterfactuals
  )

  # extract degrees of freedom
  if (is.null(df)) {
    df <- .get_df(model)
  }

  # numeric focal terms (slopes) ----------------------------------------------
  # we *only* calculate (average) slopes when numeric focal terms come first
  # thus, we don't need to care about the "margin" argument here
  # ---------------------------------------------------------------------------

  # if *first* focal predictor is numeric, compute average slopes
  if (isTRUE(focal_numeric[1])) {

    # just the "trend" (slope) of one focal predictor
    if (length(focal) == 1) {
      # contrasts of slopes ---------------------------------------------------
      # here comes the code to test wether a slope is significantly different
      # from null (contrasts)
      # -----------------------------------------------------------------------
    } else {
      # pairwise comparison of slopes -----------------------------------------
      # here comes the code to test wether slopes between groups are
      # significantly different from each other (pairwise comparison)
      # -----------------------------------------------------------------------

    }

  } else {

    # testing groups (factors) ------------------------------------------------
    # Here comes the code for pairwise comparisons of categorical focal terms
    # -------------------------------------------------------------------------

    data(efc, package = "ggeffects")
    efc <- datawizard::to_factor(efc, c("e42dep", "c172code", "e16sex"))

    fit <- suppressWarnings(lme4::glmer(
      tot_sc_e ~ e42dep + e17age + e16sex * c172code + (1 | e15relat),
      data = efc,
      family = poisson()
    ))
    # count diff
    emmeans(fit, "e16sex", by = "c172code") |> contrast(method = "pairwise", adjust = "none")
    emmeans(fit, c("e16sex", "c172code")) |> contrast(method = "pairwise", adjust = "none")
    emmeans(fit, "e16sex", counterfactuals = "e16sex") |> contrast(method = "pairwise", adjust = "none")

    # IRR
    emmeans(fit, "e16sex") |> pairs(adjust = "none", type = "response")

  }


  # further results
  out[[estimate_name]] <- .comparisons$estimate
  out$conf.low <- .comparisons$conf.low
  out$conf.high <- .comparisons$conf.high
  out$p.value <- .comparisons$p.value

  # for pairwise comparisons, we may have comparisons inside one level when we
  # have multiple focal terms, like "1-1" and "a-b". In this case, the comparison
  # of 1 to 1 ("1-1") is just the contrast for the level "1", we therefore can
  # collpase that string
  if (isTRUE(collapse_levels)) {
    out <- .collapse_levels(out, datagrid, focal, by)
  }

  # replace back commas
  for (i in focal) {
    # in ".fix_comma_levels()", we replaced "," by "#*#", and now
    # we need to revert this, to preserve original level strings
    if (any(grepl("#*#", out[[i]], fixed = TRUE))) {
      out[[i]] <- gsub("#*#", ",", out[[i]], fixed = TRUE)
    }
  }

  # filter by-variables?
  if (!is.null(by)) {
    for (by_factor in by) {
      # values in "by" are character vectors, which are saved as "level-level".
      # we now extract the unique values, and filter the data frame
      unique_values <- unique(datagrid[[by_factor]])
      by_levels <- paste0(unique_values, "-", unique_values)
      keep_rows <- out[[by_factor]] %in% c(by_levels, unique_values)
      # filter final data frame
      out <- out[keep_rows, , drop = FALSE]
      # but we also need to filter the ".comparisons" data frame
      .comparisons <- .comparisons[keep_rows, , drop = FALSE]
      # finally, replace "level-level" just by "level"
      for (i in seq_along(by_levels)) {
        out[[by_factor]] <- gsub(
          by_levels[i],
          unique_values[i],
          out[[by_factor]],
          fixed = TRUE
        )
      }
    }
    # remove by-terms from focal terms
    focal <- focal[!focal %in% by]
  }

  # p-value adjustment?
  if (!is.null(p_adjust)) {
    out <- .p_adjust(out, p_adjust, datagrid, focal, .comparisons$statistic, df, verbose)
  }

  # add back response levels?
  if ("group" %in% colnames(.comparisons)) {
    out <- cbind(
      data.frame(response.level = .comparisons$group, stringsAsFactors = FALSE),
      out
    )
  } else if (minfo$is_ordinal || minfo$is_multinomial) {
    resp_levels <- levels(insight::get_response(model))
    if (!is.null(resp_levels) && all(rowMeans(sapply(resp_levels, grepl, .comparisons$term, fixed = TRUE)) > 0)) { # nolint
      colnames(out)[seq_along(focal)] <- paste0("Response Level by ", paste0(focal, collapse = " & "))
      if (length(focal) > 1) {
        out[2:length(focal)] <- NULL
      }
    }
  }

  class(out) <- c("ggcomparisons", "data.frame")
  attr(out, "ci_level") <- ci_level
  attr(out, "test") <- test
  attr(out, "p_adjust") <- p_adjust
  attr(out, "df") <- df
  attr(out, "rope_range") <- rope_range
  attr(out, "scale") <- scale
  attr(out, "scale_label") <- .scale_label(minfo, scale)
  attr(out, "linear_model") <- minfo$is_linear
  attr(out, "hypothesis_label") <- hypothesis_label
  attr(out, "estimate_name") <- estimate_name
  attr(out, "msg_intervals") <- msg_intervals
  attr(out, "verbose") <- verbose
  attr(out, "standard_error") <- .comparisons$std.error
  attr(out, "link_inverse") <- insight::link_inverse(model)
  attr(out, "link_function") <- insight::link_function(model)

  out
}
