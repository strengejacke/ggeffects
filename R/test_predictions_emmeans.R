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

      # argument "test" will be ignored for average slopes
      test <- NULL
      # prepare argument list for "marginaleffects::slopes"
      # we add dot-args later, that modulate the scale of the contrasts
      fun_args <- list(
        model = model,
        variables = focal,
        df = df,
        conf_level = ci_level
      )
      .comparisons <- do.call(
        get("avg_slopes", asNamespace("marginaleffects")),
        .compact_list(c(fun_args, dot_args))
      )
      # "extracting" labels for this simple case is easy...
      out <- data.frame(x_ = "slope", stringsAsFactors = FALSE)
      colnames(out) <- focal

    } else {

      # pairwise comparison of slopes -----------------------------------------
      # here comes the code to test wether slopes between groups are
      # significantly different from each other (pairwise comparison)
      # -----------------------------------------------------------------------

      # prepare argument list for "marginaleffects::slopes"
      # we add dot-args later, that modulate the scale of the contrasts
      fun_args <- list(
        model = model,
        variables = focal[1],
        by = focal[2:length(focal)],
        newdata = datagrid,
        hypothesis = test,
        df = df,
        conf_level = ci_level
      )
      # "trends" (slopes) of numeric focal predictor by group levels
      # of other focal predictor
      .comparisons <- do.call(
        get("slopes", asNamespace("marginaleffects")),
        .compact_list(c(fun_args, dot_args))
      )

      # labelling terms ------------------------------------------------------
      # here comes the code for extracting nice term labels
      # ----------------------------------------------------------------------

      # for pairwise comparisons, we need to extract contrasts
      if (!is.null(test) && all(test == "pairwise")) {

        # pairwise comparisons of slopes --------------------------------------
        # here comes the code to extract labels for pairwise comparison of slopes
        # ---------------------------------------------------------------------

        # before we extract labels, we need to check whether any factor level
        # contains a "," - in this case, strplit() will not work properly
        .comparisons$term <- .fix_comma_levels(.comparisons$term, datagrid, focal)

        # if we find a comma in the terms column, we have two categorical predictors
        if (any(grepl(",", .comparisons$term, fixed = TRUE))) {
          contrast_terms <- data.frame(
            do.call(rbind, strsplit(.comparisons$term, " - ", fixed = TRUE)),
            stringsAsFactors = FALSE
          )

          # split and recombine term names
          pairs1 <- unlist(strsplit(contrast_terms[[1]], ",", fixed = TRUE))
          pairs2 <- unlist(strsplit(contrast_terms[[2]], ",", fixed = TRUE))
          contrast_pairs <- paste0(
            insight::trim_ws(pairs1),
            "-",
            insight::trim_ws(pairs2)
          )

          # create data frame - since we have two categorical predictors at
          # this point (and one numerical), we create a data frame with three
          # columns (one per focal term).
          out <- data.frame(
            x_ = "slope",
            x__ = contrast_pairs[c(TRUE, FALSE)],
            x___ = contrast_pairs[c(FALSE, TRUE)],
            stringsAsFactors = FALSE
          )
        } else {

          out <- data.frame(
            x_ = "slope",
            x__ = gsub(" ", "", .comparisons$term, fixed = TRUE),
            stringsAsFactors = FALSE
          )
        }
        colnames(out) <- focal

      } else if (is.null(test)) {

        # contrasts of slopes -------------------------------------------------
        # here comes the code to extract labels for trends of slopes
        # ---------------------------------------------------------------------

        # if we have simple slopes without pairwise comparisons, we can
        # copy the information directly from the marginaleffects-object
        grid_categorical <- as.data.frame(
          .comparisons[focal[2:length(focal)]],
          stringsAsFactors = FALSE
        )
        out <- cbind(data.frame(x_ = "slope", stringsAsFactors = FALSE), grid_categorical)
        colnames(out) <- focal

      } else {

        # hypothesis testing of slopes ----------------------------------------
        # here comes the code to extract labels for special hypothesis tests
        # ---------------------------------------------------------------------

        # if we have specific comparisons of estimates, like "b1 = b2", we
        # want to replace these shortcuts with the full related predictor names
        # and levels
        if (any(grepl("b[0-9]+", .comparisons$term))) {
          # prepare argument list for "marginaleffects::slopes"
          # we add dot-args later, that modulate the scale of the contrasts
          fun_args <- list(
            model = model,
            variables = focal[1],
            by = focal[2:length(focal)],
            hypothesis = NULL,
            df = df,
            conf_level = ci_level
          )
          # re-compute comoparisons for all combinations, so we know which
          # estimate refers to which combination of predictor levels
          .full_comparisons <- do.call(
            get("slopes", asNamespace("marginaleffects")),
            c(fun_args, dot_args)
          )
          # replace "hypothesis" labels with names/levels of focal predictors
          hypothesis_label <- .extract_labels(
            full_comparisons = .full_comparisons,
            focal = focal[2:length(focal)],
            test = test,
            old_labels = .comparisons$term
          )
        }
        # we have a specific hypothesis, like "b3 = b4". We just copy that information
        out <- data.frame(Hypothesis = .comparisons$term, stringsAsFactors = FALSE)
      }
    }
    estimate_name <- ifelse(is.null(test), "Slope", "Contrast")

  } else {

    # testing groups (factors) ------------------------------------------------
    # Here comes the code for pairwise comparisons of categorical focal terms
    # -------------------------------------------------------------------------

    by_variables <- NULL # for average predictions
    fun <- "predictions"

    # "variables" argument ----------------------------------------------------
    # for mixed models, and for "marginalmeans" and "empirical", we need to
    # provide the "variables" argument to "marginaleffects::predictions".
    # furthermore, for mixed models, we average across random effects and thus
    # use "avg_predictions" instead of "predictions"
    # -------------------------------------------------------------------------

    if (need_average_predictions) {
      # marginaleffects handles single and multiple variables differently here
      if (length(focal) > 1) {
        by_variables <- sapply(focal, function(i) unique(datagrid[[i]]), simplify = FALSE) # nolint
      } else {
        by_variables <- focal
      }
      by_arg <- NULL
      fun <- "avg_predictions"
    } else if (margin %in% c("marginalmeans", "empirical")) {
      # for "marginalmeans", we need "variables" argument. This must be a list
      # with representative values. Else, we cannot calculate comparisons at
      # representative values of the balanced data grid
      by_variables <- .data_grid(
        model,
        model_frame = insight::get_data(model),
        terms = terms,
        value_adjustment = "mean",
        emmeans.only = TRUE
      )
    }
    # prepare argument list for "marginaleffects::predictions"
    # we add dot-args later, that modulate the scale of the contrasts
    fun_args <- list(
      model = model,
      by = by_arg,
      variables = by_variables,
      newdata = datagrid,
      hypothesis = test,
      df = df,
      conf_level = ci_level
    )
    # for counterfactual predictions, we need no data grid
    if (margin == "empirical") {
      fun_args$newdata <- NULL
    }
    .comparisons <- do.call(
      get(fun, asNamespace("marginaleffects")),
      .compact_list(c(fun_args, dot_args))
    )

    # nice term labels --------------------------------------------------------
    # here comes the code for extracting nice term labels ---------------------
    # -------------------------------------------------------------------------

    # pairwise comparisons - we now extract the group levels from the "term"
    # column and create separate columns for contrats of focal predictors
    if (!is.null(test) && all(test == "pairwise")) {

      ## pairwise comparisons of group levels -----

      # for "predictions()", term name is "Row 1 - Row 2" etc. For
      # "avg_predictions()", we have "level_a1, level_b1 - level_a2, level_b1"
      # etc. we first want to have a data frame, where each column is one
      # combination of levels, so we split at "," and/or "-".

      # before we extract labels, we need to check whether any factor level
      # contains a "," - in this case, strplit() will not work properly
      .comparisons$term <- .fix_comma_levels(.comparisons$term, datagrid, focal)

      # split and recombine levels of focal terms. We now have a data frame
      # where each column represents one factor level of one focal predictor
      contrast_terms <- data.frame(
        do.call(rbind, strsplit(.comparisons$term, "(,| - )")),
        stringsAsFactors = FALSE
      )
      contrast_terms[] <- lapply(contrast_terms, function(i) {
        # remove certain chars
        for (j in c("(", ")", "Row")) {
          i <- gsub(j, "", i, fixed = TRUE)
        }
        insight::trim_ws(i)
      })

      if (need_average_predictions || margin %in% c("marginalmeans", "empirical")) {
        # in .comparisons$term, for mixed models and when "margin" is either
        # "marginalmeans" or "empirical", we have the factor levels as values,
        # where factor levels from different variables are comma-separated. There
        # are edge cases where we have more than one focal term, but for one of
        # those only one value is requested, e.g.: `terms = c("sex", "education [high]")`
        # in this case, .comparisons$term only contains levels of the first focal
        # term, and no comma (no levels of second focal term are comma separated).
        # in such cases, we simply remove those focal terms, which levels are not
        # appearing in .comparisons$term

        # we first need to get the relevant values / factor levels we want to
        # check for. These can be different from the data grid, when representative
        # values are specified in brackets via the "terms" argument.
        if (is.list(by_variables)) {
          values_to_check <- by_variables
        } else {
          values_to_check <- lapply(datagrid[focal], unique)
        }

        # we then check whether representative values of focal terms actually
        # appear in our pairwise comparisons data frame.
        focal_found <- vapply(values_to_check, function(i) {
          any(vapply(as.character(i), function(j) {
            any(grepl(j, unique(as.character(.comparisons$term)), fixed = TRUE))
          }, TRUE))
        }, TRUE)

        # we temporarily update our focal terms, for extracting labels.
        if (all(focal_found)) {
          updated_focal <- focal
        } else {
          updated_focal <- focal[focal_found]
        }

        # for "avg_predictions()", we already have the correct labels of factor
        # levels, we just need to re-arrange, so that each column represents a
        # pairwise combination of factor levels for each factor
        out <- as.data.frame(lapply(seq_along(updated_focal), function(i) {
          tmp <- contrast_terms[seq(i, ncol(contrast_terms), by = length(updated_focal))]
          unlist(lapply(seq_len(nrow(tmp)), function(j) {
            .contrasts <- as.character(unlist(tmp[j, ]))
            .contrasts_string <- paste(.contrasts, collapse = "-")
          }))
        }), stringsAsFactors = FALSE)

      } else {

        # only for temporary use, for colnames, see below
        updated_focal <- focal
        # check whether we have row numbers, or (e.g., for polr or ordinal models)
        # factor levels. When we have row numbers, we coerce them to numeric and
        # extract related factor levels. Else, in case of ordinal outcomes, we
        # should already have factor levels...
        if (all(vapply(contrast_terms, function(i) anyNA(suppressWarnings(as.numeric(i))), TRUE)) || minfo$is_ordinal || minfo$is_multinomial) { # nolint
          out <- as.data.frame(lapply(updated_focal, function(i) {
            unlist(lapply(seq_len(nrow(contrast_terms)), function(j) {
              .contrasts_string <- paste(unlist(contrast_terms[j, ]), collapse = "-")
            }))
          }), stringsAsFactors = FALSE)
        } else {
          # for "predictions()", we now have the row numbers. We can than extract
          # the factor levels from the data of the data grid, as row numbers in
          # "contrast_terms" correspond to rows in "grid".
          out <- as.data.frame(lapply(updated_focal, function(i) {
            unlist(lapply(seq_len(nrow(contrast_terms)), function(j) {
              .contrasts <- datagrid[[i]][as.numeric(unlist(contrast_terms[j, ]))]
              .contrasts_string <- paste(.contrasts, collapse = "-")
            }))
          }), stringsAsFactors = FALSE)
        }

      }
      # the final result is a data frame with one column per focal predictor,
      # and the pairwise combinations of factor levels are the values
      colnames(out) <- updated_focal

    } else if (is.null(test)) {

      ## contrasts of group levels -----

      # we have simple contrasts - we can just copy from the data frame
      # returned by "marginaleffects" to get nice labels
      out <- as.data.frame(.comparisons[focal], stringsAsFactors = FALSE)

    } else {

      ## hypothesis testing of group levels -----

      # if we have specific comparisons of estimates, like "b1 = b2", we
      # want to replace these shortcuts with the full related predictor names
      # and levels
      if (any(grepl("b[0-9]+", .comparisons$term))) {
        # re-compute comoparisons for all combinations, so we know which
        # estimate refers to which combination of predictor levels
        if (need_average_predictions || margin %in% c("marginalmeans", "empirical")) {
          fun <- "avg_predictions"
        } else {
          fun <- "predictions"
        }
        fun_args <- list(
          model = model,
          variables = by_variables,
          newdata = datagrid,
          hypothesis = NULL,
          df = df,
          conf_level = ci_level
        )
        # for counterfactual predictions, we need no data grid
        if (margin == "empirical") {
          fun_args$newdata <- NULL
        }

        .full_comparisons <- do.call(
          get(fun, asNamespace("marginaleffects")),
          c(fun_args, dot_args)
        )

        # replace "hypothesis" labels with names/levels of focal predictors
        hypothesis_label <- .extract_labels(
          full_comparisons = .full_comparisons,
          focal = focal,
          test = test,
          old_labels = .comparisons$term
        )
      }
      # we have a specific hypothesis, like "b3 = b4". We just copy that information
      out <- data.frame(Hypothesis = .comparisons$term, stringsAsFactors = FALSE)
    }
    estimate_name <- ifelse(is.null(test), "Predicted", "Contrast")
  }

  # add result from equivalence test
  if (!is.null(rope_range)) {
    .comparisons <- marginaleffects::hypotheses(.comparisons, equivalence = rope_range, conf_level = ci_level)
    .comparisons$p.value <- .comparisons$p.value.equiv
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
