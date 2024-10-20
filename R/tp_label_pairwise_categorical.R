.tp_label_pairwise_categorical <- function(.comparisons,
                                           datagrid,
                                           focal,
                                           need_average_predictions,
                                           margin,
                                           by_variables) {
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
        paste(.contrasts, collapse = "-")
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
          paste(unlist(contrast_terms[j, ]), collapse = "-")
        }))
      }), stringsAsFactors = FALSE)
    } else {
      # for "predictions()", we now have the row numbers. We can than extract
      # the factor levels from the data of the data grid, as row numbers in
      # "contrast_terms" correspond to rows in "grid".
      out <- as.data.frame(lapply(updated_focal, function(i) {
        unlist(lapply(seq_len(nrow(contrast_terms)), function(j) {
          .contrasts <- datagrid[[i]][as.numeric(unlist(contrast_terms[j, ]))]
          paste(.contrasts, collapse = "-")
        }))
      }), stringsAsFactors = FALSE)
    }
  }
  # the final result is a data frame with one column per focal predictor,
  # and the pairwise combinations of factor levels are the values
  colnames(out) <- updated_focal

  out
}
