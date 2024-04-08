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
                                      ...) {
  insight::check_if_installed("emmeans")

  # model information
  minfo <- insight::model_info(model, verbose = FALSE)
  model_data <- insight::get_data(model)

  ## TODO: add further scale options later?

  # for now, only response scale
  if (!identical(scale, "response")) {
    insight::format_error("Only `scale = 'response'` is currently supported.")
  }

  # validate test argument
  if (is.null(test)) {
    test <- "contrast"
  }
  test <- match.arg(test, c("contrast", "pairwise", "interaction"))

  # focal terms, representative values
  focal <- .clean_terms(terms)

  # check if focal terms appear in fixed effects
  if (!all(focal %in% insight::find_variables(model, effects = "fixed", flatten = TRUE))) {
    insight::format_error("Focal terms must be fixed effects.")
  }

  # data grids -----------------------------------------------------------------
  # we create data grids and list with representative values here. needed later
  # ----------------------------------------------------------------------------
  datagrid <- data_grid(model, terms, ...)

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

  # extract degrees of freedom
  if (is.null(df)) {
    df <- .get_df(model)
  }

  # pvalue adjustment
  if (is.null(p_adjust)) {
    p_adjust <- "none"
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
      emm <- emmeans::emtrends(m, spec = focal, var = focal)
      .comparisons <- emmeans::test(emm)
      out <- as.data.frame(emm)
      # save p-values, these get lost after call to "confint()"
      p_values <- as.data.frame(.comparisons)$p.value
      estimate_name <- "Slope"
      # rename special column with ".trend"
      colnames(out)[colnames(out) == paste0(focal, ".trend")] <- estimate_name
      # and fill with value "slope" (for printing)
      out[[1]] <- "slope"
    } else {
      # pairwise comparison of slopes -----------------------------------------
      # here comes the code to test wether slopes between groups are
      # significantly different from each other (pairwise comparison)
      # -----------------------------------------------------------------------
      my_args <- .compact_list(list(
        model,
        specs = focal[2],
        var = focal[1],
        at = at_list,
        by = by,
        counterfactuals = counterfactuals
      ))

      ## TODO: 3-way interaction?

      emm <- do.call(emmeans::emtrends, my_args)
      .comparisons <- switch(test,
        contrast = emmeans::contrast(emm, method = "eff", adjust = p_adjust),
        pairwise = emmeans::contrast(emm, method = "pairwise", adjust = p_adjust)
      )
      # save p-values, these get lost after call to "confint()"
      p_values <- as.data.frame(.comparisons)$p.value
      # nice data frame, including confidence intervals
      out <- suppressWarnings(as.data.frame(stats::confint(.comparisons, level = ci_level)))
      # rename
      colnames(out)[1] <- focal[2]
      out[[1]] <- gsub(" - ", "-", out[[1]], fixed = TRUE)
      estimate_name <- "Contrast"
    }

    # rename columns
    out <- .rename_emmeans_columns(out)

  } else {
    # testing groups (factors) ------------------------------------------------
    # Here comes the code for pairwise comparisons of categorical focal terms
    # -------------------------------------------------------------------------
    my_args <- .compact_list(list(
      model,
      specs = focal,
      at = at_list,
      by = by,
      counterfactuals = counterfactuals
    ))

    emm <- do.call(emmeans::emmeans, my_args)
    .comparisons <- switch(test,
      contrast = emmeans::contrast(emm, method = "eff", adjust = p_adjust),
      pairwise = emmeans::contrast(emm, method = "pairwise", adjust = p_adjust),
      interaction = {
        arg <- as.list(rep("pairwise", times = length(focal)))
        names(arg) <- focal
        emmeans::contrast(emm, interaction = arg, adjust = p_adjust)
      }
    )
    estimate_name <- "Contrast"
    # save p-values, these get lost after call to "confint()"
    p_values <- as.data.frame(.comparisons)$p.value
    # nice data frame, including confidence intervals
    out <- suppressWarnings(as.data.frame(stats::confint(.comparisons, level = ci_level)))

    # rename columns
    out <- .rename_emmeans_columns(out)

    # create nice labels------ ------------------------------------------------
    # Here comes the code for pairwise comparisons of categorical focal terms
    # -------------------------------------------------------------------------
    if (test == "interaction") {
      colnames(out)[1:2] <- focal
    } else if (length(focal) > 1) {
      focal_grid <- expand.grid(at_list[focal])
      contrast_levels <- do.call(rbind, lapply(1:(nrow(focal_grid) - 1), function(i) {
        suppressWarnings(cbind(focal_grid[i, ], focal_grid[-1:-i, ]))
      }))
      rownames(contrast_levels) <- NULL
      contrast_terms <- as.data.frame(lapply(seq_along(focal), function(i) {
        tmp <- contrast_levels[seq(i, ncol(contrast_levels), by = length(focal))]
        unlist(lapply(seq_len(nrow(tmp)), function(j) {
          .contrasts <- as.character(unlist(tmp[j, ]))
          paste(.contrasts, collapse = "-")
        }))
      }), stringsAsFactors = FALSE)
      # name columns
      colnames(contrast_terms) <- focal
      # remove old focal terms
      out[1] <- NULL
      # bind new focal term to "out"
      out <- cbind(contrast_terms, out)
    } else {
      # if we just have one focal term, we can rename the first column
      colnames(out)[1] <- focal
    }

    # fix levels
    for (i in focal) {
      # remove variable name from contrasts levels
      out[[i]] <- gsub(i, "", out[[i]], fixed = TRUE)
      # remove spaces around "-"
      out[[i]] <- gsub(" - ", "-", out[[i]], fixed = TRUE)
    }
    if (test == "interaction") {
      out[[2]] <- gsub("-", " and ", out[[2]], fixed = TRUE)
    }
  }

  # rename estimate column
  colnames(out)[colnames(out) == "estimate"] <- estimate_name
  # add p
  out$p.value <- p_values

  # for pairwise comparisons, we may have comparisons inside one level when we
  # have multiple focal terms, like "1-1" and "a-b". In this case, the comparison
  # of 1 to 1 ("1-1") is just the contrast for the level "1", we therefore can
  # collpase that string
  if (isTRUE(collapse_levels)) {
    out <- .collapse_levels(out, datagrid, focal, by)
  }

  ## TODO: fix levels with "-"

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

  out$df <- NULL
  out$std.error <- NULL

  class(out) <- c("ggcomparisons", "data.frame")
  attr(out, "ci_level") <- ci_level
  attr(out, "test") <- test
  attr(out, "p_adjust") <- p_adjust
  attr(out, "df") <- df
  attr(out, "linear_model") <- minfo$is_linear
  attr(out, "estimate_name") <- estimate_name
  attr(out, "verbose") <- verbose
  attr(out, "scale") <- scale
  attr(out, "scale_label") <- .scale_label(minfo, scale)
  attr(out, "standard_error") <- as.data.frame(.comparisons)$std.error
  attr(out, "link_inverse") <- insight::link_inverse(model)
  attr(out, "link_function") <- insight::link_function(model)
  attr(out, "digits") <- NULL

  out
}


# rename columns
.rename_emmeans_columns <- function(x) {
  .var_rename(
    x,
    SE = "std.error",
    lower.CL = "conf.low",
    upper.CL = "conf.high",
    asymp.LCL = "conf.low",
    asymp.UCL = "conf.high",
    lower.HPD = "conf.low",
    upper.HPD = "conf.high"
  )
}