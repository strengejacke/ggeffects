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

  custom_contrasts <- NULL

  # validate test argument
  if (is.null(test)) {
    test <- "contrast"
  }
  # custom tests, e.g. custom contrasts?
  if (is.data.frame(test)) {
    custom_contrasts <- test
    test <- "custom"
  }
  test <- match.arg(test, c("contrast", "pairwise", "interaction", "custom", "consecutive"))

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
    # remove "by" from "terms"
    terms <- terms[!terms %in% by]
  }

  # focal terms, representative values
  focal <- .clean_terms(terms)

  # check if focal terms appear in fixed effects
  if (!all(focal %in% insight::find_variables(model, effects = "fixed", flatten = TRUE))) {
    insight::format_error("Focal terms must be fixed effects.")
  }

  # data grids -----------------------------------------------------------------
  # we create data grids and list with representative values here. needed later
  # ----------------------------------------------------------------------------
  datagrid <- data_grid(model, terms, verbose = FALSE, ...)

  at_list <- .data_grid(
    model,
    model_frame = model_data,
    terms = terms,
    value_adjustment = "mean",
    emmeans.only = TRUE,
    verbose = FALSE
  )

  # identify numeric and other focal terms
  focal_numeric <- vapply(model_data[focal], is.numeric, TRUE)
  focal_other <- !focal_numeric

  if (identical(margin, "empirical")) {
    counterfactuals <- focal[focal_other]
  } else {
    counterfactuals <- NULL
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
      emm <- emmeans::emtrends(model, spec = focal, var = focal, regrid = "response")
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
        specs = focal[2:length(focal)],
        var = focal[1],
        at = at_list,
        by = by,
        regrid = "response",
        counterfactuals = counterfactuals
      ))

      ## TODO: 3-way interaction?

      emm <- do.call(emmeans::emtrends, my_args)
      contrast_method <- switch(test,
        custom = custom_contrasts,
        consecutive = "consec",
        contrast = "eff",
        pairwise = "pairwise"
      )
      .comparisons <- emmeans::contrast(emm, method = contrast_method, adjust = p_adjust)
      # save p-values, these get lost after call to "confint()"
      p_values <- as.data.frame(.comparisons)$p.value
      # nice data frame, including confidence intervals
      out <- suppressWarnings(as.data.frame(stats::confint(.comparisons, level = ci_level)))
      # rename
      colnames(out)[1] <- focal[2]
      out[[1]] <- gsub(" - ", "-", out[[1]], fixed = TRUE)
      estimate_name <- "Contrast"
    }

    # create nice labels ------------------------------------------------------
    # Here comes the code for pairwise comparisons of categorical focal terms
    # -------------------------------------------------------------------------

    # fix levels - here we remove the variable name from the values
    out <- .clean_levels(out, focal)
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
      regrid = "response",
      counterfactuals = counterfactuals
    ))

    emm <- do.call(emmeans::emmeans, my_args)
    .comparisons <- switch(test,
      custom = emmeans::contrast(emm, method = custom_contrasts, adjust = p_adjust),
      consecutive = emmeans::contrast(emm, method = "consec", adjust = p_adjust),
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

    # create nice labels ------------------------------------------------------
    # Here comes the code for pairwise comparisons of categorical focal terms
    # -------------------------------------------------------------------------
    if (test == "interaction") {
      colnames(out)[seq_along(focal)] <- focal
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

    # fix levels - here we remove the variable name from the values
    out <- .clean_levels(out, focal)

    if (test == "interaction") {
      # use "and" instead of "-" for labels of interaction contrasts
      for (i in 2:length(focal)) {
        out[[i]] <- gsub(" - ", " and ", out[[i]], fixed = TRUE)
        out[[i]] <- gsub("-", " and ", out[[i]], fixed = TRUE)
      }
    } else if (test == "contrast") {
      # for test = NULL, we remove the "effect" label
      out[[1]] <- gsub(" effect$", "", out[[1]])
    }
  }

  # final preparation -------------------------------------------------------

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

  out$df <- NULL
  out$std.error <- NULL

  class(out) <- c("ggcomparisons", "data.frame")
  attr(out, "ci_level") <- ci_level
  attr(out, "test") <- test
  attr(out, "p_adjust") <- p_adjust
  attr(out, "df") <- df
  attr(out, "by_factor") <- by
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


.clean_levels <- function(x, focal) {
  # fix levels - here we remove the variable name from the values
  for (i in focal) {
    # sanity check - does column exist, and is variable name not exactly identical
    # to value? E.g., if we have a variable named "coffee" with values
    # "coffee-control", we don't want to remove that variable name from the levels.
    if (i %in% colnames(x) && !any(grepl(paste0("\\b", i, "\\b"), x[[i]]))) {
      # remove variable name from contrasts levels
      x[[i]] <- gsub(i, "", x[[i]], fixed = TRUE)
      # remove spaces around "-"
      x[[i]] <- gsub(" - ", "-", x[[i]], fixed = TRUE)
    }
  }
  x
}