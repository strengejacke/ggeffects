.test_predictions_emmeans <- function(object,
                                      terms = NULL,
                                      by = NULL,
                                      test = "pairwise",
                                      test_args = NULL,
                                      equivalence = NULL,
                                      scale = "response",
                                      p_adjust = NULL,
                                      df = NULL,
                                      ci_level = 0.95,
                                      collapse_levels = FALSE,
                                      margin = "marginalmeans",
                                      verbose = TRUE,
                                      ...) {
  insight::check_if_installed(c("emmeans", "datawizard"))

  # model information
  minfo <- insight::model_info(object, verbose = FALSE)
  model_data <- insight::get_data(object, verbose = FALSE)

  ## TODO: add further scale options later?

  # for now, only response scale
  if (!identical(scale, "response")) {
    insight::format_error("Only `scale = \"response\"` is currently supported.")
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
  test <- match.arg(
    test,
    c("contrast", "pairwise", "interaction", "custom", "exclude", "consecutive", "polynomial")
  )

  # check for valid by-variable
  by <- .validate_by_argument(by, model_data)
  if (!is.null(by)) {
    # remove "by" from "terms"
    terms <- terms[!terms %in% by]
  }

  # focal terms, representative values
  focal <- .clean_terms(terms)

  # check if focal terms appear in fixed effects
  if (!all(focal %in% insight::find_variables(object, effects = "fixed", flatten = TRUE))) {
    insight::format_error("Focal terms must be fixed effects.")
  }

  # data grids -----------------------------------------------------------------
  # we create data grids and list with representative values here. needed later
  # ----------------------------------------------------------------------------
  datagrid <- data_grid(object, terms, verbose = FALSE, ...)

  at_list <- .data_grid(
    object,
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
    df <- .get_df(object)
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
      emm <- emmeans::emtrends(object, spec = focal, var = focal, regrid = "response")
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
        object,
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
        exclude = "del.eff",
        polynomial = "poly",
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
      object,
      specs = focal,
      at = at_list,
      by = by,
      regrid = "response",
      counterfactuals = counterfactuals
    ))

    emm <- do.call(emmeans::emmeans, my_args)
    # set default option when test is not "interaction"
    if (is.null(test_args) && test != "interaction") {
      test_args <- emmeans::get_emm_option("contrast")
    }
    .comparisons <- switch(test,
      custom = emmeans::contrast(emm, method = custom_contrasts, adjust = p_adjust, option = test_args),
      consecutive = emmeans::contrast(emm, method = "consec", adjust = p_adjust, option = test_args),
      contrast = emmeans::contrast(emm, method = "eff", adjust = p_adjust, option = test_args),
      exclude = emmeans::contrast(emm, method = "del.eff", adjust = p_adjust, option = test_args),
      polynomial = emmeans::contrast(emm, method = "poly", adjust = p_adjust, option = test_args),
      pairwise = emmeans::contrast(emm, method = "pairwise", adjust = p_adjust, option = test_args),
      interaction = {
        if (is.null(test_args)) {
          test_args <- as.list(rep("pairwise", times = length(focal)))
          names(test_args) <- focal
        }
        emmeans::contrast(emm, interaction = test_args, adjust = p_adjust)
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
    } else if (test %in% c("contrast", "exclude")) {
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

  out$std.error <- as.data.frame(.comparisons)$SE

  # we now sort rows, first by "by", than by "focal". Since "by" terms can also
  # be in "focal", we need to remove "by" from "focal" first.
  if (!is.null(by)) {
    focal <- c(by, focal[!focal %in% by])
    # restore original type of focal terms, for data_arrange
    out <- .restore_focal_types(out, focal, model_data)
  }
  out <- suppressWarnings(datawizard::data_arrange(out, focal, safe = TRUE))

  class(out) <- c("ggcomparisons", "data.frame")
  attr(out, "ci_level") <- ci_level
  attr(out, "test") <- test
  attr(out, "p_adjust") <- p_adjust
  attr(out, "df") <- df
  attr(out, "by_factor") <- by
  attr(out, "datagrid") <- datagrid
  attr(out, "linear_model") <- minfo$is_linear
  attr(out, "estimate_name") <- estimate_name
  attr(out, "verbose") <- verbose
  attr(out, "engine") <- "emmeans"
  attr(out, "scale") <- scale
  attr(out, "scale_label") <- .scale_label(minfo, scale)
  attr(out, "standard_error") <- out$std.error
  attr(out, "link_inverse") <- .link_inverse(object, ...)
  attr(out, "link_function") <- insight::link_function(object)
  attr(out, "digits") <- NULL

  out$std.error <- NULL
  out$df <- NULL

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


.is_emmeans_contrast <- function(test) {
  identical(test, "interaction") ||
    identical(test, "consec") ||
    identical(test, "exclude") ||
    identical(test, "contrast") ||
    identical(test, "polynomial") ||
    identical(test, "consecutive") ||
    is.data.frame(test)
}


.restore_focal_types <- function(out, focal, model_data) {
  if (is.null(focal)) {
    return(out)
  }
  if (is.null(model_data)) {
    return(out)
  }
  if (!all(focal %in% colnames(out))) {
    return(out)
  }
  if (!all(focal %in% colnames(model_data))) {
    return(out)
  }
  focal <- unique(focal)
  # check type for all focal terms
  for (i in focal) {
    # check if all levels from comparisons also appear in the data - else, we
    # cannot safely restore the type
    if (all(unique(out[[i]]) %in% unique(model_data[[i]]))) {
      if (is.factor(model_data[[i]]) && !is.factor(out[[i]])) {
        # restore factors
        out[[i]] <- factor(out[[i]], levels = levels(model_data[[i]]))
      } else if (is.character(model_data[[i]]) && !is.character(out[[i]])) {
        # restore characters
        out[[i]] <- as.character(out[[i]])
      }
    }
  }
  out
}
