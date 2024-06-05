.test_predictions_ggeffects <- function(object,
                                        by = NULL,
                                        test = "pairwise",
                                        equivalence = NULL,
                                        scale = "response",
                                        p_adjust = NULL,
                                        df = NULL,
                                        ci_level = 0.95,
                                        collapse_levels = FALSE,
                                        verbose = TRUE,
                                        ...) {
  insight::check_if_installed("datawizard")

  # sanity check for certain arguments that are not (yet) supported
  if (!is.null(equivalence)) {
    insight::format_error("Equivalence testing is currently not supported for `engine = \"ggeffects\"`.")
  }
  if (!is.null(scale) && scale != "response") {
    insight::format_error("Only `scale = \"response\"` is supported for `engine = \"ggeffects\"`.")
  }

  # check test-argument
  if (is.null(test)) {
    test <- "contrasts"
  }
  test <- match.arg(test, c("contrasts", "pairwise", "interaction"))

  # we convert the ggeffects object to a data frame, using the original
  # names of the focal terms as column names
  predictions <- datagrid <- as.data.frame(object, terms_to_colnames = TRUE)

  # some attributes we need
  focal_terms <- attributes(object)$terms
  original_terms <- attributes(object)$original.terms
  at_list <- attributes(object)$at.list
  type <- attributes(object)$type
  margin <- attributes(object)$margin
  std_erros <- attributes(object)$standard_error
  dof <- attributes(object)$df
  is_latent <- !is.null(attributes(object)$latent_thresholds)

  # warn for very long at-list
  values_at_lengths <- lengths(at_list)
  if (any(values_at_lengths > 20) && verbose) {
    warning(insight::format_message(
      paste0(
        "The number of levels to compare is very high for the following terms: ",
        toString(names(values_at_lengths)[values_at_lengths > 20]),
        ". This may lead to a large number of pairwise comparisons."
      )
    ), call. = FALSE, immediate. = TRUE)
  }

  ## TODO: include vcov(predictions) in calculation of standard errors?
  # vcov matrix, for adjusting se
  vcov_matrix <- .safe(stats::vcov(object, verbose = FALSE, ...))

  # we now need to get the model object
  object <- .get_model_object(object)
  minfo <- insight::model_info(object)
  pred_type <- "response"

  # set defaults
  if (is.null(df) || is.na(df)) {
    if (is.null(dof)) {
      df <- .get_df(object)
    } else {
      df <- dof
    }
  }
  if (is.null(ci_level) || is.na(ci_level)) {
    ci_level <- 0.95
  }
  crit_factor <- (1 + ci_level) / 2

  ## TODO: For Bayesian models, we always use the returned standard errors
  # need to check whether scale is always correct

  if (!minfo$is_linear && !minfo$is_bayesian && !is_latent) {
    # zero-inflated probabilities from glmmTMB?
    if (inherits(object, "glmmTMB") && type == "zi_prob") {
      pred_type <- "zprob"
    }
    se_from_predictions <- tryCatch(
      {
        data_grid <- data_grid(object, original_terms)
        # arguments for predict(), to get SE on response scale
        # for non-Gaussian models
        my_args <- list(
          object,
          newdata = data_grid,
          type = pred_type,
          se.fit = TRUE
        )
        # for mixed models, need to set re.form to NULL or NA
        if (insight::is_mixed_model(object)) {
          if (identical(type, "re") && !identical(margin, "empirical")) {
            my_args$re.form <- NULL
          } else {
            my_args$re.form <- NA
          }
        }
        do.call(stats::predict, my_args)
      },
      error = function(e) {
        e
      }
    )
    # check if everything worked as expected
    if (inherits(se_from_predictions, "error")) {
      insight::format_error(
        "This model (family) is probably not supported. The error that occured was:",
        se_from_predictions$message
      )
    }
    # check if we have standard errors
    if (is.null(se_from_predictions$se.fit)) {
      insight::format_error("Could not extract standard errors from predictions.")
    }
    preds_with_se <- merge(
      predictions,
      cbind(data_grid, se_prob = se_from_predictions$se.fit),
      sort = FALSE,
      all = TRUE
    )
    predictions$std.error <- preds_with_se$se_prob
  }

  # check for valid by-variable
  by <- .validate_by_argument(by, predictions)

  # compute contrasts or comparisons
  out <- switch(
    test,
    contrasts = .compute_contrasts(predictions, df),
    pairwise = .compute_comparisons(predictions, df, vcov_matrix, at_list, focal_terms, crit_factor),
    interaction = .compute_interactions(predictions, df, vcov_matrix, at_list, focal_terms, crit_factor)
  )

  # for pairwise comparisons, we may have comparisons inside one level when we
  # have multiple focal terms, like "1-1" and "a-b". In this case, the comparison
  # of 1 to 1 ("1-1") is just the contrast for the level "1", we therefore can
  # collpase that string
  if (isTRUE(collapse_levels)) {
    out <- .collapse_levels(out, predictions, focal_terms, by)
  }

  # filter by-variables?
  if (!is.null(by)) {
    for (by_factor in by) {
      # values in "by" are character vectors, which are saved as "level-level".
      # we now extract the unique values, and filter the data frame
      unique_values <- unique(predictions[[by_factor]])
      by_levels <- paste0(unique_values, "-", unique_values)
      keep_rows <- out[[by_factor]] %in% c(by_levels, unique_values)
      # filter final data frame
      out <- out[keep_rows, , drop = FALSE]
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
    focal_terms <- focal_terms[!focal_terms %in% by]
    # re-arrange columns, so it matches output from "emmeans" (by comes after focal)
    out <- datawizard::data_relocate(out, select = focal_terms, before = by)
  }

  # p-value adjustment?
  if (!is.null(p_adjust)) {
    out <- .p_adjust(out, p_adjust, predictions, focal_terms, out$statistic, df, verbose)
  }

  # arrange data, but where possible, restore original data type before
  out <- .restore_focal_types(out, focal = c(by, focal_terms), model_data = predictions)
  out <- suppressWarnings(datawizard::data_arrange(out, c(by, focal_terms), safe = TRUE))

  class(out) <- c("ggcomparisons", "data.frame")
  attr(out, "ci_level") <- ci_level
  attr(out, "test") <- test
  attr(out, "p_adjust") <- p_adjust
  attr(out, "df") <- df
  attr(out, "verbose") <- verbose
  attr(out, "scale") <- "response"
  attr(out, "engine") <- "ggeffects"
  attr(out, "by_factor") <- by
  attr(out, "datagrid") <- datagrid
  attr(out, "scale_label") <- .scale_label(minfo, pred_type)
  attr(out, "standard_error") <- out$std.error
  attr(out, "link_inverse") <- insight::link_inverse(object)
  attr(out, "link_function") <- insight::link_function(object)
  attr(out, "linear_model") <- minfo$is_linear
  attr(out, "estimate_name") <- "Contrast"
  attr(out, "msg_intervals") <- FALSE

  # remove unused variables
  out$std.error <- NULL
  out$statistic <- NULL

  out
}


# contrasts ---------------------------------------------------------------
.compute_contrasts <- function(predictions, df) {
  # contrasts means we simply add the p-value to the predictions
  out <- predictions
  out$statistic <- out$predicted / out$std.error
  out$p.value <- 2 * stats::pt(abs(out$statistic), df = df, lower.tail = FALSE)
  out
}


# pairwise comparisons ----------------------------------------------------
.compute_comparisons <- function(predictions, df, vcov_matrix, at_list, focal_terms, crit_factor) {
  # pairwise comparisons are a bit more complicated, as we need to create
  # pairwise combinations of the levels of the focal terms.

  # since we split at "." later, we need to replace "." in all levels
  # with a unique character combination
  at_list <- lapply(at_list, function(i) {
    gsub(".", "#_#", as.character(i), fixed = TRUE)
  })
  # create pairwise combinations
  level_pairs <- interaction(expand.grid(at_list))
  # using the matrix and then removing the lower triangle, we get all
  # pairwise combinations, except the ones that are the same
  M <- matrix(
    1,
    nrow = length(level_pairs),
    ncol = length(level_pairs),
    dimnames = list(level_pairs, level_pairs)
  )
  M[!upper.tri(M)] <- NA
  # table() works fine to create variables of this pairwise combinations
  pairs_data <- stats::na.omit(as.data.frame(as.table(M)))
  pairs_data$Freq <- NULL
  pairs_data <- lapply(pairs_data, as.character)
  # the levels are combined by ".", we need to split them and then create
  # a list of data frames, where each data frames contains the levels of
  # the focal terms as variables
  pairs_data <- lapply(pairs_data, function(i) {
    # split at ".", which is the separator char for levels
    pair <- strsplit(i, ".", fixed = TRUE)
    # since we replaced "." with "#_#" in original levels,
    # we need to replace it back here
    pair <- lapply(pair, gsub, pattern = "#_#", replacement = ".", fixed = TRUE)
    datawizard::data_rotate(as.data.frame(pair))
  })
  # now we iterate over all pairs and try to find the corresponding predictions
  out <- do.call(rbind, lapply(seq_len(nrow(pairs_data[[1]])), function(i) {
    pos1 <- predictions[[focal_terms[1]]] == pairs_data[[1]][i, 1]
    pos2 <- predictions[[focal_terms[1]]] == pairs_data[[2]][i, 1]

    if (length(focal_terms) > 1) {
      pos1 <- pos1 & predictions[[focal_terms[2]]] == pairs_data[[1]][i, 2]
      pos2 <- pos2 & predictions[[focal_terms[2]]] == pairs_data[[2]][i, 2]
    }
    if (length(focal_terms) > 2) {
      pos1 <- pos1 & predictions[[focal_terms[3]]] == pairs_data[[1]][i, 3]
      pos2 <- pos2 & predictions[[focal_terms[3]]] == pairs_data[[2]][i, 3]
    }
    # once we have found the correct rows for the pairs, we can calculate
    # the contrast. We need the predicted values first
    predicted1 <- predictions$predicted[pos1]
    predicted2 <- predictions$predicted[pos2]
    # we then create labels for the pairs. "result" is a data frame with
    # the labels (of the pairwise contrasts) as columns.
    result <- as.data.frame(do.call(cbind, lapply(seq_along(focal_terms), function(j) {
      paste(pairs_data[[1]][i, j], pairs_data[[2]][i, j], sep = "-")
    })))
    colnames(result) <- focal_terms
    # we then add the contrast and the standard error. for linear models, the
    # SE is sqrt(se1^2 + se2^2).
    result$Contrast <- predicted1 - predicted2
    ## TODO: we may add "- 2 * vcov(predictions)[pos1, pos2]" inside sqrt() here?
    result$std.error <- sqrt(predictions$std.error[pos1]^2 + predictions$std.error[pos2]^2)
    result
  }))
  # add CI and p-values
  out$conf.low <- out$Contrast - stats::qt(crit_factor, df = df) * out$std.error
  out$conf.high <- out$Contrast + stats::qt(crit_factor, df = df) * out$std.error
  out$statistic <- out$Contrast / out$std.error
  out$p.value <- 2 * stats::pt(abs(out$statistic), df = df, lower.tail = FALSE)
  out
}


# interaction contrasts  ----------------------------------------------------
.compute_interactions <- function(predictions, df, vcov_matrix, at_list, focal_terms, crit_factor) {
  ## TODO: interaction contrasts currently only work for two focal terms
  if (length(focal_terms) != 2) {
    msg <- "Interaction contrasts currently only work for two focal terms."
    if (length(focal_terms) > 2) {
      cleaned_f3 <- .clean_terms(focal_terms[3])
      s1 <- "pr <- predict_response(\n    model,"
      s2 <- paste0("terms = c(", datawizard::text_concatenate(focal_terms[1:2], enclose = "\"", sep = ", ", last = ", "), "),") # nolint
      s3 <- paste0("condition = c(", cleaned_f3, " = \"", at_list[[cleaned_f3]][1], "\")")
      s4 <- ")"
      msg <- c(
        msg,
        "You can try to fix remaining focal terms to specific values, using the `condition` argument, e.g.:.",
        paste0("\n  ", insight::color_text(s1, "cyan")),
        paste0("  ", insight::color_text(s2, "cyan")),
        paste0("  ", insight::color_text(s3, "cyan")),
        paste0(insight::color_text(s4, "cyan")),
        insight::color_text("test_predictions(pr, engine = \"ggeffects\")", "cyan")
      )
    }
    insight::format_error(msg)
  }

  # create pairwise combinations of first focal term
  level_pairs <- at_list[[1]]
  M <- matrix(
    1,
    nrow = length(level_pairs),
    ncol = length(level_pairs),
    dimnames = list(level_pairs, level_pairs)
  )
  M[!upper.tri(M)] <- NA
  # table() works fine to create variables of this pairwise combinations
  pairs_focal1 <- stats::na.omit(as.data.frame(as.table(M)))
  pairs_focal1$Freq <- NULL

  # create pairwise combinations of second focal term
  level_pairs <- at_list[[2]]
  M <- matrix(
    1,
    nrow = length(level_pairs),
    ncol = length(level_pairs),
    dimnames = list(level_pairs, level_pairs)
  )
  M[!upper.tri(M)] <- NA
  # table() works fine to create variables of this pairwise combinations
  pairs_focal2 <- stats::na.omit(as.data.frame(as.table(M)))
  pairs_focal2$Freq <- NULL

  # now we iterate over all pairs and try to find the corresponding predictions
  out <- do.call(rbind, lapply(seq_len(nrow(pairs_focal1)), function(i) {
    # differences between levels of first focal term
    pos1 <- predictions[[focal_terms[1]]] == pairs_focal1[i, 1]
    pos2 <- predictions[[focal_terms[1]]] == pairs_focal1[i, 2]

    do.call(rbind, lapply(seq_len(nrow(pairs_focal2)), function(j) {
      # difference between levels of first focal term, *within* first
      # level of second focal term
      pos_1a <- pos1 & predictions[[focal_terms[2]]] == pairs_focal2[j, 1]
      pos_1b <- pos2 & predictions[[focal_terms[2]]] == pairs_focal2[j, 1]
      # difference between levels of first focal term, *within* second
      # level of second focal term
      pos_2a <- pos1 & predictions[[focal_terms[2]]] == pairs_focal2[j, 2]
      pos_2b <- pos2 & predictions[[focal_terms[2]]] == pairs_focal2[j, 2]
      # once we have found the correct rows for the pairs, we can calculate
      # the contrast. We need the predicted values first
      predicted1 <- predictions$predicted[pos_1a] - predictions$predicted[pos_1b]
      predicted2 <- predictions$predicted[pos_2a] - predictions$predicted[pos_2b]
      # we then create labels for the pairs. "result" is a data frame with
      # the labels (of the pairwise contrasts) as columns.
      result <- data.frame(
        a = paste(pairs_focal1[i, 1], pairs_focal1[i, 2], sep = "-"),
        b = paste(pairs_focal2[j, 1], pairs_focal2[j, 2], sep = " and "),
        stringsAsFactors = FALSE
      )
      colnames(result) <- focal_terms
      # we then add the contrast and the standard error. for linear models, the
      # SE is sqrt(se1^2 + se2^2)
      result$Contrast <- predicted1 - predicted2
      result$std.error <- sqrt(sum(
        predictions$std.error[pos_1a]^2, predictions$std.error[pos_1b]^2,
        predictions$std.error[pos_2a]^2, predictions$std.error[pos_2b]^2
      ))
      result
    }))
  }))
  # add CI and p-values
  out$conf.low <- out$Contrast - stats::qt(crit_factor, df = df) * out$std.error
  out$conf.high <- out$Contrast + stats::qt(crit_factor, df = df) * out$std.error
  out$statistic <- out$Contrast / out$std.error
  out$p.value <- 2 * stats::pt(abs(out$statistic), df = df, lower.tail = FALSE)
  out
}
