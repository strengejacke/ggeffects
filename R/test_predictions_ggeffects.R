.test_predictions_ggeffects <- function(model,
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
  # we convert the ggeffects object to a data frame, using the original
  # names of the focal terms as column names
  predictions <- as.data.frame(model, terms_to_colnames = TRUE)

  # some attributes we need
  focal_terms <- attributes(model)$terms
  at_list <- attributes(model)$at.list
  dof <- attributes(model)$df
  # we now need to get the model object
  model <- .get_model_object(model)
  minfo <- insight::model_info(model)

  ## TODO: currently only works for linear models
  if (!minfo$is_linear) {
    insight::format_error("Currently, only linear models are supported.")
  }

  # check for valid by-variable
  by <- .validate_by_argument(by, predictions)

  if (is.null(test) || test == "contrasts") {
    # contrasts ---------------------------------------------------------------
    # contrasts means we simply add the p-value to the predictions
    # -------------------------------------------------------------------------
    out <- predictions
    out$statistic <- out$predicted / out$std.error
    out$p.value <- 2 * stats::pt(abs(out$statistic), df = dof, lower.tail = FALSE)
  } else if (test == "pairwise") {
    # pairwise comparisons ----------------------------------------------------
    # pairwise comparisons are a bit more complicated, as we need to create
    # pairwise combinations of the levels of the focal terms.
    # -------------------------------------------------------------------------

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
      pair <- strsplit(i, ".", fixed = TRUE)
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
      # SE is sqrt(se1^2 + se2^2)
      result$Contrast <- predicted1 - predicted2
      result$SE <- sqrt(predictions$std.error[pos1]^2 + predictions$std.error[pos2]^2)
      result
    }))
    # add CI and p-values
    out$CI_low <- out$Contrast - stats::qt(0.975, df = dof) * out$SE
    out$CI_high <- out$Contrast + stats::qt(0.975, df = dof) * out$SE
    out$Statistic <- out$Contrast / out$SE
    out$p <- 2 * stats::pt(abs(out$Statistic), df = dof, lower.tail = FALSE)
  }

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
  }

  # p-value adjustment?
  if (!is.null(p_adjust)) {
    out <- .p_adjust(out, p_adjust, predictions, focal_terms, out$statistic, dof, verbose)
  }

  class(out) <- c("ggcomparisons", "data.frame")
  attr(out, "ci_level") <- ci_level
  attr(out, "test") <- test
  attr(out, "p_adjust") <- p_adjust
  attr(out, "df") <- dof
  attr(out, "verbose") <- verbose
  attr(out, "scale") <- "response"
  attr(out, "standard_error") <- out$SE
  attr(out, "link_inverse") <- insight::link_inverse(model)
  attr(out, "link_function") <- insight::link_function(model)
  attr(out, "linear_model") <- minfo$is_linear
  attr(out, "estimate_name") <- "Contrast"
  attr(out, "msg_intervals") <- FALSE

  # remove unused variables
  out$SE <- NULL
  out$Statistic <- NULL

  out
}
