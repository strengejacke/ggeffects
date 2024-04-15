.test_predictions_ggeffects <- function(model,
                                        by = NULL,
                                        test = "pairwise",
                                        equivalence = NULL,
                                        scale = "response",
                                        p_adjust = NULL,
                                        df = NULL,
                                        ci_level = 0.95,
                                        collapse_levels = FALSE,
                                        engine = "ggeffects",
                                        verbose = TRUE,
                                        ...) {
  pr_data <- as.data.frame(model, terms_to_colnames = TRUE)

  # some attributes we need
  focal_terms <- attributes(model)$terms
  at_list <- attributes(model)$at.list
  dof <- attributes(model)$df
  model <- .get_model_object(model)
  minfo <- insight::model_info(model)

  if (is.null(test) || test == "contrasts") {
    # contrasts
    out <- pr_data
    out$statistic <- out$predicted / out$std.error
    out$p.value <- 2 * stats::pt(abs(out$tatistic), df = dof, lower.tail = FALSE)
  } else if (test == "pairwise") {
    # create pairwise combinations
    level_pairs <- interaction(expand.grid(at_list))
    M <- matrix(
      1,
      nrow = length(level_pairs),
      ncol = length(level_pairs),
      dimnames = list(level_pairs, level_pairs)
    )
    M[!upper.tri(M)] <- NA
    pairs_data <- stats::na.omit(as.data.frame(as.table(M)))
    pairs_data$Freq <- NULL
    pairs_data <- lapply(pairs_data, as.character)

    pairs_data <- lapply(pairs_data, function(i) {
      pair <- strsplit(i, ".", fixed = TRUE)
      datawizard::data_rotate(as.data.frame(pair))
    })

    out <- do.call(rbind, lapply(seq_len(nrow(pairs_data[[1]])), function(i) {
      pos1 <- pr_data[[focal_terms[1]]] == pairs_data[[1]][i, 1]
      pos2 <- pr_data[[focal_terms[1]]] == pairs_data[[2]][i, 1]

      if (length(focal_terms) > 1) {
        pos1 <- pos1 & pr_data[[focal_terms[2]]] == pairs_data[[1]][i, 2]
        pos2 <- pos2 & pr_data[[focal_terms[2]]] == pairs_data[[2]][i, 2]
      }
      if (length(focal_terms) > 2) {
        pos1 <- pos1 & pr_data[[focal_terms[3]]] == pairs_data[[1]][i, 3]
        pos2 <- pos2 & pr_data[[focal_terms[3]]] == pairs_data[[2]][i, 3]
      }

      predicted1 <- pr_data$predicted[pos1]
      predicted2 <- pr_data$predicted[pos2]

      result <- as.data.frame(do.call(cbind, lapply(seq_along(focal_terms), function(j) {
        paste(pairs_data[[1]][i, j], pairs_data[[2]][i, j], sep = "-")
      })))

      colnames(result) <- focal_terms
      result$Contrast <- predicted1 - predicted2
      result$SE <- sqrt(pr_data$std.error[pos1]^2 + pr_data$std.error[pos2]^2)
      result
    }))

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
    out <- .collapse_levels(out, pr_data, focal, by)
  }

  # p-value adjustment?
  if (!is.null(p_adjust)) {
    out <- .p_adjust(out, p_adjust, pr_data, focal, out$statistic, dof, verbose)
  }

  class(out) <- c("ggcomparisons", "data.frame")
  attr(out, "ci_level") <- ci_level
  attr(out, "test") <- test
  attr(out, "p_adjust") <- p_adjust
  attr(out, "df") <- dof
  attr(out, "verbose") <- verbose
  attr(out, "scale") <- "response"
  attr(out, "standard_error") <- out$std.error
  attr(out, "link_inverse") <- insight::link_inverse(model)
  attr(out, "link_function") <- insight::link_function(model)
  attr(out, "linear_model") <- minfo$is_linear
  attr(out, "estimate_name") <- "Contrast"
  attr(out, "msg_intervals") <- FALSE

  out
}
