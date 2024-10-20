.tp_label_pairwise_slopes <- function(.comparisons, datagrid, focal) {
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

  list(.comparisons = .comparisons, out = out)
}
