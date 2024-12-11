.tp_label_hypothesis_formula <- function(.comparisons,
                                         focal,
                                         margin,
                                         model_data,
                                         test) {
  columns_to_select <- c("hypothesis", intersect(focal, colnames(.comparisons)))
  out <- as.data.frame(.comparisons[columns_to_select], stringsAsFactors = FALSE)

  # for default margin, we don't have factor levels. get them here
  if (!margin %in% c("marginalmeans", "empirical")) {
    factor_levels <- levels(model_data[[focal[1]]])
    regex <- "\\d+(\\.\\d+)?"
    # Extract numbers using gregexpr and regmatches
    matches <- gregexpr(regex, out$hypothesis)
    numbers <- unique(unlist(regmatches(out$hypothesis, matches), use.names = FALSE))
    if (length(numbers) > length(factor_levels)) {
      factor_levels <- rep(factor_levels, length(numbers) / length(factor_levels))
    }
    factor_levels <- stats::setNames(factor_levels, seq_along(factor_levels))
    for (i in seq_along(factor_levels)) {
      out$hypothesis <- gsub(
        names(factor_levels[i]),
        factor_levels[i],
        out$hypothesis,
        fixed = TRUE
      )
    }
  }
  list(hypothesis_label = insight::safe_deparse(test), out = out)
}
