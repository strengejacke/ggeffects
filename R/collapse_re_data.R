.collaps_re_data <- function(grid, model, collaps_re = NULL, residuals = FALSE) {
  data <- insight::get_data(model)

  if (is.null(collaps_re)) {
    collaps_re <- insight::find_random(model, flatten = TRUE)
  }

  if (length(collaps_re) > 1) {
    collaps_re <- collaps_re[1]
    warning("More than one random grouping variable found.",
            "\n  Using `", collaps_re, "`.", call. = FALSE)
  }

  if (!collaps_re %in% colnames(data)) {
    stop("Could not find `", collaps_re, "` column.", call. = FALSE)
  }

  if (residuals) {
    rawdata <- residualize_over_grid(grid, model, protect_names = TRUE)
    y_name <- "predicted"
  } else {
    rawdata <- attr(grid, "rawdata", exact = TRUE)
    y_name <- "response"

    if (any(sapply(rawdata[-(1:2)], Negate(is.factor))) ||
        attr(grid, "x.is.factor", exact = TRUE) == "0") {
      warning("Collapsing usually not informative across a continious variable.",
              call. = FALSE)
    }
  }

  rawdata$random <- factor(data[[collaps_re]])

  agg_data <- stats::aggregate(rawdata[[y_name]],
                               by = rawdata[colnames(rawdata) != y_name],
                               FUN = mean)

  colnames(agg_data)[ncol(agg_data)] <- y_name
  colnames(agg_data)[colnames(agg_data) == "group"] <- "group_col"

  agg_data
}
