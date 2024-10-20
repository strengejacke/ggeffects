.tp_label_hypothesis_categorical <- function(.comparisons,
                                             need_average_predictions,
                                             margin,
                                             object,
                                             by_variables,
                                             datagrid,
                                             df,
                                             ci_level,
                                             dot_args,
                                             include_random,
                                             focal,
                                             test) {
  # if we have specific comparisons of estimates, like "b1 = b2", we
  # want to replace these shortcuts with the full related predictor names
  # and levels
  if (any(grepl("b[0-9]+", .comparisons$term))) {
    # re-compute comoparisons for all combinations, so we know which
    # estimate refers to which combination of predictor levels
    if (need_average_predictions || margin %in% c("marginalmeans", "empirical")) {
      fun <- "avg_predictions"
    } else {
      fun <- "predictions"
    }
    fun_args <- list(
      model = object,
      variables = by_variables,
      newdata = datagrid,
      hypothesis = NULL,
      df = df,
      conf_level = ci_level
    )
    # for counterfactual predictions, we need no data grid
    if (margin == "empirical") {
      fun_args$newdata <- NULL
    }
    .full_comparisons <- .call_me(fun, fun_args, dot_args, include_random)

    # replace "hypothesis" labels with names/levels of focal predictors
    hypothesis_label <- .extract_labels(
      full_comparisons = .full_comparisons,
      focal = focal,
      test = test,
      old_labels = .comparisons$term
    )
  }
  # we have a specific hypothesis, like "b3 = b4". We just copy that information
  out <- data.frame(Hypothesis = .comparisons$term, stringsAsFactors = FALSE)

  list(hypothesis_label = hypothesis_label, out = out)
}
