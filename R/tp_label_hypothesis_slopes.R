.tp_label_hypothesis_slopes <- function(.comparisons,
                                        object,
                                        focal,
                                        df,
                                        ci_level,
                                        dot_args,
                                        include_random) {
  # if we have specific comparisons of estimates, like "b1 = b2", we
  # want to replace these shortcuts with the full related predictor names
  # and levels
  if (any(grepl("b[0-9]+", .comparisons$term))) {
    # prepare argument list for "marginaleffects::slopes"
    # we add dot-args later, that modulate the scale of the contrasts
    fun_args <- list(
      model = object,
      variables = focal[1],
      by = focal[2:length(focal)],
      hypothesis = NULL,
      df = df,
      conf_level = ci_level
    )
    # re-compute comoparisons for all combinations, so we know which
    # estimate refers to which combination of predictor levels
    .full_comparisons <- .call_me("slopes", fun_args, dot_args, include_random)
    # replace "hypothesis" labels with names/levels of focal predictors
    hypothesis_label <- .extract_labels(
      full_comparisons = .full_comparisons,
      focal = focal[2:length(focal)],
      test = test,
      old_labels = .comparisons$term
    )
  }
  # we have a specific hypothesis, like "b3 = b4". We just copy that information
  out <- data.frame(Hypothesis = .comparisons$term, stringsAsFactors = FALSE)

  list(hypothesis_label = hypothesis_label, out = out)
}
