#' @importFrom stats simulate quantile sd complete.cases
simulate_predictions <- function(model, nsim, clean_terms, ci) {
  fitfram <- insight::get_data(model)
  fam <- insight::model_info(model)

  if (fam$is_binomial | fam$is_ordinal | fam$is_categorical)
    stop("Can't simulate predictions from models with binary, categorical or ordinal outcome. Please use another option for argument `type`.", call. = FALSE)

  sims <- stats::simulate(model, nsim = nsim, re.form = NULL)

  fitfram$predicted <- apply(sims, 1, mean)
  fitfram$conf.low <- apply(sims, 1, stats::quantile, probs = 1 - ci)
  fitfram$conf.high <- apply(sims, 1, stats::quantile, probs = ci)
  fitfram$std.error <- apply(sims, 1, stats::sd)

  means_predicted <- tapply(
    fitfram$predicted,
    lapply(clean_terms, function(i) fitfram[[i]]),
    function(j) mean(j, na.rm = TRUE),
    simplify = FALSE
  )

  means_conf_low <- tapply(
    fitfram$conf.low,
    lapply(clean_terms, function(i) fitfram[[i]]),
    function(j) mean(j, na.rm = TRUE),
    simplify = FALSE
  )

  means_conf_high <- tapply(
    fitfram$conf.high,
    lapply(clean_terms, function(i) fitfram[[i]]),
    function(j) mean(j, na.rm = TRUE),
    simplify = FALSE
  )

  means_se <- tapply(
    fitfram$std.error,
    lapply(clean_terms, function(i) fitfram[[i]]),
    function(j) mean(j, na.rm = TRUE),
    simplify = FALSE
  )

  terms_df <- as.data.frame(expand.grid(attributes(means_predicted)$dimnames), stringsAsFactors = FALSE)
  colnames(terms_df) <- clean_terms

  fitfram <- cbind(
    terms_df,
    predicted = unlist(lapply(means_predicted, function(i) if (is.null(i)) NA else i)),
    conf.low = unlist(lapply(means_conf_low, function(i) if (is.null(i)) NA else i)),
    conf.high = unlist(lapply(means_conf_high, function(i) if (is.null(i)) NA else i)),
    std.error = unlist(lapply(means_se, function(i) if (is.null(i)) NA else i))
  )
  rownames(fitfram) <- NULL
  fitfram <- fitfram[stats::complete.cases(fitfram), ]

  if (length(clean_terms) == 1) {
    fitfram <- fitfram[order(fitfram[[1]]), ]
  } else if (length(clean_terms) == 2) {
    fitfram <- fitfram[order(fitfram[[1]], fitfram[[2]]), ]
  } else if (length(clean_terms) == 3) {
    fitfram <- fitfram[order(fitfram[[1]], fitfram[[2]], fitfram[[3]]), ]
  } else if (length(clean_terms) == 4) {
    fitfram <- fitfram[order(fitfram[[1]], fitfram[[2]], fitfram[[3]], fitfram[[4]]), ]
  }

  fitfram
}
