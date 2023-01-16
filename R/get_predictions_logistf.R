get_predictions_logistf <- function(model, fitfram, terms, ...) {

  prdat <- data.frame(
    predictions = model$predict,
    insight::get_data(model, source = "frame")
  )

  grp_means <- tapply(
    prdat$predictions,
    lapply(terms, function(i) prdat[[i]]),
    function(j) mean(j, na.rm = TRUE),
    simplify = FALSE
  )

  terms_df <- data.frame(expand.grid(attributes(grp_means)$dimnames), stringsAsFactors = FALSE)
  colnames(terms_df) <- terms
  terms_df <- .convert_numeric_factors(terms_df)

  pv <- cbind(terms_df, predicted = unlist(grp_means))
  rownames(pv) <- NULL

  fitfram <- merge(fitfram, pv)

  # CI
  fitfram$conf.low <- NA
  fitfram$conf.high <- NA

  fitfram
}
