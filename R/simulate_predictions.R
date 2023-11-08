simulate_predictions <- function(model, nsim, clean_terms, ci, type) {
  fitfram <- insight::get_data(model, source = "frame")

  # sanity check - could data be extracted from model frame?
  if (is.null(fitfram)) {
    fitfram <- .safe(insight::get_data(model, source = "environment"))
  }

  fam <- insight::model_info(model)

  if (fam$is_binomial || fam$is_multinomial || fam$is_ordinal || fam$is_categorical)
    insight::format_error("Can't simulate predictions from models with binary, categorical or ordinal outcome. Please use another option for argument `type`.") # nolint

  if (type == "sim") {
    sims <- suppressWarnings(tryCatch(
      stats::simulate(model, nsim = nsim, re.form = NULL),
      error = function(e) stats::simulate(model, nsim = nsim, re.form = NA)
    ))
  } else {
    sims <- stats::simulate(model, nsim = nsim, re.form = NA)
  }

  fitfram$predicted <- rowMeans(sims)
  fitfram$conf.low <- apply(sims, 1, stats::quantile, probs = 1 - ci)
  fitfram$conf.high <- apply(sims, 1, stats::quantile, probs = ci)
  fitfram$std.error <- apply(sims, 1, stats::sd)

  means_predicted <- tapply(
    fitfram$predicted,
    lapply(clean_terms, function(i) fitfram[[i]]),
    mean,
    na.rm = TRUE,
    simplify = FALSE
  )

  means_conf_low <- tapply(
    fitfram$conf.low,
    lapply(clean_terms, function(i) fitfram[[i]]),
    mean,
    na.rm = TRUE,
    simplify = FALSE
  )

  means_conf_high <- tapply(
    fitfram$conf.high,
    lapply(clean_terms, function(i) fitfram[[i]]),
    mean,
    na.rm = TRUE,
    simplify = FALSE
  )

  means_se <- tapply(
    fitfram$std.error,
    lapply(clean_terms, function(i) fitfram[[i]]),
    mean,
    na.rm = TRUE,
    simplify = FALSE
  )

  terms_df <- data.frame(
    expand.grid(attributes(means_predicted)$dimnames),
    stringsAsFactors = FALSE
  )
  colnames(terms_df) <- clean_terms
  terms_df <- .convert_numeric_factors(terms_df)

  fitfram <- cbind(
    terms_df,
    predicted = unlist(lapply(means_predicted, function(i) if (is.null(i)) NA else i), use.names = FALSE),
    conf.low = unlist(lapply(means_conf_low, function(i) if (is.null(i)) NA else i), use.names = FALSE),
    conf.high = unlist(lapply(means_conf_high, function(i) if (is.null(i)) NA else i), use.names = FALSE),
    std.error = unlist(lapply(means_se, function(i) if (is.null(i)) NA else i), use.names = FALSE)
  )
  rownames(fitfram) <- NULL
  fitfram <- fitfram[stats::complete.cases(fitfram), , drop = FALSE]

  if (length(clean_terms) == 1) {
    fitfram <- fitfram[order(fitfram[[1]]), , drop = FALSE]
  } else if (length(clean_terms) == 2) {
    fitfram <- fitfram[order(fitfram[[1]], fitfram[[2]]), , drop = FALSE]
  } else if (length(clean_terms) == 3) {
    fitfram <- fitfram[order(fitfram[[1]], fitfram[[2]], fitfram[[3]]), , drop = FALSE]
  } else if (length(clean_terms) == 4) {
    fitfram <- fitfram[order(fitfram[[1]], fitfram[[2]], fitfram[[3]], fitfram[[4]]), , drop = FALSE]
  }

  fitfram
}



.do_simulate <- function(model, terms, ci, type = "sim", ...) {
  clean_terms <- .clean_terms(terms)
  add.args <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)

  if ("nsim" %in% names(add.args)) {
    nsim <- eval(add.args[["nsim"]])
  } else {
    nsim <- 1000
  }

  simulate_predictions(model, nsim, clean_terms, ci, type)
}
