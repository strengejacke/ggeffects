simulate_predictions <- function(model, nsim, clean_terms, ci, type, interval = "confidence") {
  fitfram <- .get_model_data(model)
  fam <- insight::model_info(model)

  if (fam$is_binomial || fam$is_multinomial || fam$is_ordinal || fam$is_categorical)
    insight::format_error("Can't simulate predictions from models with binary, categorical or ordinal outcome. Please use another option for argument `type`.") # nolint

  if (type == "simulate") {
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

  means_predicted <- .aggregate_simulations(fitfram$predicted, clean_terms, fitfram)
  means_conf_low <- .aggregate_simulations(fitfram$conf.low, clean_terms, fitfram)
  means_conf_high <- .aggregate_simulations(fitfram$conf.high, clean_terms, fitfram)
  means_se <- .aggregate_simulations(fitfram$std.error, clean_terms, fitfram)

  colnames(means_predicted) <- c(clean_terms, "predicted")
  colnames(means_conf_low) <- c(clean_terms, "conf.low")
  colnames(means_conf_high) <- c(clean_terms, "conf.high")
  colnames(means_se) <- c(clean_terms, "std.error")

  fitfram <- cbind(
    means_predicted[clean_terms],
    predicted = means_predicted$predicted,
    conf.low = means_conf_low$conf.low,
    conf.high = means_conf_high$conf.high,
    std.error = means_se$std.error
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


.do_simulate <- function(model, terms, ci, type = "simulate", interval = "confidence", ...) {
  clean_terms <- .clean_terms(terms)
  add.args <- match.call(expand.dots = FALSE)[["..."]]

  if ("nsim" %in% names(add.args)) {
    nsim <- eval(add.args[["nsim"]])
  } else {
    nsim <- 1000
  }

  simulate_predictions(model, nsim, clean_terms, ci, type, interval)
}


.aggregate_simulations <- function(sims, clean_terms, datagrid) {
  stats::aggregate(
    sims,
    lapply(clean_terms, function(i) datagrid[[i]]),
    mean,
    na.rm = TRUE,
    simplify = TRUE
  )
}
