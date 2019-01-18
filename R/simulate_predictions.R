#' @importFrom stats simulate quantile sd
#' @importFrom sjstats model_frame model_family
#' @importFrom rlang syms .data
#' @importFrom dplyr group_by summarize ungroup
simulate_predictions <- function(model, nsim, clean_terms, ci) {
  fitfram <- sjstats::model_frame(model)
  fam <- sjstats::model_family(model)

  if (fam$is_bin | fam$is_ordinal | fam$is_categorical)
    stop("Can't simulate predictions from models with binary, categorical or ordinal outcome. Please use another option for argument `type`.", call. = FALSE)

  sims <- stats::simulate(model, nsim = nsim, re.form = NULL)

  fitfram$predicted <- apply(sims, 1, mean)
  fitfram$conf.low <- apply(sims, 1, stats::quantile, probs = 1 - ci)
  fitfram$conf.high <- apply(sims, 1, stats::quantile, probs = ci)
  fitfram$std.error <- apply(sims, 1, stats::sd)

  grp <- rlang::syms(clean_terms)
  fitfram %>%
    dplyr::group_by(!!! grp) %>%
    dplyr::summarize(
      predicted = mean(.data$predicted),
      conf.low = mean(.data$conf.low),
      conf.high = mean(.data$conf.high),
      std.error = mean(.data$std.error)
    ) %>%
    dplyr::ungroup()
}
