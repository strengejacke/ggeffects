#' @importFrom insight get_data
#' @importFrom rlang syms
#' @importFrom dplyr group_by summarise left_join
get_predictions_logistf <- function(model, fitfram, terms, ...) {

  prdat <- data.frame(
    predictions = model$predict,
    insight::get_data(model)
  )

  grp <- rlang::syms(terms)

  pv <- prdat %>%
    dplyr::group_by(!!! grp) %>%
    dplyr::summarise(predicted = mean(.data$predictions, na.rm = TRUE))

  fitfram <- suppressMessages(dplyr::left_join(fitfram, pv))

  # CI
  fitfram$conf.low <- NA
  fitfram$conf.high <- NA

  fitfram
}
