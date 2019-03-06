#' @importFrom dplyr bind_cols
get_predictions_multinom <- function(model, fitfram, ci.lvl, linv, typical, terms, fun, ...) {

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975


  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "probs",
      ...
    )

  if (is.data.frame(prdat) || is.matrix(prdat))
    nc <- 1:ncol(prdat)
  else
    nc <- 1

  # Matrix to vector
  tmp <- prdat %>%
    as.data.frame() %>%
    dplyr::bind_cols(fitfram)

  fitfram <- .gather(tmp, key = "response.level", value = "predicted", colnames(tmp)[nc])


  # se.pred <-
  #   get_se_from_vcov(
  #     model = model,
  #     fitfram = fitfram,
  #     typical = typical,
  #     terms = terms,
  #     fun = fun
  #   )
  #
  # if (!is.null(se.pred)) {
  #   se.fit <- se.pred$se.fit
  #   fitfram <- se.pred$fitfram
  #   # CI
  #   fitfram$conf.low <- linv(stats::qlogis(fitfram$predicted) - stats::qnorm(ci) * se.fit)
  #   fitfram$conf.high <- linv(stats::qlogis(fitfram$predicted) + stats::qnorm(ci) * se.fit)
  # } else {
  #   # No CI
  #   fitfram$conf.low <- NA
  #   fitfram$conf.high <- NA
  # }

  # No CI
  fitfram$conf.low <- NA
  fitfram$conf.high <- NA

  fitfram
}
