#' @export
get_predictions.multinom <- function(model,
                                     data_grid = NULL,
                                     terms = NULL,
                                     ci_level = 0.95,
                                     type = NULL,
                                     typical = NULL,
                                     vcov = NULL,
                                     vcov_args = NULL,
                                     condition = NULL,
                                     interval = "confidence",
                                     bias_correction = FALSE,
                                     link_inverse = insight::link_inverse(model),
                                     model_info = NULL,
                                     verbose = TRUE,
                                     ...) {
  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- (1 + ci_level) / 2
  } else {
    ci <- 0.975
  }

  if (inherits(model, c("multinom_weightit", "ordinal_weightit"))) {
    # we need the name of the response in the data
    data_grid[[insight::find_response(model)]] <- insight::get_response(model)[1]
  }

  prdat <- stats::predict(
    model,
    newdata = data_grid,
    type = "probs",
    ...
  )

  if (is.data.frame(prdat) || is.matrix(prdat))
    nc <- seq_len(ncol(prdat))
  else
    nc <- 1

  # Matrix to vector
  tmp <- cbind(as.data.frame(prdat), data_grid)
  data_grid <- .gather(tmp, names_to = "response.level", values_to = "predicted", colnames(tmp)[nc])


  # se.pred <-
  #   .standard_error_predictions(
  #     model = model,
  #     prediction_data = data_grid,
  #     typical = typical,
  #     terms = terms,
  #     model_class = model_class
  #   )
  #
  # if (!is.null(se.pred)) {
  #   se.fit <- se.pred$se.fit
  #   data_grid <- se.pred$prediction_data
  #   # CI
  #   data_grid$conf.low <- link_inverse(stats::qlogis(data_grid$predicted) - stats::qnorm(ci) * se.fit)
  #   data_grid$conf.high <- link_inverse(stats::qlogis(data_grid$predicted) + stats::qnorm(ci) * se.fit)
  # } else {
  #   # No CI
  #   data_grid$conf.low <- NA
  #   data_grid$conf.high <- NA
  # }

  # No CI
  data_grid$conf.low <- NA
  data_grid$conf.high <- NA

  data_grid
}

#' @export
get_predictions.multinom_weightit <- get_predictions.multinom

#' @export
get_predictions.ordinal_weightit <- get_predictions.multinom

#' @export
get_predictions.bracl <- get_predictions.multinom

#' @export
get_predictions.brmultinom <- get_predictions.multinom
