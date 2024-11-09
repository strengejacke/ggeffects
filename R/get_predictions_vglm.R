#' @export
get_predictions.vglm <- function(model,
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
  insight::check_if_installed("VGAM", "to calculate adjusted predictions for a vector generalized linear model")

  se <- !is.null(ci_level) && !is.na(ci_level)

  is_multivariate <- isTRUE(model@extra$multiple.responses)

  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- (1 + ci_level) / 2
  } else {
    ci <- 0.975
  }

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  if ((model_info$is_ordinal || model_info$is_multinomial) && !isTRUE(se)) {
    type <- "response"
  } else {
    type <- "link"
  }

  prdat <- VGAM::predictvglm(
    model,
    newdata = data_grid,
    type = type,
    se.fit = se,
    ...
  )

  if (model_info$is_ordinal || model_info$is_multinomial || is_multivariate) {
    # start here with cumulative link models
    resp <- insight::get_response(model, verbose = FALSE)

    if (is.data.frame(resp))
      resp.names <- colnames(resp)
    else
      resp.names <- levels(resp)

    if (se) {
      dat <- data.frame(predicted = prdat$fitted.values)
      if (!is_multivariate) {
        resp.names <- resp.names[-1]
      }
    } else {
      dat <- data.frame(predicted = prdat)
      link_inverse <- function(mu) mu
    }

    colnames(dat) <- resp.names
    data_grid <- cbind(dat, data_grid)

    # for cumulative link models, we have predicted values for each response
    # category. Hence, gather columns

    data_grid <- .gather(data_grid, names_to = "response.level", values_to = "predicted", resp.names)
    data_grid$predicted <- link_inverse(data_grid$predicted)
    if (is.matrix(data_grid$predicted)) data_grid$predicted <- as.vector(data_grid$predicted[, 2])

    if (se) {
      d1 <- data.frame(ci.low = prdat$fitted.values - tcrit * prdat$se.fit)
      d2 <- data.frame(ci.high = prdat$fitted.values + tcrit * prdat$se.fit)
      d3 <- data.frame(se = prdat$se.fit)
      colnames(d1) <- sprintf("ci_low_%s", resp.names)
      colnames(d2) <- sprintf("ci_high_%s", resp.names)
      colnames(d3) <- sprintf("se_%s", resp.names)

      dat1 <- .gather(d1, names_to = "response.level", values_to = "conf.low")
      dat2 <- .gather(d2, names_to = "response.level", values_to = "conf.high")
      dat3 <- .gather(d3, names_to = "response.level", values_to = "se")

      data_grid$conf.low <- link_inverse(dat1$conf.low)
      data_grid$conf.high <- link_inverse(dat2$conf.high)

      if (is.matrix(data_grid$conf.low)) data_grid$conf.low <- as.vector(data_grid$conf.low[, 2])
      if (is.matrix(data_grid$conf.high)) data_grid$conf.high <- as.vector(data_grid$conf.high[, 2])

      attr(data_grid, "std.error") <- dat3$se
      data_grid$response.level <- sprintf("P[Y >= %s]", data_grid$response.level)
    }
  } else {
    # start here for other models
    prdat$fitted.values <- as.vector(prdat$fitted.values)
    data_grid$predicted <- suppressWarnings(link_inverse(prdat$fitted.values))

    # did user request standard errors? if yes, compute CI
    if (se) {
      # calculate CI
      data_grid$conf.low <- suppressWarnings(link_inverse(prdat$fitted.values - tcrit * prdat$se.fit))
      data_grid$conf.high <- suppressWarnings(link_inverse(prdat$fitted.values + tcrit * prdat$se.fit))
    } else {
      # no CI
      data_grid$conf.low <- NA
      data_grid$conf.high <- NA
    }
  }

  data_grid
}
