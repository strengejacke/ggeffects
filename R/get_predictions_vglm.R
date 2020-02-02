#' @importFrom insight model_info get_response
get_predictions_vglm <- function(model, fitfram, ci.lvl, linv, ...) {

  if (!requireNamespace("VGAM", quietly = TRUE)) {
    stop("Package `VGAM` needed to calculate marginal effects for a vector generalized linear model.", call. = FALSE)
  }

  se <- !is.null(ci.lvl) && !is.na(ci.lvl)
  mi <- insight::model_info(model)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  if ((mi$is_ordinal || mi$is_multinomial) && !isTRUE(se)) {
    type <- "response"
  } else {
    type <- "link"
  }

  prdat <- VGAM::predictvglm(
    model,
    newdata = fitfram,
    type = type,
    se.fit = se,
    ...
  )

  if (mi$is_ordinal || mi$is_multinomial) {
    # start here with cumulative link models
    resp <- insight::get_response(model)

    if (is.data.frame(resp))
      resp.names <- colnames(resp)
    else
      resp.names <- levels(resp)

    if (se) {
      dat <- data.frame(predicted = prdat$fitted.values)
      resp.names <- resp.names[-1]
    } else {
      dat <- data.frame(predicted = prdat)
      linv <- function(mu) mu
    }

    colnames(dat) <- resp.names
    fitfram <- cbind(dat, fitfram)

    # for cumulative link models, we have predicted values for each response
    # category. Hence, gather columns

    fitfram <- .gather(fitfram, names_to = "response.level", values_to = "predicted", resp.names)
    fitfram$predicted <- linv(fitfram$predicted)
    if (is.matrix(fitfram$predicted)) fitfram$predicted <- as.vector(fitfram$predicted[, 2])

    if (se) {
      d1 <- data.frame(ci.low = prdat$fitted.values - stats::qnorm(ci) * prdat$se.fit)
      d2 <- data.frame(ci.high = prdat$fitted.values + stats::qnorm(ci) * prdat$se.fit)
      d3 <- data.frame(se = prdat$se.fit)
      colnames(d1) <- sprintf("ci_low_%s", resp.names)
      colnames(d2) <- sprintf("ci_high_%s", resp.names)
      colnames(d3) <- sprintf("se_%s", resp.names)

      dat1 <- .gather(d1, names_to = "response.level", values_to = "conf.low")
      dat2 <- .gather(d2, names_to = "response.level", values_to = "conf.high")
      dat3 <- .gather(d3, names_to = "response.level", values_to = "se")

      fitfram$conf.low <- linv(dat1$conf.low)
      fitfram$conf.high <- linv(dat2$conf.high)

      if (is.matrix(fitfram$conf.low)) fitfram$conf.low <- as.vector(fitfram$conf.low[, 2])
      if (is.matrix(fitfram$conf.high)) fitfram$conf.high <- as.vector(fitfram$conf.high[, 2])

      attr(fitfram, "std.error") <- dat3$se
      fitfram$response.level <- sprintf("P[Y >= %s]", fitfram$response.level)
    }
  } else {
    # start here for other models
    prdat$fitted.values <- as.vector(prdat$fitted.values)
    fitfram$predicted <- suppressWarnings(linv(prdat$fitted.values))

    # did user request standard errors? if yes, compute CI
    if (se) {
      # calculate CI
      fitfram$conf.low <- suppressWarnings(linv(prdat$fitted.values - stats::qnorm(ci) * prdat$se.fit))
      fitfram$conf.high <- suppressWarnings(linv(prdat$fitted.values + stats::qnorm(ci) * prdat$se.fit))
    } else {
      # no CI
      fitfram$conf.low <- NA
      fitfram$conf.high <- NA
    }
  }

  fitfram
}
