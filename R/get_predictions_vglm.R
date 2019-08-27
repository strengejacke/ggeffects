#' @importFrom insight model_info find_response
get_predictions_vglm <- function(model, fitfram, ci.lvl, linv, ...) {

  if (!requireNamespace("VGAM", quietly = TRUE)) {
    stop("Package `VGAM` needed to calculate marginal effects for a vector generalized linear model.", call. = FALSE)
  }

  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  prdat <- VGAM::predictvglm(
    model,
    newdata = fitfram,
    type = "link",
    se.fit = se,
    ...
  )

  if (insight::model_info(model)$is_ordinal) {
    # start here with cumulative link models
    resp.names <- insight::find_response(model, combine = FALSE)

    if (se) {
      dat <- data.frame(predicted = prdat$fitted.values)
    } else {
      dat <- data.frame(predicted = prdat)
    }

    colnames(dat) <- resp.names[-1]
    fitfram <- cbind(dat, fitfram)

    # for cumulative link models, we have predicted values for each response
    # category. Hence, gather columns

    fitfram <- .gather(fitfram, "response.level", "predicted", resp.names[-1])
    fitfram$predicted <- linv(fitfram$predicted)
    if (is.matrix(fitfram$predicted)) fitfram$predicted <- as.vector(fitfram$predicted[, 2])

    if (se) {
      d1 <- data.frame(ci.low = prdat$fitted.values - stats::qnorm(ci) * prdat$se.fit)
      d2 <- data.frame(ci.high = prdat$fitted.values + stats::qnorm(ci) * prdat$se.fit)
      d3 <- data.frame(se = prdat$se.fit)
      colnames(d1) <- sprintf("ci_low_%s", resp.names[-1])
      colnames(d2) <- sprintf("ci_high_%s", resp.names[-1])
      colnames(d3) <- sprintf("se_%s", resp.names[-1])

      dat1 <- .gather(d1, "response.level", "conf.low")
      dat2 <- .gather(d2, "response.level", "conf.high")
      dat3 <- .gather(d3, "response.level", "se")

      fitfram$conf.low <- linv(dat1$conf.low)
      fitfram$conf.high <- linv(dat2$conf.high)

      if (is.matrix(fitfram$conf.low)) fitfram$conf.low <- as.vector(fitfram$conf.low[, 2])
      if (is.matrix(fitfram$conf.high)) fitfram$conf.high <- as.vector(fitfram$conf.high[, 2])

      attr(fitfram, "std.error") <- dat3$se
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
