#' @importFrom stats confint plogis
.ggemmeans_add_confint <- function(model, tmp, ci.lvl, type = "fe", pmode = NULL) {
  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  if (type %in% c("re", "re.zi")) {

      fitfram <- suppressWarnings(
        .var_rename(
          as.data.frame(tmp),
          SE = "std.error",
          emmean = "predicted",
          lower.CL = "conf.low",
          upper.CL = "conf.high",
          prob = "predicted",
          asymp.LCL = "conf.low",
          asymp.UCL = "conf.high",
          lower.HPD = "conf.low",
          upper.HPD = "conf.high"
        )
      )

    revar <- .get_residual_variance(model)
    # get link-function and back-transform fitted values
    # to original scale, so we compute proper CI
    if (!is.null(revar)) {
      if (!is.null(pmode) && pmode %in% c("prob", "count")) {
        lf <- insight::link_function(model)
        fitfram$conf.low <- exp(lf(fitfram$conf.low) - stats::qnorm(ci) * sqrt(revar))
        fitfram$conf.high <- exp(lf(fitfram$conf.high) + stats::qnorm(ci) * sqrt(revar))
      } else {
        fitfram$conf.low <- fitfram$conf.low - stats::qnorm(ci) * sqrt(revar)
        fitfram$conf.high <- fitfram$conf.high + stats::qnorm(ci) * sqrt(revar)
      }
      fitfram$std.error <- sqrt(fitfram$std.error^2 + revar)
    }
    fitfram
  } else if (inherits(model, "multinom")) {
    fitfram <- suppressWarnings(
      .var_rename(
        as.data.frame(tmp),
        SE = "std.error",
        emmean = "predicted",
        lower.CL = "conf.low",
        upper.CL = "conf.high",
        prob = "predicted",
        asymp.LCL = "conf.low",
        asymp.UCL = "conf.high",
        lower.HPD = "conf.low",
        upper.HPD = "conf.high"
      )
    )
    lf <- insight::link_function(model)
    fitfram$conf.low <- stats::plogis(lf(fitfram$predicted) - stats::qnorm(ci) * fitfram$std.error)
    fitfram$conf.high <- stats::plogis(lf(fitfram$predicted) + stats::qnorm(ci) * fitfram$std.error)
    fitfram
  } else {
    suppressWarnings(
      .var_rename(
        as.data.frame(stats::confint(tmp, level = ci.lvl)),
        SE = "std.error",
        emmean = "predicted",
        lower.CL = "conf.low",
        upper.CL = "conf.high",
        prob = "predicted",
        asymp.LCL = "conf.low",
        asymp.UCL = "conf.high",
        lower.HPD = "conf.low",
        upper.HPD = "conf.high"
      )
    )
  }
}
