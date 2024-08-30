.ggemmeans_add_confint <- function(model, tmp, ci.lvl, type = "fixed", pmode = NULL, interval = NULL) {
  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl)) {
    ci <- (1 + ci.lvl) / 2
  } else {
    ci <- 0.975
  }

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  if (type %in% c("random", "zero_inflated_random") || identical(interval, "prediction")) {

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
        fitfram$conf.low <- exp(lf(fitfram$conf.low) - tcrit * sqrt(revar))
        fitfram$conf.high <- exp(lf(fitfram$conf.high) + tcrit * sqrt(revar))
      } else {
        fitfram$conf.low <- fitfram$conf.low - tcrit * sqrt(revar)
        fitfram$conf.high <- fitfram$conf.high + tcrit * sqrt(revar)
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
    fitfram$conf.low <- stats::plogis(lf(fitfram$predicted) - tcrit * fitfram$std.error)
    fitfram$conf.high <- stats::plogis(lf(fitfram$predicted) + tcrit * fitfram$std.error)
    fitfram
  } else {
    fitfram <- suppressWarnings(
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
    # edge case: for models with inverse-link, we need to "switch" CIs
    if (identical(insight::model_info(model)$link_function, "inverse")) {
      ci_low <- fitfram$conf.high
      fitfram$conf.high <- fitfram$conf.low
      fitfram$conf.low <- ci_low
    }
    fitfram
  }
}
