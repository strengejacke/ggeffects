get_predictions_gam <- function(model, data_grid, ci.lvl, linv, type, ...) {
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- 0.975

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  mi <- insight::model_info(model)

  if (!mi$is_zero_inflated && type %in% c("fe.zi", "re.zi")) {
    type <- "fe"
    insight::format_alert(sprintf("Model has no zero-inflation part. Changing prediction-type to \"%s\".", type))
  }


  prdat <- stats::predict(
    model,
    newdata = data_grid,
    type = "link",
    se.fit = se
  )


  if (type == "fe.zi") {

    # check if number of simulations was provided

    add.args <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)

    if ("nsim" %in% names(add.args))
      nsim <- eval(add.args[["nsim"]])
    else
      nsim <- 1000


    # simulate predictions, for standad errors / CI

    prdat.sim <- .get_zeroinfl_gam_predictions(model = model, newdata = data_grid, nsim = nsim)


    # make sure we have only predicted values as vector, no SE

    if (.obj_has_name(prdat, "fit")) {
      prdat <- list(
        cond = as.vector(prdat$fit[, 1]),
        zi = as.vector(prdat$fit[, 2])
      )
    } else {
      prdat <- list(
        cond = as.vector(prdat[, 1]),
        zi = as.vector(prdat[, 2])
      )
    }

    prdat <- as.vector(exp(prdat$cond) * (1 - stats::plogis(prdat$zi)))


    # success?

    if (is.null(prdat.sim) || inherits(prdat.sim, c("error", "simpleError"))) {

      insight::print_color("Error: Confidence intervals could not be computed.\n", "red")

      data_grid$predicted <- prdat
      data_grid$conf.low <- NA
      data_grid$conf.high <- NA

    } else {

      sims <- exp(prdat.sim$cond) * (1 - stats::plogis(prdat.sim$zi))

      data_grid$predicted <- prdat
      data_grid$std.error <- apply(sims, 1, stats::sd)

      conf.low <- apply(sims, 1, stats::quantile, probs = 1 - ci)
      conf.high <- apply(sims, 1, stats::quantile, probs = ci)
      ci.range <- (conf.high - conf.low) / 2

      # fix negative CI
      ci.low <- data_grid$predicted - ci.range
      neg.ci <- ci.low < 0
      if (any(neg.ci)) {
        ci.range[neg.ci] <- ci.range[neg.ci] - abs(ci.low[neg.ci]) - 1e-05
        data_grid$std.error[neg.ci] <- data_grid$std.error[neg.ci] - ((abs(ci.low[neg.ci]) + 1e-05) / tcrit)
      }

      data_grid$conf.low <- data_grid$predicted - ci.range
      data_grid$conf.high <- data_grid$predicted + ci.range

      if (.obj_has_name(data_grid, "std.error")) {
        # copy standard errors
        attr(data_grid, "std.error") <- data_grid$std.error
        data_grid <- .remove_column(data_grid, "std.error")
      }
    }

  } else {

    if (mi$is_zero_inflated) {
      if (.obj_has_name(prdat, "fit")) {
        prdat$fit <- as.vector(prdat$fit[, 1])
        prdat$se.fit <- as.vector(prdat$se.fit[, 1])
      } else {
        prdat <- as.vector(prdat[, 1])
      }
      linv <- exp
    }

    # did user request standard errors? if yes, compute CI
    if (se) {
      # copy predictions
      data_grid$predicted <- linv(prdat$fit)

      # calculate CI
      data_grid$conf.low <- linv(prdat$fit - tcrit * prdat$se.fit)
      data_grid$conf.high <- linv(prdat$fit + tcrit * prdat$se.fit)

      # copy standard errors
      attr(data_grid, "std.error") <- prdat$se.fit

    } else {
      # copy predictions
      data_grid$predicted <- linv(as.vector(prdat))

      # no CI
      data_grid$conf.low <- NA
      data_grid$conf.high <- NA
    }

    data_grid
  }
}
