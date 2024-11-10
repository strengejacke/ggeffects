#' @export
get_predictions.coxph <- function(model,
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
  if (type %in% c("survival", "cumulative_hazard")) {
    get_predictions_survival(model, data_grid, ci_level, type, terms, ...)
  } else {
    get_predictions_coxph(
      model, data_grid = data_grid, ci_level = ci_level, typical = typical,
      terms = terms, vcov = vcov, vcov_args = vcov_args, condition = condition,
      interval = interval, verbose = verbose, ...)
  }
}


get_predictions_survival <- function(model, data_grid, ci_level, type, terms, ...) {
  # does user want standard errors?
  se <- !is.null(ci_level) && !is.na(ci_level)

  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level))
    ci <- (1 + ci_level) / 2
  else
    ci <- 0.975

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  insight::check_if_installed("survival")

  # get survial probabilities and cumulative hazards

  prdat <- survival::survfit(
    model,
    newdata = data_grid,
    se.fit = TRUE,
    conf.int = ci,
    ...
  )

  # check what user requested and either return surv probs
  # or cumulative hazards, including CI

  if (type == "survival") {
    pr <- prdat$surv
    lower <- prdat$lower
    upper <- prdat$upper
  } else {
    pr <- prdat$cumhaz
    lower <- pr - tcrit * prdat$std.err
    upper <- pr + tcrit * prdat$std.err
    # ugly fix...
    pr[which(pr < 0)] <- 0
    lower[which(lower < 0)] <- 0
    upper[which(upper < 0)] <- 0
    # copy standard errors
    attr(data_grid, "std.error") <- prdat$std.err
  }

  # Now we need the groups, as survfit() only returns numeric indices

  clean_terms <- .clean_terms(terms)
  ff <- data_grid[clean_terms]

  out <- do.call(rbind, lapply(seq_len(nrow(ff)), function(i) {
    dat <- data.frame(
      time = prdat$time,
      predicted = pr[, i],
      conf.low = lower[, i],
      conf.high = upper[, i]
    )

    dat2 <- lapply(seq_len(ncol(ff)), function(.x) ff[i, .x])
    names(dat2) <- clean_terms
    dat2 <- data.frame(dat2, stringsAsFactors = FALSE)

    cbind(dat[, 1, drop = FALSE], dat2, dat[, 2:4])
  }))

  if (min(out$time, na.rm = TRUE) > 1) {
    predicted <- as.numeric(type == "survival")
    conf.low <- as.numeric(type == "survival")
    conf.high <- as.numeric(type == "survival")

    dat <- expand.grid(lapply(out[clean_terms], unique))
    names(dat) <- clean_terms

    out <- rbind(
      out,
      cbind(time = 1, dat, predicted = predicted,conf.low = conf.low, conf.high = conf.high)
    )
  }

  # sanity check - don't return NA
  out[stats::complete.cases(out), ]
}


get_predictions_coxph <- function(model,
                                  data_grid,
                                  ci_level,
                                  typical,
                                  terms,
                                  vcov,
                                  vcov_args,
                                  condition,
                                  interval,
                                  verbose = TRUE,
                                  ...) {
  # does user want standard errors?
  se <- !is.null(ci_level) && !is.na(ci_level)

  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- (1 + ci_level) / 2
  } else {
    ci <- 0.975
  }

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  prdat <- stats::predict(
    model,
    newdata = data_grid,
    type = "lp",
    se.fit = se,
    ...
  )

  # did user request standard errors? if yes, compute CI
  if (!is.null(vcov) || (!is.null(interval) && interval == "prediction")) {
    # copy predictions
    data_grid$predicted <- exp(prdat$fit)

    se.pred <- .standard_error_predictions(
      model = model,
      prediction_data = data_grid,
      typical = typical,
      terms = terms,
      vcov = vcov,
      vcov_args = vcov_args,
      condition = condition,
      interval = interval,
      verbose = verbose
    )

    if (.check_returned_se(se.pred)) {
      se.fit <- se.pred$se.fit
      data_grid <- se.pred$prediction_data

      # CI
      data_grid$conf.low <- data_grid$predicted - tcrit * se.fit
      data_grid$conf.high <- data_grid$predicted + tcrit * se.fit

      # copy standard errors
      attr(data_grid, "std.error") <- se.fit
      attr(data_grid, "prediction.interval") <- attr(se.pred, "prediction_interval")
    } else {
      # CI
      data_grid$conf.low <- NA
      data_grid$conf.high <- NA
    }
  } else if (se) {
    # copy predictions
    data_grid$predicted <- exp(prdat$fit)

    # calculate CI
    data_grid$conf.low <- exp(prdat$fit - tcrit * prdat$se.fit)
    data_grid$conf.high <- exp(prdat$fit + tcrit * prdat$se.fit)

    # copy standard errors
    attr(data_grid, "std.error") <- prdat$se.fit
  } else {
    # copy predictions
    data_grid$predicted <- exp(as.vector(prdat))

    # no CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  data_grid
}
