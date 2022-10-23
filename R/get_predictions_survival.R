get_predictions_survival <- function(model, fitfram, ci.lvl, type, terms, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  insight::check_if_installed("survival")

  # get survial probabilities and cumulative hazards

  prdat <- survival::survfit(
    model,
    newdata = fitfram,
    se.fit = TRUE,
    conf.int = ci,
    ...
  )

  # check what user requested and either return surv probs
  # or cumulative hazards, including CI

  if (type == "surv") {
    pr <- prdat$surv
    lower <- prdat$lower
    upper <- prdat$upper
  } else {
    pr <- prdat$cumhaz
    lower <- pr - stats::qnorm(ci) * prdat$std.err
    upper <- pr + stats::qnorm(ci) * prdat$std.err
    # ugly fix...
    pr[which(pr < 0)] <- 0
    lower[which(lower < 0)] <- 0
    upper[which(upper < 0)] <- 0
    # copy standard errors
    attr(fitfram, "std.error") <- prdat$std.err
  }

  # Now we need the groups, as survfit() only returns numeric indices

  clean_terms <- .clean_terms(terms)
  ff <- fitfram[clean_terms]

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
    time <- 1
    predicted <- ifelse(type == "surv", 1, 0)
    conf.low <- ifelse(type == "surv", 1, 0)
    conf.high <- ifelse(type == "surv", 1, 0)

    dat <- expand.grid(lapply(out[clean_terms], unique))
    names(dat) <- clean_terms

    out <- rbind(
      out,
      cbind(time = 1, dat, predicted = predicted,
            conf.low = conf.low, conf.high = conf.high)
    )
  }

  # sanity check - don't return NA
  out[stats::complete.cases(out), ]
}
