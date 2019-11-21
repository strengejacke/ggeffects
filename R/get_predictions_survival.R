get_predictions_survival <- function(model, fitfram, ci.lvl, type, terms, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package `survival` required. Please install it.", call. = FALSE)
  }

  # get survial probabilities and cumulative hazards

  prdat <- survival::survfit(
    model,
    newdata = fitfram,
    se.fit = TRUE,
    conf.int = ci,
    ...
  )

  # check what user requested and either returnd surv probs
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

  purrr::map_df(sjmisc::seq_row(ff), function(i) {

    dat <- data.frame(
      time = prdat$time,
      predicted = pr[, i],
      conf.low = lower[, i],
      conf.high = upper[, i]
    )

    dat2 <- lapply(sjmisc::seq_col(ff), function(.x) ff[i, .x])
    names(dat2) <- clean_terms
    dat2 <- data.frame(dat2, stringsAsFactors = FALSE)

    cbind(dat[, 1, drop = FALSE], dat2, dat[, 2:4])
  })
}
