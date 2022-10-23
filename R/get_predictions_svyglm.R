get_predictions_svyglm <- function(model, fitfram, ci.lvl, linv, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- 0.975

  # get predictions
  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "link",
      se.fit = se,
      ...
    )

  # check if user wants standard errors
  if (se) {
    # get variance matrix for standard errors. "survey" stores the information
    # somewhat different from classical predict function
    vv <- attr(prdat, "var")

    # compute standard errors
    if (is.matrix(vv))
      prdat <- as.data.frame(cbind(prdat, sqrt(diag(vv))))
    else
      prdat <- as.data.frame(cbind(prdat, sqrt(vv)))

    # consistent column names
    colnames(prdat) <- c("fit", "se.fit")

    # copy predictions
    fitfram$predicted <- linv(prdat$fit)

    # calculate CI
    fitfram$conf.low <- linv(prdat$fit - stats::qnorm(ci) * prdat$se.fit)
    fitfram$conf.high <- linv(prdat$fit + stats::qnorm(ci) * prdat$se.fit)

    # copy standard errors
    attr(fitfram, "std.error") <- prdat$se.fit

  } else {
    # copy predictions
    fitfram$predicted <- linv(as.vector(prdat))

    # no CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}
