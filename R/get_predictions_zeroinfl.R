#' @importFrom stats qlogis predict qnorm
#' @importFrom dplyr case_when select
get_predictions_zeroinfl <- function(model, fitfram, ci.lvl, linv, type, fun, typical, terms, vcov.fun, vcov.type, vcov.args, condition, ...) {
  # get prediction type.
  pt <- dplyr::case_when(
    fun == "zeroinfl" && type == "fe" ~ "count",
    fun == "zeroinfl" && type == "fe.zi" ~ "response",
    fun == "zerotrunc" && type == "fe" ~ "count",
    fun == "zerotrunc" && type == "fe.zi" ~ "response",
    fun == "hurdle" && type == "fe" ~ "count",
    fun == "hurdle" && type == "fe.zi" ~ "response",
    TRUE ~ "response"
  )

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975


  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

  if ("nsim" %in% names(add.args))
    nsim <- eval(add.args[["nsim"]])
  else
    nsim <- 1000


  # get predictions
  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = pt,
      ...
    )

  # need back-transformation
  fitfram$predicted <- log(as.vector(prdat))


  if (type == "fe.zi") {

    mf <- insight::get_data(model)
    clean_terms <- get_clear_vars(terms)

    newdata <- get_expanded_data(
      model,
      mf,
      terms,
      typ.fun = typical,
      fac.typical = FALSE,
      pretty.message = FALSE,
      condition = condition
    )

    prdat.sim <- get_zeroinfl_predictions(model, newdata, nsim, terms, typical, condition)

    if (is.null(prdat.sim) || inherits(prdat.sim, c("error", "simpleError"))) {

      insight::print_color("red", "Error: Confidence intervals could not be computed.\n")
      cat("Possibly a polynomial term is held constant (and does not appear in the `terms`-argument). Or try reducing number of simulation, using argument `nsim` (e.g. `nsim = 100`).\n")

      fitfram$predicted <- as.vector(prdat)
      fitfram$conf.low <- NA
      fitfram$conf.high <- NA

    } else {

      sims <- exp(prdat.sim$cond) * (1 - stats::plogis(prdat.sim$zi))
      fitfram <- get_zeroinfl_fitfram(fitfram, newdata, as.vector(prdat), sims, ci, clean_terms)

      if (obj_has_name(fitfram, "std.error")) {
        # copy standard errors
        attr(fitfram, "std.error") <- fitfram$std.error
        fitfram <- dplyr::select(fitfram, -.data$std.error)
      }

    }

  } else {

    # get standard errors from variance-covariance matrix
    se.pred <-
      get_se_from_vcov(
        model = model,
        fitfram = fitfram,
        typical = typical,
        type = type,
        terms = terms,
        fun = fun,
        vcov.fun = vcov.fun,
        vcov.type = vcov.type,
        vcov.args = vcov.args,
        condition = condition
      )


    if (!is.null(se.pred)) {

      se.fit <- se.pred$se.fit
      fitfram <- se.pred$fitfram

      # CI
      fitfram$conf.low <- linv(fitfram$predicted - stats::qnorm(ci) * se.fit)
      fitfram$conf.high <- linv(fitfram$predicted + stats::qnorm(ci) * se.fit)

      # copy standard errors
      attr(fitfram, "std.error") <- se.fit

    } else {
      # CI
      fitfram$conf.low <- NA
      fitfram$conf.high <- NA
    }

    fitfram$predicted <- linv(fitfram$predicted)

  }

  fitfram
}
