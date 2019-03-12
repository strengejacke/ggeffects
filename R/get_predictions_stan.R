#' @importFrom sjmisc rotate_df
#' @importFrom purrr map_dbl map_df
#' @importFrom dplyr bind_cols select bind_rows n_distinct
#' @importFrom stats median formula
get_predictions_stan <- function(model, fitfram, ci.lvl, type, faminfo, ppd, terms = NULL, ...) {
  # check if pkg is available
  if (!requireNamespace("rstantools", quietly = TRUE)) {
    stop("Package `rstantools` is required to compute predictions.", call. = F)
  }

  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # check whether predictions should be conditioned
  # on random effects (grouping level) or not.
  if (type != "fe")
    ref <- NULL
  else
    ref <- NA


  # check if we have terms that are ordinal, and if so,
  # convert factors to ordinal in "newdata" to allow
  # predictions for monotonic models

  if (!is.null(terms)) {
    mf <- insight::get_data(model)
    vo <- colnames(dplyr::select_if(mf, is.ordered))
    fac2ord <- which(terms %in% vo)

    if (!sjmisc::is_empty(fac2ord)) {
      for (i in fac2ord) fitfram[[terms[i]]] <- as.ordered(fitfram[[terms[i]]])
    }
  }


  # compute posterior predictions
  if (ppd) {
    # for binomial models, "newdata" also needs a response
    # value. we take the value for a successful event
    if (faminfo$is_binomial) {
      resp.name <- insight::find_response(model)
      # successfull events
      fitfram[[resp.name]] <- factor(1)
    }

    prdat2 <- prdat <- rstantools::posterior_predict(
      model,
      newdata = fitfram,
      re.form = ref,
      ...
    )

  } else {
    # get posterior distribution of the linear predictor
    # note that these are not best practice for inferences,
    # because they don't take the measurement error into account
    prdat <- rstantools::posterior_linpred(
      model,
      newdata = fitfram,
      transform = TRUE,
      re.form = ref,
      re_formula = ref,
      ...
    )

    if (faminfo$is_mixed) {
      # tell user
      message("Note: uncertainty of error terms are not taken into account. You may want to use `rstantools::posterior_predict()`.")
    }
  }

  # we have a list of 4000 samples, so we need to coerce to data frame
  prdat <- as.data.frame(prdat)


  # handle cumulative link models

  if (inherits(model, "brmsfit") && faminfo$family %in% c("cumulative", "categorical")) {

    tmp <- prdat %>%
      purrr::map_df(stats::median) %>%
      .gather(key = "grp", value = "predicted")

    resp.vals <- levels(insight::get_response(model))
    term.cats <- nrow(fitfram)
    fitfram <- purrr::map_df(1:length(resp.vals), ~ fitfram)

    fitfram$response.level <- rep(unique(resp.vals), each = term.cats)
    fitfram$predicted <- tmp$predicted

  } else if (insight::is_multivariate(model)) {

    # handle multivariate response models

    tmp <- prdat %>%
      purrr::map_df(stats::median) %>%
      .gather(key = "grp", value = "predicted")

    resp.vars <- insight::find_response(model, combine = FALSE)
    fitfram <- purrr::map_df(1:length(resp.vars), ~ fitfram)
    fitfram$response.level <- ""

    for (i in resp.vars) {
      pos <- string_ends_with(pattern = i, x = tmp$grp)

      if (sjmisc::is_empty(pos)) {
        i <- gsub(pattern = "[\\_\\.]", replacement = "", x = i)
        # same as
        # i <- gsub(pattern = "(\\_|\\.)", replacement = "", x = i)
        pos <- string_ends_with(pattern = i, x = tmp$grp)
      }

      fitfram$response.level[pos] <- i
    }

    fitfram$predicted <- tmp$predicted

  } else {
    # compute median, as "most probable estimate"
    fitfram$predicted <- purrr::map_dbl(prdat, stats::median)
  }


  # for posterior predictive distributions, we compute
  # the predictive intervals

  if (ppd) {

    # for multivariate reponse models, we have an array
    # instead of matrix - get CIs for each response

    if (inherits(prdat2, "array")) {
      tmp <- purrr::map_df(1:dim(prdat2)[3], function(.x) {
        as.data.frame(rstantools::predictive_interval(as.matrix(prdat2[, , .x]), prob = ci.lvl))
      })
    } else {
      tmp <- rstantools::predictive_interval(prdat2, prob = ci.lvl)
    }
  } else {
    tmp <- rstantools::predictive_interval(as.matrix(prdat), prob = ci.lvl)
  }

  hdi <- list(
    tmp[, 1],
    tmp[, 2]
  )

  if (se) {
    # bind HDI
    fitfram$conf.low <- hdi[[1]]
    fitfram$conf.high <- hdi[[2]]
  } else {
    # no CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}
