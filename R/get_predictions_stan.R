get_predictions_stan <- function(model, data_grid, ci.lvl, type, model_info, ppd, terms = NULL, verbose = TRUE, ...) {
  # check if pkg is available
  insight::check_if_installed("rstantools")

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
    mf <- insight::get_data(model, source = "frame", verbose = FALSE)
    vo <- names(which(vapply(mf, is.ordered, logical(1))))
    fac2ord <- which(terms %in% vo)

    if (!.is_empty(fac2ord)) {
      for (i in fac2ord) data_grid[[terms[i]]] <- as.ordered(data_grid[[terms[i]]])
    }
  }


  # compute posterior predictions
  if (ppd) {
    # for binomial models, "newdata" also needs a response
    # value. we take the value for a successful event
    if (model_info$is_binomial) {
      resp.name <- insight::find_response(model)
      resp.value <- insight::get_response(model)
      # successful events
      if (is.factor(resp.value)) {
        data_grid[[resp.name]] <- levels(resp.value)[2]
      } else {
        data_grid[[resp.name]] <- unique(resp.value)[2]
      }
    }

    prdat2 <- prdat <- rstantools::posterior_predict(
      model,
      newdata = data_grid,
      re.form = ref,
      ...
    )

  } else {
    # get posterior distribution of the linear predictor
    # note that these are not best practice for inferences,
    # because they don't take the measurement error into account
    prdat <- rstantools::posterior_epred(
      model,
      newdata = data_grid,
      re.form = ref,
      re_formula = ref,
      ...
    )

    if (model_info$is_mixed && verbose) {
      # tell user
      insight::format_alert("Note: uncertainty of error terms are not taken into account. You may want to use `rstantools::posterior_predict()`.")
    }
  }

  # we have a list of 4000 samples, so we need to coerce to data frame
  prdat <- as.data.frame(prdat)


  # handle cumulative link models

  if (inherits(model, "brmsfit") && model_info$family %in% c("cumulative", "categorical")) {

    tmp <- as.data.frame(lapply(prdat, stats::median))
    tmp <- .gather(tmp, names_to = "grp", values_to = "predicted")

    rownames(tmp) <- NULL
    tmp$grp <- gsub("X", "", tmp$grp, fixed = TRUE)

    resp <- insight::get_response(model)
    if (is.data.frame(resp))
      resp <- resp[[1]] # Model must have been using weights

    # Response could be a factor or numeric
    if (is.factor(resp))
      resp.vals <- levels(resp)
    else
      resp.vals <- sort(unique(resp))

    term.cats <- nrow(data_grid)

    data_grid <- do.call(rbind, rep(list(data_grid), time = length(resp.vals)))
    data_grid$response.level <- rep(unique(resp.vals), each = term.cats)
    data_grid$predicted <- tmp$predicted

  } else if (insight::is_multivariate(model)) {

    # handle multivariate response models

    tmp <- as.data.frame(lapply(prdat, stats::median))
    tmp <- .gather(tmp, names_to = "grp", values_to = "predicted")

    rownames(tmp) <- NULL
    tmp$grp <- gsub("X", "", tmp$grp, fixed = TRUE)

    resp.vars <- insight::find_response(model, combine = FALSE)

    # exclude weighting variables
    resp.weights <- unique(insight::find_weights(model))

    if (!is.null(resp.weights)) {
      resp.vars <- setdiff(resp.vars, resp.weights)
    }

    data_grid <- do.call(rbind, rep(list(data_grid), time = length(resp.vars)))
    data_grid$response.level <- ""

    for (i in resp.vars) {
      pos <- string_ends_with(pattern = i, x = tmp$grp)

      if (.is_empty(pos)) {
        i <- gsub(pattern = "[\\_\\.]", replacement = "", x = i)
        # same as
        # i <- gsub(pattern = "(\\_|\\.)", replacement = "", x = i)
        pos <- string_ends_with(pattern = i, x = tmp$grp)
      }

      data_grid$response.level[pos] <- i
    }

    data_grid$predicted <- tmp$predicted

  } else {
    # compute median, as "most probable estimate"
    data_grid$predicted <- vapply(prdat, stats::median, numeric(1))
  }


  # for posterior predictive distributions, we compute
  # the predictive intervals

  if (ppd) {

    # for multivariate response models, we have an array
    # instead of matrix - get CIs for each response

    if (inherits(prdat2, "array")) {
      if (length(dim(prdat2)) == 3) {
        tmp <- do.call(rbind, lapply(seq_len(dim(prdat2)[3]), function(.x) {
          as.data.frame(rstantools::posterior_interval(as.matrix(prdat2[, , .x]), prob = ci.lvl))
        }))
      } else {
        tmp <- as.data.frame(rstantools::posterior_interval(prdat2), prob = ci.lvl)
      }
    } else {
      tmp <- rstantools::posterior_interval(prdat2, prob = ci.lvl)
    }
  } else {
    tmp <- rstantools::posterior_interval(as.matrix(prdat), prob = ci.lvl)
  }

  predint <- list(
    tmp[, 1],
    tmp[, 2]
  )

  if (se) {
    # bind predictive intervals int
    data_grid$conf.low <- predint[[1]]
    data_grid$conf.high <- predint[[2]]
  } else {
    # no CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  data_grid
}
