.join_simulations <- function(prediction_data, newdata, prdat, sims, ci, clean_terms) {
  # after "bootstrapping" confidence intervals by simulating from the
  # multivariate normal distribution, we need to prepare the data and
  # calculate "bootstrapped" estimates and CIs.

  prediction_data$sort__id <- seq_len(nrow(prediction_data))
  column_matches <- sapply(colnames(prediction_data), function(.x) any(unique(prediction_data[[.x]]) %in% newdata[[.x]]))

  # we need two data grids here: one for all combination of levels from the
  # model predictors ("newdata"), and one with the current combinations only
  # for the terms in question ("prediction_data"). "sims" has always the same
  # number of rows as "newdata", but "prediction_data" might be shorter. So we
  # merge "prediction_data" and "newdata", add mean and quantiles from "sims"
  # as new variables, and then later only keep the original observations
  # from "prediction_data" - by this, we avoid unequal row-lengths.

  join_by <- colnames(prediction_data)[column_matches]
  prediction_data <- merge(newdata, prediction_data, by = join_by, all = TRUE, sort = FALSE)

  prediction_data$predicted <- apply(sims, 1, mean)
  prediction_data$conf.low <- apply(sims, 1, stats::quantile, probs = 1 - ci)
  prediction_data$conf.high <- apply(sims, 1, stats::quantile, probs = ci)
  prediction_data$std.error <- apply(sims, 1, stats::sd)

  # group_by() changes the order of rows / variables in "prediction_data", however
  # we later add back the original predictions "prdat" (see below), which
  # correspond to the *current* sorting of prediction_data. So we add a dummy-ID,
  # which we use to restore the original sorting of prediction_data later...


  # The following code is a replace for dplyr, when grouping and
  # summarizing data. previous code was:
  #
  # grp <- rlang::syms(clean_terms)
  # prediction_data <- prediction_data %>%
  #   dplyr::filter(!is.na(.data$sort__id)) %>%
  #   dplyr::group_by(!!! grp) %>%
  #   dplyr::summarize(
  #     predicted = mean(.data$predicted),
  #     conf.low = mean(.data$conf.low),
  #     conf.high = mean(.data$conf.high),
  #     std.error = mean(.data$std.error),
  #     id = .data$sort__id
  #   ) %>%
  #   dplyr::ungroup()

  prediction_data <- prediction_data[!is.na(prediction_data$sort__id), , drop = FALSE]

  prediction_data <- cbind(
    stats::aggregate(
      prediction_data[c("predicted", "conf.low", "conf.high", "std.error")],
      by = prediction_data[clean_terms],
      FUN = mean,
      na.rm = TRUE
    ),
    id = prediction_data$sort__id
  )

  rownames(prediction_data) <- NULL

  if (length(clean_terms) == 1) {
    prediction_data <- prediction_data[order(prediction_data[[1]]), , drop = FALSE]
  } else if (length(clean_terms) == 2) {
    prediction_data <- prediction_data[order(prediction_data[[1]], prediction_data[[2]]), , drop = FALSE]
  } else if (length(clean_terms) == 3) {
    prediction_data <- prediction_data[order(prediction_data[[1]], prediction_data[[2]], prediction_data[[3]]), , drop = FALSE]
  } else if (length(clean_terms) == 4) {
    prediction_data <- prediction_data[order(prediction_data[[1]], prediction_data[[2]], prediction_data[[3]], prediction_data[[4]]), , drop = FALSE]
  }

  # we use the predicted values from "predict(type = "reponse")", but the
  # bootstrapped CI - so we need to fix a bit here. "predict(type = "reponse")"
  # works as intended, but the related standard errors are not reliable (due
  # to the combination of the conditional and zero-inflated model), so we need
  # the simulated standard errors and CI's - but can use the "correct" predictions.
  # in order to make CI and predictions match, we take the simulated CI-range
  # and use the original predicted values as "center" for those CI-ranges.

  if (length(prdat) == nrow(prediction_data)) {
    prediction_data <- prediction_data[order(prediction_data$id), , drop = FALSE]
    ci.range <- (prediction_data$conf.high - prediction_data$conf.low) / 2
    prediction_data$predicted <- prdat

    # fix negative CI
    ci.low <- prediction_data$predicted - ci.range
    neg.ci <- ci.low < 0
    if (any(neg.ci)) {
      ci.range[neg.ci] <- ci.range[neg.ci] - abs(ci.low[neg.ci]) - 1e-05
      prediction_data$std.error[neg.ci] <- prediction_data$std.error[neg.ci] - ((abs(ci.low[neg.ci]) + 1e-05) / stats::qnorm(ci))
    }

    prediction_data$conf.low <- prediction_data$predicted - ci.range
    prediction_data$conf.high <- prediction_data$predicted + ci.range
    prediction_data <- .remove_column(prediction_data, "id")
  }

  prediction_data
}




.simulate_zi_predictions <- function(model,
                                     newdata,
                                     nsim = 1000,
                                     terms = NULL,
                                     value_adjustment = NULL,
                                     condition = NULL) {

  # Since the zero inflation and the conditional model are working in "opposite
  # directions", confidence intervals can not be derived directly  from the
  # "predict()"-function. Thus, confidence intervals for type = "fe.zi" are
  # based on quantiles of simulated draws from a multivariate normal distribution
  # (see also _Brooks et al. 2017, pp.391-392_ for details).

  if (inherits(model, "glmmTMB")) {
    .simulate_predictions_glmmTMB(model, newdata, nsim, terms, value_adjustment, condition)
  } else if (inherits(model, "MixMod")) {
    .simulate_predictions_MixMod(model, newdata, nsim, terms, value_adjustment, condition)
  } else {
    .simulate_predictions_zeroinfl(model, newdata, nsim, terms, value_adjustment, condition)
  }
}






.simulate_predictions_glmmTMB <- function(model,
                                          newdata,
                                          nsim,
                                          terms = NULL,
                                          value_adjustment = NULL,
                                          condition = NULL) {
  insight::check_if_installed("lme4")

  # Since the zero inflation and the conditional model are working in "opposite
  # directions", confidence intervals can not be derived directly  from the
  # "predict()"-function. Thus, confidence intervals for type = "fe.zi" are
  # based on quantiles of simulated draws from a multivariate normal distribution
  # (see also _Brooks et al. 2017, pp.391-392_ for details).

  tryCatch(
    {
      condformula <- lme4::nobars(stats::formula(model)[-2])
      ziformula <- lme4::nobars(stats::formula(model$modelInfo$allForm$ziformula))

      # if formula has a polynomial term, and this term is one that is held
      # constant, model.matrix() with "newdata" will throw an error - so we
      # re-build the newdata-argument by including all values for poly-terms, if
      # these are hold constant.

      fixes <- .rows_to_keep(model, newdata, condformula, ziformula, terms, value_adjustment, condition)

      if (!is.null(fixes)) {
        keep <- fixes$keep
        newdata <- fixes$newdata
      } else {
        keep <- NULL
      }

      x.cond <- stats::model.matrix(condformula, newdata)
      beta.cond <- lme4::fixef(model)$cond

      x.zi <- stats::model.matrix(ziformula, newdata)
      beta.zi <- lme4::fixef(model)$zi

      cond.varcov <- insight::get_varcov(model, component = "conditional")
      zi.varcov <- insight::get_varcov(model, component = "zero_inflated")

      pred.condpar.psim <- MASS::mvrnorm(n = nsim, mu = beta.cond, Sigma = cond.varcov)
      pred.cond.psim <- x.cond %*% t(pred.condpar.psim)
      pred.zipar.psim <- MASS::mvrnorm(n = nsim, mu = beta.zi, Sigma = zi.varcov)
      pred.zi.psim <- x.zi %*% t(pred.zipar.psim)

      if (!.is_empty(keep)) {
        pred.cond.psim <- pred.cond.psim[keep, , drop = FALSE]
        pred.zi.psim <- pred.zi.psim[keep, , drop = FALSE]
      }

      list(cond = pred.cond.psim, zi = pred.zi.psim)
    },
    error = function(x) { x },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )
}


.simulate_predictions_MixMod <- function(model,
                                         newdata,
                                         nsim,
                                         terms = NULL,
                                         value_adjustment = NULL,
                                         condition = NULL) {

  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("You need to install package `lme4` first to compute marginal effects.", call. = FALSE)
  }

  tryCatch(
    {
      condformula <- stats::formula(model, type = "fixed")
      ziformula <- stats::formula(model, type = "zi_fixed")

      # if formula has a polynomial term, and this term is one that is held
      # constant, model.matrix() with "newdata" will throw an error - so we
      # re-build the newdata-argument by including all values for poly-terms, if
      # these are hold constant.

      fixes <- .rows_to_keep(model, newdata, condformula, ziformula, terms, value_adjustment, condition)

      if (!is.null(fixes)) {
        keep <- fixes$keep
        newdata <- fixes$newdata
      } else {
        keep <- NULL
      }

      x.cond <- stats::model.matrix(condformula, newdata)
      beta.cond <- lme4::fixef(model, sub_model = "main")

      x.zi <- stats::model.matrix(ziformula, newdata)
      beta.zi <- lme4::fixef(model, sub_model = "zero_part")

      cond.varcov <- insight::get_varcov(model, component = "conditional")
      zi.varcov <- insight::get_varcov(model, component = "zero_inflated")

      pred.condpar.psim <- MASS::mvrnorm(n = nsim, mu = beta.cond, Sigma = cond.varcov)
      pred.cond.psim <- x.cond %*% t(pred.condpar.psim)
      pred.zipar.psim <- MASS::mvrnorm(n = nsim, mu = beta.zi, Sigma = zi.varcov)
      pred.zi.psim <- x.zi %*% t(pred.zipar.psim)

      if (!.is_empty(keep)) {
        pred.cond.psim <- pred.cond.psim[keep, , drop = FALSE]
        pred.zi.psim <- pred.zi.psim[keep, , drop = FALSE]
      }

      list(cond = pred.cond.psim, zi = pred.zi.psim)
    },
    error = function(x) { x },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )
}


.simulate_predictions_zeroinfl <- function(model, newdata, nsim = 1000, terms = NULL, value_adjustment = NULL, condition = NULL) {
  tryCatch(
    {
      condformula <- stats::as.formula(paste0("~", insight::safe_deparse(stats::formula(model)[[3]][[2]])))
      ziformula <- stats::as.formula(paste0("~", insight::safe_deparse(stats::formula(model)[[3]][[3]])))

      # if formula has a polynomial term, and this term is one that is held
      # constant, model.matrix() with "newdata" will throw an error - so we
      # re-build the newdata-argument by including all values for poly-terms, if
      # these are hold constant.

      fixes <- .rows_to_keep(model, newdata, condformula, ziformula, terms, value_adjustment, condition)

      if (!is.null(fixes)) {
        keep <- fixes$keep
        newdata <- fixes$newdata
      } else {
        keep <- NULL
      }

      x.cond <- stats::model.matrix(condformula, model = "count", data = newdata)
      beta.cond <- stats::coef(model, model = "count")

      x.zi <- stats::model.matrix(ziformula, model = "zero", data = newdata)
      beta.zi <- stats::coef(model, model = "zero")

      cond.varcov <- insight::get_varcov(model, component = "conditional")
      zi.varcov <- insight::get_varcov(model, component = "zero_inflated")

      pred.condpar.psim <- MASS::mvrnorm(nsim, mu = beta.cond, Sigma = cond.varcov)
      pred.cond.psim <- x.cond %*% t(pred.condpar.psim)

      pred.zipar.psim <- MASS::mvrnorm(nsim, mu = beta.zi, Sigma = zi.varcov)
      pred.zi.psim <- x.zi %*% t(pred.zipar.psim)

      if (!.is_empty(keep)) {
        pred.cond.psim <- pred.cond.psim[keep, , drop = FALSE]
        pred.zi.psim <- pred.zi.psim[keep, , drop = FALSE]
      }

      list(cond = pred.cond.psim, zi = pred.zi.psim)
    },
    error = function(x) { x },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )
}






.rows_to_keep <- function(model, newdata, condformula, ziformula, terms, value_adjustment, condition) {
  # if formula has a polynomial term, and this term is one that is held
  # constant, model.matrix() with "newdata" will throw an error - so we
  # re-build the newdata-argument by including all values for poly-terms, if
  # these are hold constant.

  const.values <- attr(newdata, "constant.values")
  condformula_string <- insight::safe_deparse(condformula)
  ziformula_string <- insight::safe_deparse(ziformula)

  keep <- NULL

  if (.has_poly_term(condformula_string) || .has_poly_term(ziformula_string)) {

    model_frame <- insight::get_data(model)

    polycondcheck <- NULL
    polyzicheck <- NULL

    if (.has_poly_term(condformula_string)) {
      polyterm <- .get_poly_term(condformula_string)
      if (polyterm %in% names(const.values)) {
        polycondcheck <- polyterm
        polydg <- .get_poly_degree(condformula_string)
        polyvals <- paste0(
          stats::quantile(model_frame[[polyterm]], probs = seq_len(polydg + 1) / (polydg + 2)),
          collapse = ","
        )
        terms <- c(terms, sprintf("%s [%s]", polyterm, polyvals))
      }
    }

    if (.has_poly_term(ziformula_string)) {
      polyterm <- .get_poly_term(ziformula_string)
      if (polyterm %in% names(const.values)) {
        polyzicheck <- polyterm
        polydg <- .get_poly_degree(ziformula_string)
        polyvals <- paste0(
          stats::quantile(model_frame[[polyterm]], probs = seq_len(polydg + 1) / (polydg + 2)),
          collapse = ","
        )
        terms <- c(terms, sprintf("%s [%s]", polyterm, polyvals))
      }
    }

    newdata <- .data_grid(
      model = model,
      model_frame = model_frame,
      terms = terms,
      value_adjustment = value_adjustment,
      factor_adjustment = FALSE,
      show_pretty_message = FALSE,
      condition = condition
    )

    keep.cond <- vector("numeric")
    keep.zi <- vector("numeric")

    if (!is.null(polycondcheck)) {
      keep.cond <- unlist(lapply(polycondcheck, function(.x) {
        wm <- newdata[[.x]][which.min(abs(newdata[[.x]] - .typical_value(newdata[[.x]], fun = value_adjustment)))]
        as.vector(which(newdata[[.x]] == wm))
      }))
    }

    if (!is.null(polyzicheck)) {
      keep.zi <- unlist(lapply(polyzicheck, function(.x) {
        wm <- newdata[[.x]][which.min(abs(newdata[[.x]] - .typical_value(newdata[[.x]], fun = value_adjustment)))]
        as.vector(which(newdata[[.x]] == wm))
      }))
    }

    keep <- intersect(keep.cond, keep.zi)

    if (.is_empty(keep))
      keep <- unique(c(keep.cond, keep.zi))
  }

  if (.is_empty(keep)) return(NULL)

  list(keep = keep, newdata = newdata)
}



.get_zeroinfl_gam_predictions <- function(model, newdata, nsim = 1000) {
  tryCatch(
    {
      mm <- stats::model.matrix(model, data = newdata)

      linpred <- attr(mm, "lpi", exact = TRUE)
      cond <- linpred[[1]]
      zi <- linpred[[2]]

      x.cond <- mm[, cond]
      x.zi <- mm[, zi]

      beta.cond <- stats::coef(model)[cond]
      beta.zi <- stats::coef(model)[zi]

      varcov.cond <- stats::vcov(model)[cond, cond]
      varcov.zi <- stats::vcov(model)[zi, zi]

      psim.cond <- MASS::mvrnorm(nsim, mu = beta.cond, Sigma = varcov.cond)
      pred.cond <- x.cond %*% t(psim.cond)

      psim.zi <- MASS::mvrnorm(nsim, mu = beta.zi, Sigma = varcov.zi)
      pred.zi <- x.zi %*% t(psim.zi)

      list(cond = pred.cond, zi = pred.zi)
    },
    error = function(x) { x },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )
}
