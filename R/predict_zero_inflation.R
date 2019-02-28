#' @importFrom dplyr group_by summarize ungroup left_join filter arrange slice
#' @importFrom rlang syms .data
#' @importFrom stats quantile sd
#' @importFrom purrr map_lgl
get_zeroinfl_fitfram <- function(fitfram, newdata, prdat, sims, ci, clean_terms) {
  # after "bootstrapping" confidence intervals by simulating from the
  # multivariate normal distribution, we need to prepare the data and
  # calculate "bootstrapped" estimates and CIs.

  fitfram$sort__id <- 1:nrow(fitfram)
  column_matches <- purrr::map_lgl(colnames(fitfram), ~ any(unique(fitfram[[.x]]) %in% newdata[[.x]]))
  join_by <- colnames(fitfram)[column_matches]
  fitfram <- suppressMessages(suppressWarnings(dplyr::left_join(newdata, fitfram, by = join_by)))

  fitfram$predicted <- apply(sims, 1, mean)
  fitfram$conf.low <- apply(sims, 1, stats::quantile, probs = 1 - ci)
  fitfram$conf.high <- apply(sims, 1, stats::quantile, probs = ci)
  fitfram$std.error <- apply(sims, 1, stats::sd)

  # group_by() changes the order of rows / variables in "fitfram", however
  # we later add back the original predictions "prdat" (see below), which
  # correspond to the *current* sorting of fitfram. So we add a dummy-ID,
  # which we use to restore the original sorting of fitfram later...

  grp <- rlang::syms(clean_terms)
  fitfram <- fitfram %>%
    dplyr::filter(!is.na(.data$sort__id)) %>%
    dplyr::group_by(!!! grp) %>%
    dplyr::summarize(
      predicted = mean(.data$predicted),
      conf.low = mean(.data$conf.low),
      conf.high = mean(.data$conf.high),
      std.error = mean(.data$std.error),
      id = .data$sort__id
    ) %>%
    dplyr::ungroup()

  # we use the predicted values from "predict(type = "reponse")", but the
  # bootstrapped CI - so we need to fix a bit here. "predict(type = "reponse")"
  # works as intended, but the related standard errors are not reliable (due
  # to the combination of the conditional and zero-inflated model), so we need
  # the simulated standard errors and CI's - but can use the "correct" predictions.
  # in order to make CI and predictions match, we take the simulated CI-range
  # and use the original predicted values as "center" for those CI-ranges.

  if (length(prdat) == nrow(fitfram)) {
    fitfram <- dplyr::arrange(fitfram, .data$id)
    ci.range <- (fitfram$conf.high - fitfram$conf.low) / 2
    fitfram$predicted <- prdat

    # fix negative CI
    ci.low <- fitfram$predicted - ci.range
    neg.ci <- ci.low < 0
    if (any(neg.ci)) {
      ci.range[neg.ci] <- ci.range[neg.ci] - abs(ci.low[neg.ci]) - 1e-05
      fitfram$std.error[neg.ci] <- fitfram$std.error[neg.ci] - ((abs(ci.low[neg.ci]) + 1e-05) / stats::qnorm(ci))
    }

    fitfram$conf.low <- fitfram$predicted - ci.range
    fitfram$conf.high <- fitfram$predicted + ci.range
    fitfram <- dplyr::select(fitfram, -.data$id)
  }

  fitfram
}


#' @importFrom MASS mvrnorm
#' @importFrom stats model.matrix formula
#' @importFrom sjmisc is_empty
get_glmmTMB_predictions <- function(model, newdata, nsim, terms = NULL, typical = NULL, condition = NULL) {

  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("You need to install package `lme4` first to compute marginal effects.", call. = FALSE)
  }


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

      fixes <- get_rows_to_keep(model, newdata, condformula, ziformula, terms, typical, condition)

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

      cond.varcov <- getVarCov(model, component = "cond", type = "glmmTMB")
      zi.varcov <- getVarCov(model, component = "zi", type = "glmmTMB")

      pred.condpar.psim <- MASS::mvrnorm(n = nsim, mu = beta.cond, Sigma = cond.varcov)
      pred.cond.psim <- x.cond %*% t(pred.condpar.psim)
      pred.zipar.psim <- MASS::mvrnorm(n = nsim, mu = beta.zi, Sigma = zi.varcov)
      pred.zi.psim <- x.zi %*% t(pred.zipar.psim)

      if (!sjmisc::is_empty(keep)) {
        pred.cond.psim <- pred.cond.psim[keep, ]
        pred.zi.psim <- pred.zi.psim[keep, ]
      }

      list(cond = pred.cond.psim, zi = pred.zi.psim)
    },
    error = function(x) { x },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )
}


#' @importFrom MASS mvrnorm
#' @importFrom stats model.matrix formula
get_MixMod_predictions <- function(model, newdata, nsim, terms = NULL, typical = NULL, condition = NULL) {

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

      fixes <- get_rows_to_keep(model, newdata, condformula, ziformula, terms, typical, condition)

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

      cond.varcov <- getVarCov(model, component = "fixed-effects", type = "MixMod")
      zi.varcov <- getVarCov(model, component = "zero_part", type = "MixMod")

      pred.condpar.psim <- MASS::mvrnorm(n = nsim, mu = beta.cond, Sigma = cond.varcov)
      pred.cond.psim <- x.cond %*% t(pred.condpar.psim)
      pred.zipar.psim <- MASS::mvrnorm(n = nsim, mu = beta.zi, Sigma = zi.varcov)
      pred.zi.psim <- x.zi %*% t(pred.zipar.psim)

      if (!sjmisc::is_empty(keep)) {
        pred.cond.psim <- pred.cond.psim[keep, ]
        pred.zi.psim <- pred.zi.psim[keep, ]
      }

      list(cond = pred.cond.psim, zi = pred.zi.psim)
    },
    error = function(x) { x },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )
}


#' @importFrom stats model.matrix coef formula as.formula
#' @importFrom MASS mvrnorm
get_zeroinfl_predictions <- function(model, newdata, nsim = 1000, terms = NULL, typical = NULL, condition = NULL) {
  tryCatch(
    {
      condformula <- stats::as.formula(paste0("~", deparse(stats::formula(model)[[3]][[2]], width.cutoff = 500L)))
      ziformula <- stats::as.formula(paste0("~", deparse(stats::formula(model)[[3]][[3]], width.cutoff = 500L)))

      # if formula has a polynomial term, and this term is one that is held
      # constant, model.matrix() with "newdata" will throw an error - so we
      # re-build the newdata-argument by including all values for poly-terms, if
      # these are hold constant.

      fixes <- get_rows_to_keep(model, newdata, condformula, ziformula, terms, typical, condition)

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

      cond.varcov <- getVarCov(model, component = "count", type = "pscl")
      zi.varcov <- getVarCov(model, component = "zero", type = "pscl")

      pred.condpar.psim <- MASS::mvrnorm(nsim, mu = beta.cond, Sigma = cond.varcov)
      pred.cond.psim <- x.cond %*% t(pred.condpar.psim)

      pred.zipar.psim <- MASS::mvrnorm(nsim, mu = beta.zi, Sigma = zi.varcov)
      pred.zi.psim <- x.zi %*% t(pred.zipar.psim)

      if (!sjmisc::is_empty(keep)) {
        pred.cond.psim <- pred.cond.psim[keep, ]
        pred.zi.psim <- pred.zi.psim[keep, ]
      }

      list(cond = pred.cond.psim, zi = pred.zi.psim)
    },
    error = function(x) { x },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )
}


#' @importFrom stats vcov
getVarCov <- function(model, component, type = "glmmTMB") {
  # sometimes, the variance-covariance matrix (needed for Sigma in MASS::mvrnorm)
  # is negative, so MASS::mvrnorm fails. Here we check for a negativ matrix,
  # and if it is, we try to find the nearest positive matrix.

  vc <- tryCatch(
    {
      switch(
        type,
        glmmTMB = stats::vcov(model)[[component]],
        pscl = stats::vcov(model, model = component),
        MixMod = stats::vcov(model, parm = component),
        stats::vcov(model)[[component]]
      )
    },
    error = function(x) { NULL },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )

  if ((is.null(vc) || is.negativ.matrix(vc)) && requireNamespace("Matrix", quietly = TRUE)) {
    vc <- tryCatch(
      {
        m <- switch(
          type,
          glmmTMB = stats::vcov(model)[[component]],
          pscl = stats::vcov(model, model = component),
          MixMod = stats::vcov(model, parm = component),
          stats::vcov(model)[[component]]
        )

        as.matrix(Matrix::nearPD(m)$mat)
      },
      error = function(x) { NULL },
      warning = function(x) { NULL },
      finally = function(x) { NULL }
    )
  }

  vc
}


is.negativ.matrix <- function(x) {
  if (is.matrix(x) && (nrow(x) == ncol(x))) {
    eigenvalues <- eigen(x, only.values = TRUE)$values
    eigenvalues[abs(eigenvalues) < 1e-07] <- 0
    rv <- any(eigenvalues <= 0)
  } else {
    rv <- FALSE
  }

  rv
}


#' @importFrom insight get_data
#' @importFrom sjmisc typical_value
#' @importFrom stats quantile
get_rows_to_keep <- function(model, newdata, condformula, ziformula, terms, typical, condition) {
  # if formula has a polynomial term, and this term is one that is held
  # constant, model.matrix() with "newdata" will throw an error - so we
  # re-build the newdata-argument by including all values for poly-terms, if
  # these are hold constant.

  const.values <- attr(newdata, "constant.values")
  condformula_string <- deparse(condformula, width.cutoff = 500L)
  ziformula_string <- deparse(ziformula, width.cutoff = 500L)

  keep <- NULL

  if (.has_poly_term(condformula_string) || .has_poly_term(ziformula_string)) {

    mf <- insight::get_data(model)

    polycondcheck <- NULL
    polyzicheck <- NULL

    if (.has_poly_term(condformula_string)) {
      polyterm <- .get_poly_term(condformula_string)
      if (polyterm %in% names(const.values)) {
        polycondcheck <- polyterm
        polydg <- .get_poly_degree(condformula_string)
        polyvals <- paste0(
          stats::quantile(mf[[polyterm]], probs = seq_len(polydg + 1) / (polydg + 2)),
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
          stats::quantile(mf[[polyterm]], probs = seq_len(polydg + 1) / (polydg + 2)),
          collapse = ","
        )
        terms <- c(terms, sprintf("%s [%s]", polyterm, polyvals))
      }
    }

    newdata <- get_expanded_data(
      model = model,
      mf = mf,
      terms = terms,
      typ.fun = typical,
      fac.typical = FALSE,
      pretty.message = FALSE,
      condition = condition
    )

    keep.cond <- vector("numeric")
    keep.zi <- vector("numeric")

    if (!is.null(polycondcheck)) {
      keep.cond <- lapply(polycondcheck, function(.x) {
        wm <- newdata[[.x]][which.min(abs(newdata[[.x]] - sjmisc::typical_value(newdata[[.x]], fun = typical)))]
        as.vector(which(newdata[[.x]] == wm))
      }) %>% unlist()
    }

    if (!is.null(polyzicheck)) {
      keep.zi <- lapply(polyzicheck, function(.x) {
        wm <- newdata[[.x]][which.min(abs(newdata[[.x]] - sjmisc::typical_value(newdata[[.x]], fun = typical)))]
        as.vector(which(newdata[[.x]] == wm))
      }) %>% unlist()
    }

    keep <- intersect(keep.cond, keep.zi)

    if (sjmisc::is_empty(keep))
      keep <- unique(c(keep.cond, keep.zi))
  }

  if (sjmisc::is_empty(keep)) return(NULL)

  list(keep = keep, newdata = newdata)
}
