## TODO remove once this fun is available in insight

#' @importFrom insight model_info
#' @keywords internal
.get_ranef_variance <- function(x) {

  ## Code taken from GitGub-Repo of package glmmTMB
  ## Author: Ben Bolker, who used an
  ## cleaned-up/adapted version of Jon Lefcheck's code from SEMfit

  faminfo <- insight::model_info(x)

  if (!faminfo$is_mixed) {
    stop("Model is not a mixed model.", call. = FALSE)
  }

  if (faminfo$family %in% c("truncated_nbinom1", "truncated_nbinom2", "tweedie")) {
    warning("Can't get random effects variances for truncated negative binomial and tweedie families.", call. = F)
    return(NA)
  }

  # Test for non-zero random effects ((near) singularity)
  if (.is_singular(x)) {
    warning("Can't compute random effects variances. Some variance components equal zero.\n  Solution: Respecify random structure!", call. = F)
    return(NA)
  }

  # get necessary model information, like fixed and random effects,
  # variance-covariance matrix etc.
  vals <- .get_variance_information(x)

  # Separate observation variance from variance of random effects
  nr <- sapply(vals$re, nrow)
  not.obs.terms <- names(nr[nr != stats::nobs(x)])

  .get_variance_random(not.obs.terms, x = x, vals = vals)
}


#' @keywords internal
.get_variance_information <- function(x) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needs to be installed to compute variances for mixed models.", call. = FALSE)
  }

  if (inherits(x, "rstanarm") && !requireNamespace("rstanarm", quietly = TRUE)) {
    stop("Package `rstanarm` needs to be installed to compute variances for mixed models.", call. = FALSE)
  }

  if (inherits(x, "stanreg")) {
    xcomp <- rstanarm::get_x(x)
  } else {
    xcomp <- lme4::getME(x, "X")
  }

  vals <- list(
    beta = lme4::fixef(x),
    X = xcomp,
    vc = lme4::VarCorr(x),
    re = lme4::ranef(x)
  )

  # for glmmTMB, use conditional component of model only,
  # and tell user that zero-inflation is ignored

  if (inherits(x, "glmmTMB")) {
    vals <- lapply(vals, .collapse_cond)

    nullEnv <- function(x) {
      environment(x) <- NULL
      return(x)
    }

    if (!identical(nullEnv(x$modelInfo$allForm$ziformula), nullEnv(~0))) {
      warning("Random effect variance ignores effects of zero-inflation.", call. = FALSE)
    }

    dform <- nullEnv(x$modelInfo$allForm$dispformula)

    if (!identical(dform, nullEnv(~1)) && (!identical(dform, nullEnv(~0)))) {
      warning("Random effect variance ignores effects of dispersion model.", call. = FALSE)
    }
  }

  vals
}


#' glmmTMB returns a list of model information, one for conditional and one for zero-inflated part, so here we "unlist" it
#' @keywords internal
.collapse_cond <- function(x) {
  if (is.list(x) && "cond" %in% names(x)) {
    x[["cond"]]
  } else {
    x
  }
}


#' Compute variance associated with a random-effects term (Johnson 2014)
#' @importFrom stats nobs
#' @keywords internal
.get_variance_random <- function(terms, x, vals) {
  sum(sapply(
    vals$vc[terms],
    function(Sigma) {
      rn <- rownames(Sigma)

      if (!is.null(rn)) {
        valid <- rownames(Sigma) %in% colnames(vals$X)
        if (!all(valid)) {
          rn <- rn[valid]
          Sigma <- Sigma[valid, valid]
        }
      }

      Z <- vals$X[, rn, drop = FALSE]
      # Z <- vals$X[, rownames(Sigma), drop = FALSE]
      Z.m <- Z %*% Sigma
      sum(diag(crossprod(Z.m, Z))) / stats::nobs(x)
    }
  ))
}


# checks if a mixed model fit is singular or not. Need own function,
# because lme4::isSingular() does not work with glmmTMB
#' @keywords internal
.is_singular <- function(x, tolerance = 1e-5) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needs to be installed to compute variances for mixed models.", call. = FALSE)
  }

  if (inherits(x, "stanreg")) {
    singular <- FALSE
  } else if (inherits(x, "glmmTMB")) {
    vc <- .collapse_cond(lme4::VarCorr(x))
    singular <- any(sapply(vc, function(.x) any(abs(diag(.x)) < tolerance)))
  } else if (inherits(x, "merMod")) {
    theta <- lme4::getME(x, "theta")
    diag.element <- lme4::getME(x, "lower") == 0
    singular <- any(abs(theta[diag.element]) < tolerance)
  }

  singular
}
