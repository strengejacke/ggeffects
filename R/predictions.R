# select prediction method, based on model-object
select_prediction_method <- function(fun, model, expanded_frame, ci.lvl, type, binom_fam, ...) {

  # get link-inverse-function
  linv <- get_link_inverse(fun, model)

  if (fun == "svyglm") {
    # survey-objects -----
    fitfram <- get_predictions_svyglm(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "svyglm.nb") {
    # survey-glm.nb-objects -----
    fitfram <- get_predictions_svyglmnb(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun %in% c("zeroinfl", "hurdle")) {
    # zeroinfl-objects -----
    fitfram <- get_predictions_zeroinfl(model, expanded_frame, ...)
  } else if (fun == "lrm") {
    # lrm-objects -----
    fitfram <- get_predictions_lrm(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "glmmTMB") {
    # glmTMB-objects -----
    fitfram <- get_predictions_glmmTMB(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun %in% c("lmer", "nlmer", "glmer")) {
    # merMod-objects, variant -----
    fitfram <- get_predictions_merMod(model, expanded_frame, ci.lvl, linv, type, ...)
  } else if (fun == "gam") {
    # gam-objects -----
    fitfram <- get_predictions_gam(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "vgam") {
    # gam-objects -----
    fitfram <- get_predictions_vgam(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun %in% c("lme", "gls", "plm")) {
    # lme-objects -----
    fitfram <- get_predictions_lme(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "gee") {
    # gee-objects -----
    fitfram <- get_predictions_gee(model, expanded_frame, linv, ...)
  } else if (fun %in% c("glm", "glm.nb")) {
    # glm-objects -----
    fitfram <- get_predictions_glm(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "polr") {
    # glm-objects -----
    fitfram <- get_predictions_polr(model, expanded_frame, linv, ...)
  } else if (fun == "lm") {
    # lm-objects -----
    fitfram <- get_predictions_lm(model, expanded_frame, ci.lvl, linv, ...)
  } else {
    # general-objects -----
    fitfram <- get_predictions_generic(model, expanded_frame, linv, ...)
  }

  fitfram
}


# predictions for survey objects ----

get_predictions_svyglm <- function(model, fitfram, ci.lvl, linv, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "link",
      se.fit = se,
      level = ci.lvl,
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
    fitfram$conf.low <- linv(prdat$fit - stats::qnorm(.975) * prdat$se.fit)
    fitfram$conf.high <- linv(prdat$fit + stats::qnorm(.975) * prdat$se.fit)
  } else {
    # copy predictions
    fitfram$predicted <- as.vector(prdat)

    # no CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}


# predictions for glm ----

get_predictions_glm <- function(model, fitfram, ci.lvl, linv, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  prdat <-
    stats::predict.glm(
      model,
      newdata = fitfram,
      type = "link",
      se.fit = se,
      level = ci.lvl,
      ...
    )

  # copy predictions
  get_base_fitfram(fitfram, linv, prdat, se)
}


# predictions for polr ----

#' @importFrom tidyr gather
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom tibble rownames_to_column
get_predictions_polr <- function(model, fitfram, linv, ...) {
  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "probs",
      ...
    )

  prdat <- as.data.frame(prdat)

  # usually, we have same numbers of rows for predictions and model frame.
  # this is, however. not true when calling the "emm()" function. in this
  # case. just return predictions
  if (nrow(prdat) > nrow(fitfram) && ncol(prdat) == 1) {
    colnames(prdat)[1] <- "predicted"
    return(tibble::rownames_to_column(prdat, var = "response.level"))
  }

  # bind predictions to model frame
  fitfram <- dplyr::bind_cols(prdat, fitfram)

  # for proportional ordinal logistic regression (see MASS::polr),
  # we have predicted values for each response category. Hence,
  # gather columns
  fitfram <- tidyr::gather(fitfram, key = "response.level", value = "predicted", 1:ncol(prdat))

  # No CI
  fitfram$conf.low <- NA
  fitfram$conf.high <- NA

  fitfram
}


## predictions for zeroinfl ----

get_predictions_zeroinfl <- function(model, fitfram, ...) {
  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "response",
      ...
    )

  # copy predictions
  fitfram$predicted <- as.vector(prdat)

  # No CI
  fitfram$conf.low <- NA
  fitfram$conf.high <- NA

  fitfram
}


# predictions for lrm ----

#' @importFrom stats plogis
get_predictions_lrm <- function(model, fitfram, ci.lvl, linv, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "lp",
      se.fit = se,
      ...
    )

  # copy predictions
  fitfram$predicted <- stats::plogis(prdat$linear.predictors)

  # did user request standard errors? if yes, compute CI
  if (se) {
    # calculate CI
    fitfram$conf.low <- stats::plogis(prdat$linear.predictors - stats::qnorm(.975) * prdat$se.fit)
    fitfram$conf.high <- stats::plogis(prdat$linear.predictors + stats::qnorm(.975) * prdat$se.fit)
  } else {
    # No CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}


# predictions for svyglm.nb ----

get_predictions_svyglmnb <- function(model, fitfram, ci.lvl, linv, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "link",
      se.fit = se,
      level = ci.lvl,
      ...
    )

  # copy predictions
  get_base_fitfram(fitfram, linv, prdat, se)
}


# predictions for glmmTMB ----

get_predictions_glmmTMB <- function(model, fitfram, ci.lvl, linv, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  prdat <- stats::predict(
    model,
    newdata = fitfram,
    zitype = "response",
    type = "response",
    se.fit = se,
    ...
  )

  # get link-function and back-transform fitted values
  # to original scale, so we compute proper CI
  lf <- stats::family(model)$linkfun
  prdat$fit <- lf(prdat$fit)

  # did user request standard errors? if yes, compute CI
  if (se) {
    # copy predictions
    fitfram$predicted <- linv(prdat$fit)

    # calculate CI
    fitfram$conf.low <- linv(prdat$fit - stats::qnorm(.975) * prdat$se.fit)
    fitfram$conf.high <- linv(prdat$fit + stats::qnorm(.975) * prdat$se.fit)
  } else {
    # copy predictions
    fitfram$predicted <- as.vector(prdat)

    # no CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}


# predictions for merMod ----

#' @importFrom stats model.matrix terms
#' @importFrom Matrix tcrossprod
get_predictions_merMod <- function(model, fitfram, ci.lvl, linv, type, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  if (type == "fe") {
    fitfram$predicted <- stats::predict(
      model,
      newdata = fitfram,
      type = "response",
      re.form = NA,
      ...
    )
  } else {
    fitfram$predicted <- stats::predict(
      model,
      newdata = fitfram,
      type = "response",
      re.form = NULL,
      ...
    )
  }

  if (se) {
    # prepare model frame for matrix multiplication
    newdata <- get_model_frame(model)[, all.vars(stats::terms(model))[-1]] %>%
      purrr::map(~unique(.x, na.rm = T)) %>%
      expand.grid() %>%
      tibble::add_column(resp = 0)

    colnames(newdata)[ncol(newdata)] <- sjstats::resp_var(model)

    # code to compute se of prediction taken from http://glmm.wikidot.com/faq
    mm <- stats::model.matrix(stats::terms(model), newdata)
    pvar <- diag(mm %*% as.matrix(Matrix::tcrossprod(stats::vcov(model), mm)))
    se.fit <- sqrt(pvar)

    # shorten to length of fitfram
    se.fit <- se.fit[1:nrow(fitfram)]

    if (is.null(linv)) {
      # calculate CI for linear mixed models
      fitfram$conf.low <- fitfram$predicted - stats::qnorm(.975) * se.fit
      fitfram$conf.high <- fitfram$predicted + stats::qnorm(.975) * se.fit
    } else {
      # calculate CI for glmm
      fitfram$conf.low <- linv(fitfram$predicted - stats::qnorm(.975) * se.fit)
      fitfram$conf.high <- linv(fitfram$predicted + stats::qnorm(.975) * se.fit)
    }

    # tell user
    message("Note: uncertainty of the random effects parameters are not taken into account for confidence intervals.")
  } else {
    # no SE and CI for lme4-predictions
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}


# predictions for gam ----

#' @importFrom prediction prediction
get_predictions_gam <- function(model, fitfram, ci.lvl, ...) {
  # call prediction
  prdat <- prediction::prediction(model, data = fitfram, at = NULL, type = "response", ...)

  # copy predictions
  fitfram$predicted <- prdat$fitted

  fitfram
}


# predictions for vgam ----

#' @importFrom prediction prediction
get_predictions_vgam <- function(model, fitfram, ci.lvl, linv, ...) {
  prdat <- stats::predict(
    model,
    type = "response",
    ...
  )

  # copy predictions
  fitfram$predicted <- prdat$fitted

  fitfram
}


# predictions for lm ----

get_predictions_lm <- function(model, fitfram, ci.lvl, linv, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "response",
      se.fit = se,
      level = ci.lvl,
      ...
    )

  # did user request standard errors? if yes, compute CI
  if (se) {
    # copy predictions
    fitfram$predicted <- prdat$fit

    # calculate CI
    fitfram$conf.low <- prdat$fit - stats::qnorm(.975) * prdat$se.fit
    fitfram$conf.high <- prdat$fit + stats::qnorm(.975) * prdat$se.fit
  } else {
    # copy predictions
    fitfram$predicted <- as.vector(prdat)

    # no CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}


# predictions for lme ----

#' @importFrom stats model.matrix formula vcov
#' @importFrom sjstats resp_var pred_vars
#' @importFrom purrr map
#' @importFrom tibble add_column
get_predictions_lme <- function(model, fitfram, ci.lvl, linv, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "response",
      level = 0,
      ...
    )

  # copy predictions
  fitfram$predicted <- as.vector(prdat)

  # did user request standard errors? if yes, compute CI
  if (se) {
    # prepare model frame for matrix multiplication
    newdata <- get_model_frame(model)[, sjstats::pred_vars(model)] %>%
      purrr::map(~unique(.x, na.rm = T)) %>%
      expand.grid() %>%
      tibble::add_column(resp = 0)

    colnames(newdata)[ncol(newdata)] <- sjstats::resp_var(model)

    # [-2] drops response from formula
    design.mat <- stats::model.matrix(stats::formula(model)[-2], newdata)
    # code to compute se of prediction taken from http://glmm.wikidot.com/faq
    predvar <- diag(design.mat %*% stats::vcov(model) %*% t(design.mat))
    se.fit <- sqrt(predvar)

    # shorten to length of fitfram
    se.fit <- se.fit[1:nrow(fitfram)]

    # calculate CI
    fitfram$conf.low <- fitfram$predicted - stats::qnorm(.975) * se.fit
    fitfram$conf.high <- fitfram$predicted + stats::qnorm(.975) * se.fit

    # tell user
    message("Note: uncertainty of the random effects parameters are not taken into account for confidence intervals.")
  } else {
    # No CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}


# predictions for gee ----

get_predictions_gee <- function(model, fitfram, linv, ...) {
  prdat <- prdat <-
    stats::predict(
      model,
      type = "response",
      ...
    )
  # copy predictions
  fitfram$predicted <- as.vector(prdat)

  # No CI
  fitfram$conf.low <- NA
  fitfram$conf.high <- NA

  fitfram
}


# predictions for generic models ----

#' @importFrom prediction prediction
#' @importFrom tibble as_tibble
#' @importFrom sjmisc var_rename
get_predictions_generic <- function(model, fitfram, linv, ...) {
  prdat <-
    prediction::prediction(
      model,
      data = fitfram,
      type = "response",
      ...
    )

  # copy predictions
  fitfram$predicted <- prdat$fitted

  # No CI
  fitfram$conf.low <- NA
  fitfram$conf.high <- NA

  fitfram
}


get_base_fitfram <- function(fitfram, linv, prdat, se) {
  # copy predictions
  if (typeof(prdat) == "double")
    fitfram$predicted <- linv(prdat)
  else
    fitfram$predicted <- linv(prdat$fit)

  # did user request standard errors? if yes, compute CI
  if (se) {
    # calculate CI
    fitfram$conf.low <- linv(prdat$fit - stats::qnorm(.975) * prdat$se.fit)
    fitfram$conf.high <- linv(prdat$fit + stats::qnorm(.975) * prdat$se.fit)
  } else {
    # No CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}
