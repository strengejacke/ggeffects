# select prediction method, based on model-object
#' @importFrom sjstats link_inverse
select_prediction_method <- function(fun, model, expanded_frame, ci.lvl, type, faminfo, ppd, terms, typical, ...) {
  # get link-inverse-function
  linv <- sjstats::link_inverse(model)

  if (fun == "svyglm") {
    # survey-objects -----
    fitfram <- get_predictions_svyglm(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "svyglm.nb") {
    # survey-glm.nb-objects -----
    fitfram <- get_predictions_svyglmnb(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "stanreg") {
    # stan-objects -----
    fitfram <- get_predictions_stan(model, expanded_frame, ci.lvl, type, faminfo, ppd, ...)
  } else if (fun == "brmsfit") {
    # brms-objects -----
    fitfram <- get_predictions_stan(model, expanded_frame, ci.lvl, type, faminfo, ppd, ...)
  } else if (fun == "coxph") {
    # coxph-objects -----
    fitfram <- get_predictions_coxph(model, expanded_frame, ci.lvl, ...)
  } else if (fun == "lrm") {
    # lrm-objects -----
    fitfram <- get_predictions_lrm(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "glmmTMB") {
    # glmmTMB-objects -----
    fitfram <- get_predictions_glmmTMB(model, expanded_frame, ci.lvl, linv, type, ...)
  } else if (fun %in% c("lmer", "nlmer", "glmer")) {
    # merMod-objects  -----
    fitfram <- get_predictions_merMod(model, expanded_frame, ci.lvl, linv, type, terms, typical, ...)
  } else if (fun == "gam") {
    # gam-objects -----
    fitfram <- get_predictions_gam(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "vgam") {
    # vgam-objects -----
    fitfram <- get_predictions_vgam(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun %in% c("lme", "gls", "plm")) {
    # lme-objects -----
    fitfram <- get_predictions_lme(model, expanded_frame, ci.lvl, type, terms, typical, ...)
  } else if (fun == "gee") {
    # gee-objects -----
    fitfram <- get_predictions_gee(model, expanded_frame, linv, ...)
  } else if (fun == "multinom") {
    # multinom-objects -----
    fitfram <- get_predictions_multinom(model, expanded_frame, linv, ...)
  } else if (fun == "clm") {
    # clm-objects -----
    fitfram <- get_predictions_clm(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "Zelig-relogit") {
    # Zelig-relogit-objects -----
    fitfram <- get_predictions_zelig(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "polr") {
    # polr-objects -----
    fitfram <- get_predictions_polr(model, expanded_frame, ci.lvl, linv, typical, terms, fun, ...)
  } else if (fun %in% c("betareg", "truncreg", "zeroinfl", "hurdle")) {
    # betareg, truncreg, zeroinfl and hurdle-objects -----
    fitfram <- get_predictions_generic2(model, expanded_frame, ci.lvl, fun, typical, terms, ...)
  } else if (fun %in% c("glm", "glm.nb", "glmRob")) {
    # glm-objects -----
    fitfram <- get_predictions_glm(model, expanded_frame, ci.lvl, linv, ...)
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

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975

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

  # for models from "robust"-pkg (glmRob) we need to
  # suppress warnings about fake models
  prdat <-
    suppressWarnings(stats::predict.glm(
      model,
      newdata = fitfram,
      type = "link",
      se.fit = se,
      ...
    ))

  # copy predictions
  get_base_fitfram(fitfram, linv, prdat, se, ci.lvl)
}


# predictions for polr ----

#' @importFrom tidyr gather
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom tibble rownames_to_column
#' @importFrom rlang .data
get_predictions_polr <- function(model, fitfram, ci.lvl, linv, typical, terms, fun, ...) {

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975

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
  key_col <- "response.level"
  value_col <- "predicted"

  fitfram <- tidyr::gather(fitfram, !! key_col, !! value_col, !! 1:ncol(prdat))

  se.pred <-
    get_se_from_vcov(
      model = model,
      fitfram = fitfram,
      typical = typical,
      terms = terms,
      fun = fun
    )

  if (!is.null(se.pred)) {
    se.fit <- se.pred$se.fit
    fitfram <- se.pred$fitfram
    # CI
    fitfram$conf.low <- linv(stats::qlogis(fitfram$predicted) - stats::qnorm(ci) * se.fit)
    fitfram$conf.high <- linv(stats::qlogis(fitfram$predicted) + stats::qnorm(ci) * se.fit)
  } else {
    # CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }


  fitfram
}


# predictions for Zelig-relogit model ----

get_predictions_zelig <- function(model, fitfram, ci.lvl, linv, ...) {

  message("Support for Zelig-models not implemented yet.")

  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975

  # prediction, with CI
  # prdat <-
  #   Zelig::predict(
  #     model,
  #     newdata = fitfram,
  #     interval = se,
  #     level = ci,
  #     ...
  #   )

  NULL
}


# predictions for cumulative link model ----

#' @importFrom sjmisc to_long
get_predictions_clm <- function(model, fitfram, ci.lvl, linv, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975

  # prediction, with CI
  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "prob",
      interval = se,
      level = ci,
      ...
    )

  # convert to data frame.
  prdat <- as.data.frame(prdat)

  # bind predictions to model frame
  fitfram <- dplyr::bind_cols(prdat, fitfram)

  # get levels of response
  lv <- levels(sjstats::model_frame(model)[[sjstats::resp_var(model)]])

  # for proportional ordinal logistic regression (see ordinal::clm),
  # we have predicted values for each response category. Hence,
  # gather columns. Since we also have conf. int. for each response
  # category, we need to gather multiple columns at once

  if (isTRUE(se)) {

    # length of each variable block
    l <- seq_len(ncol(prdat) / 3)
    colnames(fitfram)[l] <- lv

    fitfram <- sjmisc::to_long(
      fitfram,
      keys = "response.level",
      values = c("predicted", "conf.low", "conf.high"),
      l,
      l + length(l),
      l + 2 * length(l)
    )

  } else {
    key_col <- "response.level"
    value_col <- "predicted"

    fitfram <- tidyr::gather(fitfram, !! key_col, !! value_col, !! 1:ncol(prdat))

    # No CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}


# predictions for regression models w/o SE ----

get_predictions_generic2 <- function(model, fitfram, ci.lvl, fun, typical, terms, ...) {
  # get prediction type.
  pt <- dplyr::case_when(
    fun %in% c("hurdle", "zeroinfl") ~ "response",
    TRUE ~ "response"
  )

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975

  # get predictions
  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = pt,
      ...
    )

  fitfram$predicted <- as.vector(prdat)

  # get standard errors from variance-covariance matrix
  se.pred <-
    get_se_from_vcov(
      model = model,
      fitfram = fitfram,
      typical = typical,
      terms = terms,
      fun = fun
    )


  if (!is.null(se.pred)) {
    se.fit <- se.pred$se.fit
    fitfram <- se.pred$fitfram
    # CI
    fitfram$conf.low <- fitfram$predicted - stats::qnorm(ci) * se.fit
    fitfram$conf.high <- fitfram$predicted + stats::qnorm(ci) * se.fit
  } else {
    # CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }


  fitfram
}


# predictions for lrm ----

#' @importFrom stats plogis qnorm
get_predictions_lrm <- function(model, fitfram, ci.lvl, linv, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975

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
    fitfram$conf.low <- stats::plogis(prdat$linear.predictors - stats::qnorm(ci) * prdat$se.fit)
    fitfram$conf.high <- stats::plogis(prdat$linear.predictors + stats::qnorm(ci) * prdat$se.fit)
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
      ...
    )

  # copy predictions
  get_base_fitfram(fitfram, linv, prdat, se, ci.lvl)
}


# predictions for glmmTMB ----

#' @importFrom stats family
get_predictions_glmmTMB <- function(model, fitfram, ci.lvl, linv, type, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975


  # check whether predictions should be conditioned
  # on random effects (grouping level) or not.
  if (type == "fe")
    ref <- NA
  else
    ref <- NULL


  prdat <- stats::predict(
    model,
    newdata = fitfram,
    type = "link",
    se.fit = se,
    # not implemented in glmmTMB <= 0.2.1
    # re.form = ref,
    ...
  )


  # did user request standard errors? if yes, compute CI
  if (se) {
    fitfram$predicted <- linv(prdat$fit)

    # add random effect uncertainty to s.e.
    if (type == "re" && requireNamespace("glmmTMB", quietly = TRUE)) {
      sig <- sum(attr(glmmTMB::VarCorr(model)[[1]], "sc"))
      lf <- get_link_fun(model)
      prdat$se.fit <- prdat$se.fit + lf(sig^2)
    }


    # calculate CI
    fitfram$conf.low <- linv(prdat$fit - stats::qnorm(ci) * prdat$se.fit)
    fitfram$conf.high <- linv(prdat$fit + stats::qnorm(ci) * prdat$se.fit)
  } else {
    # copy predictions
    fitfram$predicted <- linv(as.vector(prdat))

    # no CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}


# predictions for merMod ----

get_predictions_merMod <- function(model, fitfram, ci.lvl, linv, type, terms, typical, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975

  # check whether predictions should be conditioned
  # on random effects (grouping level) or not.
  if (type == "fe")
    ref <- NA
  else
    ref <- NULL


  fitfram$predicted <- stats::predict(
    model,
    newdata = fitfram,
    type = "response",
    re.form = ref,
    ...
  )

  if (se) {
    # get standard errors from variance-covariance matrix
    se.pred <-
      get_se_from_vcov(
        model = model,
        fitfram = fitfram,
        typical = typical,
        terms = terms,
        type = type
      )

    if (!is.null(se.pred)) {
      se.fit <- se.pred$se.fit
      fitfram <- se.pred$fitfram

      if (is.null(linv)) {
        # calculate CI for linear mixed models
        fitfram$conf.low <- fitfram$predicted - stats::qnorm(ci) * se.fit
        fitfram$conf.high <- fitfram$predicted + stats::qnorm(ci) * se.fit
      } else {
        # get link-function and back-transform fitted values
        # to original scale, so we compute proper CI
        lf <- get_link_fun(model)

        # calculate CI for glmm
        fitfram$conf.low <- linv(lf(fitfram$predicted) - stats::qnorm(ci) * se.fit)
        fitfram$conf.high <- linv(lf(fitfram$predicted) + stats::qnorm(ci) * se.fit)
      }
    } else {
      fitfram$conf.low <- NA
      fitfram$conf.high <- NA
    }

  } else {
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}



# predictions for stan ----

#' @importFrom tidyr gather
#' @importFrom tibble as_tibble
#' @importFrom sjstats hdi resp_var resp_val
#' @importFrom sjmisc rotate_df
#' @importFrom purrr map_dbl map_df
#' @importFrom dplyr bind_cols select bind_rows n_distinct
#' @importFrom stats median formula
#' @importFrom tidyselect ends_with
get_predictions_stan <- function(model, fitfram, ci.lvl, type, faminfo, ppd, ...) {
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

  # compute posterior predictions
  if (ppd) {
    # for binomial models, "newdata" also needs a response
    # value. we take the value for a successful event
    if (faminfo$is_bin) {
      resp.name <- sjstats::resp_var(model)
      # successfull events
      fitfram[[resp.name]] <- factor(1)
    }

    if (inherits(model, "brmsfit")) {
      prdat2 <- prdat <- rstantools::posterior_predict(
        model,
        newdata = fitfram,
        re_formula = ref,
        ...
      )
    } else {
      prdat2 <- prdat <- rstantools::posterior_predict(
        model,
        newdata = fitfram,
        re.form = ref,
        ...
      )
    }

  } else {
    # get posterior distribution of the linear predictor
    # note that these are not best practice for inferences,
    # because they don't take the uncertainty of the Sd into account
    prdat <- rstantools::posterior_linpred(
      model,
      newdata = fitfram,
      transform = TRUE,
      re.form = ref,
      re_formula = ref,
      ...
    )


    # tell user
    message("Note: uncertainty of error terms are not taken into account. You may want to use `rstantools::posterior_predict()`.")
  }

  # we have a list of 4000 samples, so we need to coerce to data frame
  prdat <- tibble::as_tibble(prdat)


  # handle cumulative link models

  if (inherits(model, "brmsfit") && faminfo$family %in% c("cumulative", "categorical")) {

    tmp <- prdat %>%
      purrr::map_df(stats::median) %>%
      tidyr::gather(key = "grp", value = "predicted")

    resp.vals <- levels(sjstats::model_frame(model)[[sjstats::resp_var(model)]])
    term.cats <- nrow(fitfram)
    fitfram <- purrr::map_df(1:length(resp.vals), ~ fitfram)

    fitfram$response.level <- rep(unique(resp.vals), each = term.cats)
    fitfram$predicted <- tmp$predicted

  } else if (inherits(model, "brmsfit") && !is.null(stats::formula(model)$responses)) {

    # handle multivariate response models

    tmp <- prdat %>%
      purrr::map_df(stats::median) %>%
      tidyr::gather(key = "grp", value = "predicted")

    resp.vars <- sjstats::resp_var(model)
    fitfram <- purrr::map_df(1:length(resp.vars), ~ fitfram)
    fitfram$response.level <- ""

    for (i in resp.vars) {
      pos <- tidyselect::ends_with(i, vars = tmp$grp)

      if (sjmisc::is_empty(pos)) {
        i <- gsub(pattern = "[\\_\\.]", replacement = "", x = i)
        pos <- tidyselect::ends_with(i, vars = tmp$grp)
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
    tmp <- rstantools::predictive_interval(prdat2)
    hdi <- list(
      tmp[, 1],
      tmp[, 2]
    )
  } else {
    # compute HDI, as alternative to CI
    hdi <- prdat %>%
      purrr::map_df(~ sjstats::hdi(.x, prob = ci.lvl)) %>%
      sjmisc::rotate_df()
  }


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


# predictions for coxph ----

#' @importFrom prediction prediction
get_predictions_coxph <- function(model, fitfram, ci.lvl, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "lp",
      se.fit = se,
      ...
    )

  # did user request standard errors? if yes, compute CI
  if (se) {
    # copy predictions
    fitfram$predicted <- exp(prdat$fit)

    # calculate CI
    fitfram$conf.low <- exp(prdat$fit - stats::qnorm(ci) * prdat$se.fit)
    fitfram$conf.high <- exp(prdat$fit + stats::qnorm(ci) * prdat$se.fit)
  } else {
    # copy predictions
    fitfram$predicted <- exp(as.vector(prdat))

    # no CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}



# predictions for gam ----

#' @importFrom prediction prediction
get_predictions_gam <- function(model, fitfram, ci.lvl, ...) {
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "response",
      se.fit = se
    )

  # did user request standard errors? if yes, compute CI
  if (se) {
    # copy predictions
    fitfram$predicted <- prdat$fit

    # calculate CI
    fitfram$conf.low <- prdat$fit - stats::qnorm(ci) * prdat$se.fit
    fitfram$conf.high <- prdat$fit + stats::qnorm(ci) * prdat$se.fit
  } else {
    # copy predictions
    fitfram$predicted <- as.vector(prdat)

    # no CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}


# predictions for vgam ----

#' @importFrom prediction prediction
get_predictions_vgam <- function(model, fitfram, ci.lvl, linv, ...) {
  prdat <- stats::predict(
    model,
    newdata = fitfram,
    type = "response",
    se.fit = FALSE
  )

  # copy predictions
  fitfram$predicted <- as.vector(prdat)

  fitfram
}


# predictions for lm ----

#' @importFrom dplyr bind_cols
#' @importFrom tidyr gather
get_predictions_lm <- function(model, fitfram, ci.lvl, linv, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "response",
      se.fit = se,
      ...
    )

  # did user request standard errors? if yes, compute CI
  if (se) {
    # copy predictions
    fitfram$predicted <- prdat$fit

    # calculate CI
    fitfram$conf.low <- prdat$fit - stats::qnorm(ci) * prdat$se.fit
    fitfram$conf.high <- prdat$fit + stats::qnorm(ci) * prdat$se.fit
  } else {
    # check if we have a multivariate response model
    pdim <- dim(prdat)
    if (!is.null(pdim) && pdim[2] > 1) {
      tmp <- dplyr::bind_cols(fitfram, as.data.frame(prdat))
      gather.vars <- (ncol(fitfram) + 1):ncol(tmp)

      fitfram <- tidyr::gather(
        tmp,
        key = "response.level",
        value = "predicted",
        !! gather.vars
      )
    } else {
      # copy predictions
      fitfram$predicted <- as.vector(prdat)
    }

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
get_predictions_lme <- function(model, fitfram, ci.lvl, type, terms, typical, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975

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
    se.pred <-
      get_se_from_vcov(
        model = model,
        fitfram = fitfram,
        typical = typical,
        terms = terms,
        type = type
      )

    if (!is.null(se.pred)) {
      se.fit <- se.pred$se.fit
      fitfram <- se.pred$fitfram

      # calculate CI
      fitfram$conf.low <- fitfram$predicted - stats::qnorm(ci) * se.fit
      fitfram$conf.high <- fitfram$predicted + stats::qnorm(ci) * se.fit
    } else {
      # No CI
      fitfram$conf.low <- NA
      fitfram$conf.high <- NA
    }
  } else {
    # No CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}


# predictions for gee ----

get_predictions_gee <- function(model, fitfram, linv, ...) {
  prdat <-
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


# predictions for multinom ----

#' @importFrom tidyr gather
#' @importFrom dplyr bind_cols
get_predictions_multinom <- function(model, fitfram, linv, ...) {
  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "probs",
      ...
    )

  nc <- 1:ncol(prdat)

  # Matrix to vector
  fitfram <- prdat %>%
    as.data.frame() %>%
    dplyr::bind_cols(fitfram) %>%
    tidyr::gather(key = "response.level", value = "predicted", !! nc)

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


get_base_fitfram <- function(fitfram, linv, prdat, se, ci.lvl) {
  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975

  # copy predictions
  if (typeof(prdat) == "double")
    fitfram$predicted <- linv(prdat)
  else
    fitfram$predicted <- linv(prdat$fit)

  # did user request standard errors? if yes, compute CI
  if (se) {
    # calculate CI
    fitfram$conf.low <- linv(prdat$fit - stats::qnorm(ci) * prdat$se.fit)
    fitfram$conf.high <- linv(prdat$fit + stats::qnorm(ci) * prdat$se.fit)
  } else {
    # No CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}



# get standard errors of predictions from model matrix and vcov ----

#' @importFrom tibble add_column
#' @importFrom stats model.matrix terms vcov
#' @importFrom dplyr arrange n_distinct
#' @importFrom sjstats resp_var model_frame
#' @importFrom rlang parse_expr
#' @importFrom purrr map flatten_chr map_lgl
get_se_from_vcov <- function(model, fitfram, typical, terms, fun = NULL, type = "fe") {
  mf <- sjstats::model_frame(model, fe.only = FALSE)

  # copy data frame with predictions
  newdata <- get_expanded_data(
    model,
    mf,
    terms,
    typ.fun = typical,
    fac.typical = FALSE,
    type = type,
    pretty.message = FALSE
  )

  # make sure we have enough values to compute CI
  if (any(purrr::map_lgl(newdata,  ~ is.factor(.x) && nlevels(.x) == 1)))
    return(NULL)

  # add response
  newdata <- tibble::add_column(newdata, response.val = 0)

  # proper column names, needed for getting model matrix
  colnames(newdata)[ncol(newdata)] <- sjstats::resp_var(model)

  # clean terms from brackets
  terms <- get_clear_vars(terms)

  # sort data by grouping levels, so we have the correct order
  # to slice data afterwards
  if (length(terms) > 2) {
    trms <- rlang::parse_expr(terms[3])
    newdata <- dplyr::arrange(newdata, !! trms)
    fitfram <- dplyr::arrange(fitfram, !! trms)
  }

  if (length(terms) > 1) {
    trms <- rlang::parse_expr(terms[2])
    newdata <- dplyr::arrange(newdata, !! trms)
    fitfram <- dplyr::arrange(fitfram, !! trms)
  }

  trms <- rlang::parse_expr(terms[1])
  newdata <- dplyr::arrange(newdata, !! trms)
  fitfram <- dplyr::arrange(fitfram, !! trms)

  # get variance-covariance-matrix, depending on model type
  if (is.null(fun))
    vcm <- as.matrix(stats::vcov(model))
  else if (fun %in% c("hurdle", "zeroinfl"))
    vcm <- as.matrix(stats::vcov(model, model = "count"))
  else if (fun == "betareg")
    vcm <- as.matrix(stats::vcov(model, model = "mean"))
  else if (fun == "truncreg") {
    vcm <- as.matrix(stats::vcov(model))
    # remove sigma from matrix
    vcm <- vcm[1:(nrow(vcm) - 1), 1:(ncol(vcm) - 1)]
  } else
    vcm <- as.matrix(stats::vcov(model))


  # here we need to fix some term names, so variable names
  # match the column names from the model matrix
  terms <- purrr::map(terms, function(.x) {
    if (is.factor(mf[[.x]])) {
      .x <- sprintf("%s%s", .x, levels(mf[[.x]])[2:nlevels(mf[[.x]])])
    }
    .x
  }) %>%
    purrr::flatten_chr()


  # code to compute se of prediction taken from
  # http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
  mm <- stats::model.matrix(stats::terms(model), newdata)
  mmdf <- as.data.frame(mm)
  mm.rows <- as.numeric(rownames(unique(mmdf[intersect(colnames(mmdf), terms)])))
  mm <- mm[mm.rows, ]

  if (!is.null(fun) && fun == "polr") {
    keep <- intersect(colnames(mm), colnames(vcm))
    vcm <- vcm[keep, keep]
    mm <- mm[, keep]
  }

  pvar <- diag(mm %*% vcm %*% t(mm))


  # condition on random effect variances
  if (type == "re") {
    sig <- 0
    if (inherits(model, c("merMod", "lmerMod", "glmerMod")) &&
        requireNamespace("lme4", quietly = TRUE)) {
      sig <- sum(attr(lme4::VarCorr(model), "sc"))
    } else if (inherits(model, c("lme", "nlme"))) {
      sig <- model$sigma
    }
    pvar <- pvar + sig^2
  }

  se.fit <- sqrt(pvar)

  # shorten to length of fitfram
  if (!is.null(fun) && fun == "polr")
    se.fit <- rep(se.fit, each = dplyr::n_distinct(fitfram$response.level))
  else
    se.fit <- se.fit[1:nrow(fitfram)]

  list(fitfram = fitfram, se.fit = se.fit)
}

