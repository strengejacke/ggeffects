# select prediction method, based on model-object
#' @importFrom sjmisc add_variables
#' @importFrom insight find_response get_response get_data model_info link_inverse is_multivariate
select_prediction_method <- function(fun,
                                     model,
                                     expanded_frame,
                                     ci.lvl,
                                     type,
                                     faminfo,
                                     ppd,
                                     terms,
                                     typical,
                                     vcov.fun,
                                     vcov.type,
                                     vcov.args,
                                     condition,
                                     ...) {
  # get link-inverse-function
  linv <- insight::link_inverse(model)
  if (is.null(linv)) linv <- function(x) x

  if (fun == "svyglm") {
    # survey-objects -----
    fitfram <- get_predictions_svyglm(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "svyglm.nb") {
    # survey-glm.nb-objects -----
    fitfram <- get_predictions_svyglmnb(model, expanded_frame, ci.lvl, linv, fun, typical, terms, vcov.fun, vcov.type, vcov.args, condition, ...)
  } else if (fun == "stanreg") {
    # stan-objects -----
    fitfram <- get_predictions_stan(model, expanded_frame, ci.lvl, type, faminfo, ppd, terms, ...)
  } else if (fun == "brmsfit") {
    # brms-objects -----
    fitfram <- get_predictions_stan(model, expanded_frame, ci.lvl, type, faminfo, ppd, terms, ...)
  } else if (fun == "coxph" && type != "surv" && type != "cumhaz") {
    # coxph-objects -----
    fitfram <- get_predictions_coxph(model, expanded_frame, ci.lvl, ...)
  } else if (fun == "coxph" && type %in% c("surv", "cumhaz")) {
    # coxph-objects -----
    fitfram <- get_predictions_survival(model, expanded_frame, ci.lvl, type, terms, ...)
  } else if (fun == "lrm") {
    # lrm-objects -----
    fitfram <- get_predictions_lrm(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "glmmTMB") {
    # glmmTMB-objects -----
    fitfram <- get_predictions_glmmTMB(model, expanded_frame, ci.lvl, linv, type, terms, typical, condition, ...)
  } else if (fun %in% c("lmer", "nlmer", "glmer")) {
    # merMod-objects  -----
    fitfram <- get_predictions_merMod(model, expanded_frame, ci.lvl, linv, type, terms, typical, condition, ...)
  } else if (fun == "gam") {
    # gam-objects -----
    fitfram <- get_predictions_gam(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "Gam") {
    # Gam-objects -----
    fitfram <- get_predictions_Gam(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "vgam") {
    # vgam-objects -----
    fitfram <- get_predictions_vgam(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun %in% c("lme", "gls", "plm")) {
    # lme-objects -----
    fitfram <- get_predictions_lme(model, expanded_frame, ci.lvl, linv, type, terms, typical, condition, ...)
  } else if (fun == "gee") {
    # gee-objects -----
    fitfram <- get_predictions_gee(model, expanded_frame, linv, ...)
  } else if (fun == "multinom") {
    # multinom-objects -----
    fitfram <- get_predictions_multinom(model, expanded_frame, ci.lvl, linv, typical, terms, fun, ...)
  } else if (fun == "clmm") {
    # clmm-objects -----
    fitfram <- get_predictions_clmm(model, terms, typical, condition, ci.lvl, linv, ...)
  } else if (fun == "clm") {
    # clm-objects -----
    fitfram <- get_predictions_clm(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "clm2") {
    # clm2-objects -----
    fitfram <- get_predictions_clm2(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "Zelig-relogit") {
    # Zelig-relogit-objects -----
    fitfram <- get_predictions_zelig(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "polr") {
    # polr-objects -----
    fitfram <- get_predictions_polr(model, expanded_frame, ci.lvl, linv, typical, terms, fun, condition, ...)
  } else if (fun %in% c("betareg", "truncreg")) {
    # betareg, truncreg-objects -----
    fitfram <- get_predictions_generic2(model, expanded_frame, ci.lvl, linv, type, fun, typical, terms, vcov.fun, vcov.type, vcov.args, condition, ...)
  } else if (fun %in% c("zeroinfl", "hurdle", "zerotrunc")) {
    # zeroinfl and hurdle-objects -----
    fitfram <- get_predictions_zeroinfl(model, expanded_frame, ci.lvl, linv, type, fun, typical, terms, vcov.fun, vcov.type, vcov.args, condition, ...)
  } else if (fun %in% c("glm", "glm.nb", "glmRob")) {
    # glm-objects -----
    fitfram <- get_predictions_glm(model, expanded_frame, ci.lvl, linv, typical, fun, terms, vcov.fun, vcov.type, vcov.args, condition, ...)
  } else if (fun == "lm") {
    # lm-objects -----
    fitfram <- get_predictions_lm(model, expanded_frame, ci.lvl, fun, typical, terms, vcov.fun, vcov.type, vcov.args, condition, ...)
  } else if (fun == "MixMod") {
    # MixMod-objects -----
    fitfram <- get_predictions_MixMod(model, expanded_frame, ci.lvl, linv, type, terms, typical, condition, ...)
  } else if (fun == "MCMCglmm") {
    # MCMCglmm-objects -----
    fitfram <- get_predictions_MCMCglmm(model, expanded_frame, ci.lvl, ...)
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
    ci <- (1 + ci.lvl) / 2
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


# predictions for glm ----

get_predictions_glm <- function(model, fitfram, ci.lvl, linv, typical, fun, terms, vcov.fun, vcov.type, vcov.args, condition, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl) && is.null(vcov.fun)

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
  get_base_fitfram(model, fitfram, linv, prdat, se, ci.lvl, fun, typical, terms, vcov.fun, vcov.type, vcov.args, condition)
}


# predictions for MixMod ----

get_predictions_MixMod <- function(model, fitfram, ci.lvl, linv, type, terms, typical, condition, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  # get info about model
  modfam <- insight::model_info(model)

  if (!modfam$is_zeroinf && type %in% c("fe.zi", "re.zi")) {
    if (type == "fe.zi")
      type <- "fe"
    else
      type <- "re"

    message(sprintf("Model has no zero-inflation part. Changing prediction-type to \"%s\".", type))
  }

  if (modfam$is_zeroinf && type %in% c("fe", "re")) {
    if (type == "fe")
      type <- "fe.zi"
    else
      type <- "re.zi"

    message(sprintf("Model has zero-inflation part, predicted values can only be conditioned on zero-inflation part. Changing prediction-type to \"%s\".", type))
  }

  prtype <- dplyr::case_when(
    type %in% c("fe", "fe.zi") ~ "mean_subject",
    type %in% c("re", "re.zi") ~ "subject_specific",
    TRUE ~ "mean_subject"
  )

  prdat <- stats::predict(
    model,
    newdata = fitfram,
    type = prtype,
    type_pred = "response",
    se.fit = se,
    level = ci.lvl,
    ...
  )

  fitfram$predicted <- prdat$pred


  if (modfam$is_zeroinf && prtype == "mean_subject") {
    add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

    if ("nsim" %in% names(add.args))
      nsim <- eval(add.args[["nsim"]])
    else
      nsim <- 1000

    mf <- insight::get_data(model)
    clean_terms <- get_clear_vars(terms)

    newdata <- get_expanded_data(
      model = model,
      mf = mf,
      terms = terms,
      typ.fun = typical,
      fac.typical = FALSE,
      pretty.message = FALSE,
      condition = condition
    )

    prdat.sim <- get_MixMod_predictions(model, newdata, nsim, terms, typical, condition)

    if (is.null(prdat.sim) || inherits(prdat.sim, c("error", "simpleError"))) {
      cat(.colour("red", "Error: Confidence intervals could not be computed.\n"))
      if (inherits(prdat.sim, c("error", "simpleError"))) {
        cat(sprintf("* Reason: %s\n", deparse(prdat.sim[[1]], width.cutoff = 500)))
        cat(sprintf("* Source: %s\n", deparse(prdat.sim[[2]], width.cutoff = 500)))
      }

      fitfram$conf.low <- NA
      fitfram$conf.high <- NA
    } else {
      sims <- exp(prdat.sim$cond) * (1 - stats::plogis(prdat.sim$zi))
      fitfram <- get_zeroinfl_fitfram(fitfram, newdata, prdat, sims, ci, clean_terms)
    }
  } else {
    if (obj_has_name(prdat, "upp")) {
      fitfram$conf.low <- prdat$low
      fitfram$conf.high <- prdat$upp
    } else if (!is.null(prdat$se.fit)) {
      lf <- get_link_fun(model)
      if (is.null(lf)) lf <- function(x) x
      fitfram$conf.low <- linv(lf(fitfram$predicted) - stats::qnorm(ci) * prdat$se.fit)
      fitfram$conf.high <- linv(lf(fitfram$predicted) + stats::qnorm(ci) * prdat$se.fit)
    } else {
      fitfram$conf.low <- NA
      fitfram$conf.high <- NA
    }
  }


  # copy standard errors
  attr(fitfram, "std.error") <- prdat$se.fit

  fitfram
}


# predictions for polr ----

#' @importFrom dplyr bind_cols bind_rows
#' @importFrom rlang .data
get_predictions_polr <- function(model, fitfram, ci.lvl, linv, typical, terms, fun, condition, ...) {

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
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
    return(rownames_as_column(prdat, var = "response.level"))
  }

  # bind predictions to model frame
  fitfram <- dplyr::bind_cols(prdat, fitfram)

  # for proportional ordinal logistic regression (see MASS::polr),
  # we have predicted values for each response category. Hence,
  # gather columns

  fitfram <- .gather(fitfram, "response.level", "predicted", colnames(prdat))

  se.pred <-
    get_se_from_vcov(
      model = model,
      fitfram = fitfram,
      typical = typical,
      terms = terms,
      fun = fun,
      condition = condition
    )

  if (!is.null(se.pred)) {

    se.fit <- se.pred$se.fit
    fitfram <- se.pred$fitfram

    # CI
    fitfram$conf.low <- linv(stats::qlogis(fitfram$predicted) - stats::qnorm(ci) * se.fit)
    fitfram$conf.high <- linv(stats::qlogis(fitfram$predicted) + stats::qnorm(ci) * se.fit)

    # copy standard errors
    attr(fitfram, "std.error") <- se.fit

  } else {
    # CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}


# predictions for Zelig-relogit model ----

get_predictions_zelig <- function(model, fitfram, ci.lvl, linv, ...) {

  stop("`ggpredict()` does currently not support Zelig-models.", call. = FALSE)

  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
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


# predictions for cumulative link mixed model ----

#' @importFrom stats confint
#' @importFrom sjmisc var_rename
get_predictions_clmm <- function(model, terms, typical, condition, ci.lvl, linv, ...) {

  if (!requireNamespace("emmeans")) {
    stop("Package `emmeans` required to compute marginal effects for clmm-models.", call. = FALSE)
  }

  values.at <- get_expanded_data(
    model = model,
    mf = insight::get_data(model),
    terms = terms,
    typ.fun = typical,
    condition = condition,
    pretty.message = FALSE,
    emmeans.only = TRUE
  )

  fitfram <- emmeans::emmeans(
    object = model,
    spec = c(insight::find_response(model, combine = FALSE), get_clear_vars(terms)),
    at = values.at,
    mode = "prob"
  ) %>%
    stats::confint(level = ci.lvl) %>%
    as.data.frame() %>%
    sjmisc::var_rename(
      prob = "predicted",
      SE = "std.error",
      asymp.LCL = "conf.low",
      asymp.UCL = "conf.high"
    )

  colnames(fitfram)[1] <- "response.level"

  # copy standard errors
  attr(fitfram, "std.error") <- fitfram$std.error

  fitfram
}


# predictions for cumulative link model ----

#' @importFrom sjmisc to_long
get_predictions_clm <- function(model, fitfram, ci.lvl, linv, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
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
  lv <- levels(insight::get_response(model))

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
    fitfram <- .gather(fitfram, "response.level", "predicted", colnames(prdat))
    # No CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}


# predictions for cumulative link model2 ----

#' @importFrom sjmisc to_long
get_predictions_clm2 <- function(model, fitfram, ci.lvl, linv, ...) {

  stop("`ggpredict()` does currently not support clm2-models.", call. = FALSE)

  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  fitfram <- sjmisc::add_variables(fitfram, as.factor(insight::get_response(model)), .before = 1)
  colnames(fitfram)[1] <- insight::find_response(model)

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
  lv <- levels(insight::get_response(model))

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
    fitfram <- .gather(fitfram, "response.level", "predicted", colnames(prdat))
    # No CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}


# predictions for regression models w/o SE ----

#' @importFrom stats qlogis
get_predictions_generic2 <- function(model, fitfram, ci.lvl, linv, type, fun, typical, terms, vcov.fun, vcov.type, vcov.args, condition, ...) {
  # get prediction type.
  pt <- dplyr::case_when(
    fun == "betareg" ~ "link",
    TRUE ~ "response"
  )

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
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


  fitfram
}


# predictions for zero-inflated and hurdle-models ----

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

      cat(.colour("red", "Error: Confidence intervals could not be computed.\n"))
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


# predictions for lrm ----

#' @importFrom stats plogis qnorm
get_predictions_lrm <- function(model, fitfram, ci.lvl, linv, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
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

    # copy standard errors
    attr(fitfram, "std.error") <- prdat$se.fit

  } else {
    # No CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}


# predictions for svyglm.nb ----

get_predictions_svyglmnb <- function(model, fitfram, ci.lvl, linv, fun, typical, terms, vcov.fun, vcov.type, vcov.args, condition, ...) {
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
  get_base_fitfram(model, fitfram, linv, prdat, se, ci.lvl, fun, typical, terms, vcov.fun, vcov.type, vcov.args, condition)
}


# predictions for glmmTMB ----

#' @importFrom dplyr select
#' @importFrom stats predict qnorm plogis
get_predictions_glmmTMB <- function(model, fitfram, ci.lvl, linv, type, terms, typical, condition, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975


  # check if we have zero-inflated model part

  modfam <- insight::model_info(model)
  clean_terms <- get_clear_vars(terms)

  if (!modfam$is_zeroinf && type %in% c("fe.zi", "re.zi")) {
    if (type == "fe.zi")
      type <- "fe"
    else
      type <- "re"

    message(sprintf("Model has no zero-inflation part. Changing prediction-type to \"%s\".", type))
  }


  # check whether predictions should be conditioned
  # on random effects (grouping level) or not.

  if (type %in% c("fe", "fe.zi"))
    ref <- NA
  else
    ref <- NULL


  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

  if ("nsim" %in% names(add.args))
    nsim <- eval(add.args[["nsim"]])
  else
    nsim <- 1000


  # predictions conditioned on zero-inflation component

  if (type %in% c("fe.zi", "re.zi")) {

    prdat <- as.vector(stats::predict(
      model,
      newdata = fitfram,
      type = "response",
      se.fit = FALSE,
      ## FIXME not implemented in glmmTMB <= 0.2.2
      # re.form = ref,
      ...
    ))

    if (!se) {

      fitfram$predicted <- prdat
      fitfram$conf.low <- NA
      fitfram$conf.high <- NA

    } else {

      mf <- insight::get_data(model)

      newdata <- get_expanded_data(
        model = model,
        mf = mf,
        terms = terms,
        typ.fun = typical,
        fac.typical = FALSE,
        pretty.message = FALSE,
        condition = condition
      )

      prdat.sim <- get_glmmTMB_predictions(model, newdata, nsim, terms, typical, condition)

      if (is.null(prdat.sim) || inherits(prdat.sim, c("error", "simpleError"))) {

        cat(.colour("red", "Error: Confidence intervals could not be computed.\n"))
        if (inherits(prdat.sim, c("error", "simpleError"))) {
          cat(sprintf("* Reason: %s\n", deparse(prdat.sim[[1]], width.cutoff = 500)))
          cat(sprintf("* Source: %s\n", deparse(prdat.sim[[2]], width.cutoff = 500)))
        }

        fitfram$predicted <- prdat
        fitfram$conf.low <- NA
        fitfram$conf.high <- NA

      } else {

        sims <- exp(prdat.sim$cond) * (1 - stats::plogis(prdat.sim$zi))
        fitfram <- get_zeroinfl_fitfram(fitfram, newdata, prdat, sims, ci, clean_terms)

        if (type == "re.zi") {
          revar <- getVarRand(model)
          # get link-function and back-transform fitted values
          # to original scale, so we compute proper CI
          lf <- get_link_fun(model)
          fitfram$conf.low <- exp(lf(fitfram$conf.low) - stats::qnorm(ci) * sqrt(revar))
          fitfram$conf.high <- exp(lf(fitfram$conf.high) + stats::qnorm(ci) * sqrt(revar))
          fitfram$std.error <- sqrt(fitfram$std.error^2 + revar)
        }
      }
    }

  } else if (type == "sim") {

    # predictions conditioned on zero-inflation component and random
    # effects, based on simulations
    fitfram <- simulate_predictions(model, nsim, clean_terms, ci)

  } else {

    # predictions conditioned on count component only

    prdat <- stats::predict(
      model,
      newdata = fitfram,
      type = "link",
      se.fit = se,
      ## FIXME not implemented in glmmTMB <= 0.2.2
      ## TODO once this is fixed, update docs in ggpredict, argument type
      # re.form = ref,
      ...
    )

    # did user request standard errors? if yes, compute CI
    if (se) {
      fitfram$predicted <- linv(prdat$fit)

      # add random effect uncertainty to s.e.
      if (type %in% c("re", "re.zi")) {
        pvar <- prdat$se.fit^2
        prdat$se.fit <- sqrt(pvar + getVarRand(model))
      }

      # calculate CI
      fitfram$conf.low <- linv(prdat$fit - stats::qnorm(ci) * prdat$se.fit)
      fitfram$conf.high <- linv(prdat$fit + stats::qnorm(ci) * prdat$se.fit)
      fitfram$std.error <- prdat$se.fit
    } else {
      # copy predictions
      fitfram$predicted <- linv(as.vector(prdat))

      # no CI
      fitfram$conf.low <- NA
      fitfram$conf.high <- NA
    }
  }

  if (obj_has_name(fitfram, "std.error")) {
    # copy standard errors
    attr(fitfram, "std.error") <- fitfram$std.error
    fitfram <- dplyr::select(fitfram, -.data$std.error)
  }

  fitfram
}


# predictions for merMod ----

get_predictions_merMod <- function(model, fitfram, ci.lvl, linv, type, terms, typical, condition, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  # check whether predictions should be conditioned
  # on random effects (grouping level) or not.
  if (type == "fe")
    ref <- NA
  else
    ref <- NULL

  clean_terms <- get_clear_vars(terms)

  if (type == "sim") {

    add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

    if ("nsim" %in% names(add.args))
      nsim <- eval(add.args[["nsim"]])
    else
      nsim <- 1000

    fitfram <- simulate_predictions(model, nsim, clean_terms, ci)

  } else {


    fitfram$predicted <- stats::predict(
      model,
      newdata = fitfram,
      type = "response",
      re.form = ref,
      allow.new.levels = TRUE,
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
          type = type,
          condition = condition
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

        # copy standard errors
        attr(fitfram, "std.error") <- se.fit

      } else {
        fitfram$conf.low <- NA
        fitfram$conf.high <- NA
      }

    } else {
      fitfram$conf.low <- NA
      fitfram$conf.high <- NA
    }

  }

  fitfram
}



# predictions for stan ----

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


    # tell user
    message("Note: uncertainty of error terms are not taken into account. You may want to use `rstantools::posterior_predict()`.")
  }

  # we have a list of 4000 samples, so we need to coerce to data frame
  prdat <- as.data.frame(prdat)


  # handle cumulative link models

  if (inherits(model, "brmsfit") && faminfo$family %in% c("cumulative", "categorical")) {

    tmp <- prdat %>%
      purrr::map_df(stats::median) %>%
      .gather(key = "grp", value = "predicted", colnames(.))

    resp.vals <- levels(insight::get_response(model))
    term.cats <- nrow(fitfram)
    fitfram <- purrr::map_df(1:length(resp.vals), ~ fitfram)

    fitfram$response.level <- rep(unique(resp.vals), each = term.cats)
    fitfram$predicted <- tmp$predicted

  } else if (insight::is_multivariate(model)) {

    # handle multivariate response models

    tmp <- prdat %>%
      purrr::map_df(stats::median) %>%
      .gather(key = "grp", value = "predicted", colnames(.))

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


# predictions for coxph ----

#' @importFrom prediction prediction
get_predictions_coxph <- function(model, fitfram, ci.lvl, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
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

    # copy standard errors
    attr(fitfram, "std.error") <- prdat$se.fit

  } else {
    # copy predictions
    fitfram$predicted <- exp(as.vector(prdat))

    # no CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}


# predictions for coxph, survival and cumulative hazard ----

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

  clean_terms <- get_clear_vars(terms)
  ff <- fitfram[clean_terms]

  purrr::map_df(sjmisc::seq_row(ff), function(i) {

    dat <- data.frame(
      time = prdat$time,
      predicted = pr[, i],
      conf.low = lower[, i],
      conf.high = upper[, i]
    )

    dat2 <- purrr::map(sjmisc::seq_col(ff), ~ ff[i, .x])
    names(dat2) <- clean_terms
    dat2 <- data.frame(dat2, stringsAsFactors = FALSE)

    cbind(dat[, 1, drop = FALSE], dat2, dat[, 2:4])
  })
}


# predictions for gam ----

#' @importFrom prediction prediction
get_predictions_gam <- function(model, fitfram, ci.lvl, linv, ...) {
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "link",
      se.fit = se
    )

  # did user request standard errors? if yes, compute CI
  if (se) {
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


# predictions for Gam ----

#' @importFrom prediction prediction
get_predictions_Gam <- function(model, fitfram, ci.lvl, linv, ...) {
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)
  se <- FALSE

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "link",
      se.fit = se
    )

  # did user request standard errors? if yes, compute CI
  if (se) {
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


# predictions for vgam ----

#' @importFrom prediction prediction
get_predictions_vgam <- function(model, fitfram, ci.lvl, linv, ...) {
  prdat <- stats::predict(
    model,
    newdata = fitfram,
    type = "link",
    se.fit = FALSE
  )

  # copy predictions
  fitfram$predicted <- linv(as.vector(prdat))

  fitfram
}


# predictions for lm ----

#' @importFrom dplyr bind_cols
get_predictions_lm <- function(model, fitfram, ci.lvl, fun, typical, terms, vcov.fun, vcov.type, vcov.args, condition, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl) && is.null(vcov.fun)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
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
  if (!is.null(vcov.fun)) {
    # copy predictions
    fitfram$predicted <- as.vector(prdat)

    se.pred <-
      get_se_from_vcov(
        model = model,
        fitfram = fitfram,
        typical = typical,
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
      fitfram$conf.low <- fitfram$predicted - stats::qnorm(ci) * se.fit
      fitfram$conf.high <- fitfram$predicted + stats::qnorm(ci) * se.fit

      # copy standard errors
      attr(fitfram, "std.error") <- se.fit

    } else {
      # CI
      fitfram$conf.low <- NA
      fitfram$conf.high <- NA
    }
  } else if (se) {
    # copy predictions
    fitfram$predicted <- prdat$fit

    # calculate CI
    fitfram$conf.low <- prdat$fit - stats::qnorm(ci) * prdat$se.fit
    fitfram$conf.high <- prdat$fit + stats::qnorm(ci) * prdat$se.fit

    # copy standard errors
    attr(fitfram, "std.error") <- prdat$se.fit

  } else {
    # check if we have a multivariate response model
    pdim <- dim(prdat)
    if (!is.null(pdim) && pdim[2] > 1) {
      tmp <- dplyr::bind_cols(fitfram, as.data.frame(prdat))
      gather.vars <- (ncol(fitfram) + 1):ncol(tmp)

      fitfram <- .gather(
        tmp,
        key = "response.level",
        value = "predicted",
        colnames(tmp)[gather.vars]
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


# predictions for MCMCglmm ----

get_predictions_MCMCglmm <- function(model, fitfram, ci.lvl, ...) {
  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "response",
      interval = "confidence",
      level = ci.lvl,
      ...
    )

  fitfram$predicted <- prdat[, 1]
  fitfram$conf.low <- prdat[, 2]
  fitfram$conf.high <- prdat[, 3]

  # copy standard errors
  attr(fitfram, "std.error") <- NULL

  fitfram
}


# predictions for lme ----

#' @importFrom stats model.matrix formula vcov
#' @importFrom purrr map
get_predictions_lme <- function(model, fitfram, ci.lvl, linv, type, terms, typical, condition, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975


  if (inherits(model, "glmmPQL"))
    pr.type <- "link"
  else
    pr.type <- "response"

  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = pr.type,
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
        type = type,
        condition = condition
      )

    if (!is.null(se.pred)) {

      se.fit <- se.pred$se.fit
      fitfram <- se.pred$fitfram

      # calculate CI
      fitfram$conf.low <- fitfram$predicted - stats::qnorm(ci) * se.fit
      fitfram$conf.high <- fitfram$predicted + stats::qnorm(ci) * se.fit

      # copy standard errors
      attr(fitfram, "std.error") <- se.fit

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

  # for glmmPQL, we need to back-transform using link-inverse

  if (inherits(model, "glmmPQL")) {
    fitfram$predicted <- linv(fitfram$predicted)
    fitfram$conf.low <- linv(fitfram$conf.low)
    fitfram$conf.high <- linv(fitfram$conf.high)
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

#' @importFrom dplyr bind_cols
get_predictions_multinom <- function(model, fitfram, ci.lvl, linv, typical, terms, fun, ...) {

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975


  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = "probs",
      ...
    )

  if (is.data.frame(prdat) || is.matrix(prdat))
    nc <- 1:ncol(prdat)
  else
    nc <- 1

  # Matrix to vector
  tmp <- prdat %>%
    as.data.frame() %>%
    dplyr::bind_cols(fitfram)

  fitfram <- .gather(tmp, key = "response.level", value = "predicted", colnames(tmp)[nc])


  # se.pred <-
  #   get_se_from_vcov(
  #     model = model,
  #     fitfram = fitfram,
  #     typical = typical,
  #     terms = terms,
  #     fun = fun
  #   )
  #
  # if (!is.null(se.pred)) {
  #   se.fit <- se.pred$se.fit
  #   fitfram <- se.pred$fitfram
  #   # CI
  #   fitfram$conf.low <- linv(stats::qlogis(fitfram$predicted) - stats::qnorm(ci) * se.fit)
  #   fitfram$conf.high <- linv(stats::qlogis(fitfram$predicted) + stats::qnorm(ci) * se.fit)
  # } else {
  #   # No CI
  #   fitfram$conf.low <- NA
  #   fitfram$conf.high <- NA
  # }

  # No CI
  fitfram$conf.low <- NA
  fitfram$conf.high <- NA

  fitfram
}


# predictions for generic models ----

#' @importFrom sjmisc var_rename
get_predictions_generic <- function(model, fitfram, linv, ...) {

  if (!requireNamespace("prediction", quietly = TRUE)) {
    stop("You need to install package `prediction` first to compute marginal effects.", call. = FALSE)
  }

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


get_base_fitfram <- function(model, fitfram, linv, prdat, se, ci.lvl, fun, typical, terms, vcov.fun, vcov.type, vcov.args, condition = NULL) {

  # compute ci, two-ways

  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975


  # copy predictions

  if (typeof(prdat) == "double")
    .predicted <- prdat
  else
    .predicted <- prdat$fit


  # get standard errors, if computed

  if (obj_has_name(prdat, "se.fit"))
    se.fit <- prdat$se.fit
  else
    se.fit <- NULL

  # get predicted values, on link-scale
  fitfram$predicted <- .predicted

  # did user request robust standard errors?

  if (!is.null(vcov.fun)) {
    se.pred <-
      get_se_from_vcov(
        model = model,
        fitfram = fitfram,
        typical = typical,
        terms = terms,
        fun = fun,
        vcov.fun = vcov.fun,
        vcov.type = vcov.type,
        vcov.args = vcov.args,
        condition = condition
      )

    if (!is.null(se.pred)) {
      fitfram <- se.pred$fitfram
      se.fit <- se.pred$se.fit
      se <- TRUE
    } else {
      se.fit <- NULL
      se <- FALSE
    }
  }


  # did user request standard errors? if yes, compute CI

  if (se && !is.null(se.fit)) {
    fitfram$conf.low <- linv(fitfram$predicted - stats::qnorm(ci) * se.fit)
    fitfram$conf.high <- linv(fitfram$predicted + stats::qnorm(ci) * se.fit)
    # copy standard errors
    attr(fitfram, "std.error") <- se.fit
  } else {
    # No CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  # transform predicted values
  fitfram$predicted <- linv(fitfram$predicted)

  fitfram
}
