# select prediction method, based on model-object
#' @importFrom sjstats link_inverse
#' @importFrom sjmisc add_variables
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
                                     ...) {
  # get link-inverse-function
  linv <- sjstats::link_inverse(model)
  if (is.null(linv)) linv <- function(x) x

  if (fun == "svyglm") {
    # survey-objects -----
    fitfram <- get_predictions_svyglm(model, expanded_frame, ci.lvl, linv, ...)
  } else if (fun == "svyglm.nb") {
    # survey-glm.nb-objects -----
    fitfram <- get_predictions_svyglmnb(model, expanded_frame, ci.lvl, linv, fun, typical, terms, vcov.fun, vcov.type, vcov.args, ...)
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
    fitfram <- get_predictions_glmmTMB(model, expanded_frame, ci.lvl, linv, type, terms, typical, ...)
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
    fitfram <- get_predictions_multinom(model, expanded_frame, ci.lvl, linv, typical, terms, fun, ...)
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
    fitfram <- get_predictions_polr(model, expanded_frame, ci.lvl, linv, typical, terms, fun, ...)
  } else if (fun %in% c("betareg", "truncreg", "zeroinfl", "hurdle")) {
    # betareg, truncreg, zeroinfl and hurdle-objects -----
    fitfram <- get_predictions_generic2(model, expanded_frame, ci.lvl, linv, type, fun, typical, terms, vcov.fun, vcov.type, vcov.args, ...)
  } else if (fun %in% c("glm", "glm.nb", "glmRob")) {
    # glm-objects -----
    fitfram <- get_predictions_glm(model, expanded_frame, ci.lvl, linv, typical, fun, terms, vcov.fun, vcov.type, vcov.args, ...)
  } else if (fun == "lm") {
    # lm-objects -----
    fitfram <- get_predictions_lm(model, expanded_frame, ci.lvl, fun, typical, terms, vcov.fun, vcov.type, vcov.args, ...)
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

get_predictions_glm <- function(model, fitfram, ci.lvl, linv, typical, fun, terms, vcov.fun, vcov.type, vcov.args, ...) {
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
  get_base_fitfram(model, fitfram, linv, prdat, se, ci.lvl, fun, typical, terms, vcov.fun, vcov.type, vcov.args)
}


# predictions for polr ----

#' @importFrom tidyr gather
#' @importFrom dplyr bind_cols bind_rows
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
    return(rownames_as_column(prdat, var = "response.level"))
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

  stop("`ggpredict()` does currently not support Zelig-models.", call. = FALSE)

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


# predictions for cumulative link model2 ----

#' @importFrom sjmisc to_long
#' @importFrom sjstats resp_var resp_val
get_predictions_clm2 <- function(model, fitfram, ci.lvl, linv, ...) {

  stop("`ggpredict()` does currently not support clm2-models.", call. = FALSE)

  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975

  fitfram <- sjmisc::add_variables(fitfram, as.factor(sjstats::resp_val(model)), .before = 1)
  colnames(fitfram)[1] <- sjstats::resp_var(model)

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

#' @importFrom stats qlogis
get_predictions_generic2 <- function(model, fitfram, ci.lvl, linv, type, fun, typical, terms, vcov.fun, vcov.type, vcov.args, ...) {
  # get prediction type.
  pt <- dplyr::case_when(
    fun == "zeroinfl" && type == "fe" ~ "count",
    fun == "zeroinfl" && type == "fe.zi" ~ "response",
    fun == "hurdle" && type == "fe" ~ "count",
    fun == "hurdle" && type == "fe.zi" ~ "response",
    fun == "betareg" ~ "link",
    TRUE ~ "response"
  )

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975


  # if (type == "debug") {
  #   mf <- sjstats::model_frame(model)
  #
  #   newdata <- get_expanded_data(
  #     model,
  #     mf,
  #     terms,
  #     typ.fun = typical,
  #     fac.typical = FALSE,
  #     pretty.message = FALSE
  #   )
  #
  #   condformula <- as.formula(paste0("~", deparse(stats::formula(model)[[3]][[2]])))
  #   x.cond <- stats::model.matrix(condformula, model = "count", data = newdata)
  #   beta.cond <- stats::coef(model, model = "count")
  #
  #   ziformula <- as.formula(paste0("~", deparse(stats::formula(model)[[3]][[3]])))
  #   x.zi <- stats::model.matrix(ziformula, model = "zero", data = newdata)
  #   beta.zi <- stats::coef(model, model = "zero")
  #
  #   pred.condpar.psim <- MASS::mvrnorm(1000, mu = beta.cond, Sigma = stats::vcov(model, model = "count"))
  #   pred.cond.psim <- x.cond %*% t(pred.condpar.psim)
  #   pred.zipar.psim <- MASS::mvrnorm(1000, mu = beta.zi, Sigma = stats::vcov(model, model = "zero"))
  #   pred.zi.psim <- x.zi %*% t(pred.zipar.psim)
  #   pred.ucount.psim <- exp(pred.cond.psim) * (1 - stats::plogis(pred.zi.psim))
  #
  #   fitfram <- newdata
  #   fitfram$predictions <- apply(pred.ucount.psim, 1, mean)
  #   fitfram$conf.low <- apply(pred.ucount.psim, 1, quantile, 1 - ci)
  #   fitfram$conf.high <- apply(pred.ucount.psim, 1, quantile, ci)
  #
  #   grp <- rlang::syms(terms)
  #   fitfram <- fitfram %>%
  #     dplyr::group_by(!!! grp) %>%
  #     dplyr::summarize(
  #       predicted = mean(.data$predictions),
  #       conf.low = mean(.data$conf.low),
  #       conf.high = mean(.data$conf.high)
  #     )
  #
  #   return(fitfram)
  #
  # }

  # get predictions
  prdat <-
    stats::predict(
      model,
      newdata = fitfram,
      type = pt,
      ...
    )


  fitfram$predicted <- as.vector(prdat)


  # for some models, we need some backtransformation

  if (fun %in% c("zeroinfl", "hurdle")) {
    fitfram$predicted <- log(fitfram$predicted)
  }


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
      vcov.args = vcov.args
    )


  if (!is.null(se.pred)) {
    se.fit <- se.pred$se.fit
    fitfram <- se.pred$fitfram
    # CI
    fitfram$conf.low <- linv(fitfram$predicted - stats::qnorm(ci) * se.fit)
    fitfram$conf.high <- linv(fitfram$predicted + stats::qnorm(ci) * se.fit)
  } else {
    # CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram$predicted <- linv(fitfram$predicted)


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

get_predictions_svyglmnb <- function(model, fitfram, ci.lvl, linv, fun, typical, terms, vcov.fun, vcov.type, vcov.args, ...) {
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
  get_base_fitfram(model, fitfram, linv, prdat, se, ci.lvl, fun, typical, terms, vcov.fun, vcov.type, vcov.args)
}


# predictions for glmmTMB ----

#' @importFrom stats predict qnorm family model.matrix formula terms vcov plogis simulate
#' @importFrom sjstats model_frame
#' @importFrom lme4 fixef nobars
#' @importFrom MASS mvrnorm
#' @importFrom dplyr group_by summarize
#' @importFrom rlang syms
get_predictions_glmmTMB <- function(model, fitfram, ci.lvl, linv, type, terms, typical, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975


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


  if (type == "fe.zi") {

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

      mf <- sjstats::model_frame(model)

      newdata <- get_expanded_data(
        model,
        mf,
        terms,
        typ.fun = typical,
        fac.typical = FALSE,
        pretty.message = FALSE
      )

      prdat.sim <- get_glmmTMB_predictions(model, newdata, nsim)

      if (is.null(prdat.sim))
        stop("Predicted values could not be computed. Try reducing number of simulation, using argument `nsim` (e.g. `nsim = 100`)", call. = FALSE)

      sims <- exp(prdat.sim$cond) * (1 - stats::plogis(prdat.sim$zi))
      fitfram <- newdata

      fitfram$predicted <- apply(sims, 1, mean)
      fitfram$conf.low <- apply(sims, 1, quantile, probs = 1 - ci)
      fitfram$conf.high <- apply(sims, 1, quantile, probs = ci)

      grp <- rlang::syms(terms)
      fitfram <- fitfram %>%
        dplyr::group_by(!!! grp) %>%
        dplyr::summarize(
          predicted = mean(.data$predicted),
          conf.low = mean(.data$conf.low),
          conf.high = mean(.data$conf.high)
        )

      # we use the predicted values from "predict(type = "reponse")", but the
      # bootstrapped CI - so we need to fix a bit here

      if (length(prdat) == nrow(fitfram)) {
        ci.range <- (fitfram$conf.high - fitfram$conf.low) / 2
        fitfram$predicted <- prdat
        fitfram$conf.low <- fitfram$predicted - ci.range
        fitfram$conf.high <- fitfram$predicted + ci.range
      }
    }
  } else if (type == "re.zi") {

    sims <- stats::simulate(model, nsim = nsim)
    fitfram <- sjstats::model_frame(model)

    fitfram$predicted <- apply(sims, 1, mean)
    fitfram$conf.low <- apply(sims, 1, quantile, probs = 1 - ci)
    fitfram$conf.high <- apply(sims, 1, quantile, probs = ci)

    grp <- rlang::syms(terms)
    fitfram <- fitfram %>%
      dplyr::group_by(!!! grp) %>%
      dplyr::summarize(
        predicted = mean(.data$predicted),
        conf.low = mean(.data$conf.low),
        conf.high = mean(.data$conf.high)
      )

  } else {
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

    # we need backtransformation for re-variance...
    lf <- get_link_fun(model)

    # did user request standard errors? if yes, compute CI
    if (se) {
      fitfram$predicted <- linv(prdat$fit)

      # add random effect uncertainty to s.e.
      if (type %in% c("re", "re.zi") && requireNamespace("glmmTMB", quietly = TRUE)) {
        sig <- sum(attr(glmmTMB::VarCorr(model)[[1]], "sc"))
        res.var <- lf(sig^2)
        if (is.null(res.var) || is.infinite(res.var) || is.na(res.var)) res.var <- 1
        prdat$se.fit <- prdat$se.fit + res.var + getVarRand(model)
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
  }

  fitfram
}


get_glmmTMB_predictions <- function(model, newdata, nsim) {
  tryCatch(
    {
      x.cond <- stats::model.matrix(lme4::nobars(stats::formula(model)[-2]), newdata)
      beta.cond <- lme4::fixef(model)$cond

      ziformula <- model$modelInfo$allForm$ziformula
      x.zi <- stats::model.matrix(stats::terms(ziformula), newdata)
      beta.zi <- lme4::fixef(model)$zi

      pred.condpar.psim <- MASS::mvrnorm(n = nsim, mu = beta.cond, Sigma = stats::vcov(model)$cond)
      pred.cond.psim <- x.cond %*% t(pred.condpar.psim)
      pred.zipar.psim <- MASS::mvrnorm(n = nsim, mu = beta.zi, Sigma = stats::vcov(model)$zi)
      pred.zi.psim <- x.zi %*% t(pred.zipar.psim)

      list(cond = pred.cond.psim, zi = pred.zi.psim)
    },
    error = function(x) { NULL },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )
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
#' @importFrom sjstats hdi resp_var resp_val
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
  # predictions for monotonic

  if (!is.null(terms)) {
    mf <- sjstats::model_frame(model)
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
    if (faminfo$is_bin) {
      resp.name <- sjstats::resp_var(model)
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
  prdat <- as.data.frame(prdat)


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
      pos <- string_ends_with(pattern = i, x = tmp$grp)

      if (sjmisc::is_empty(pos)) {
        i <- gsub(pattern = "[\\_\\.]", replacement = "", x = i)
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


# predictions for coxph, survival and cumulative hazard ----

get_predictions_survival <- function(model, fitfram, ci.lvl, type, terms, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
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
get_predictions_lm <- function(model, fitfram, ci.lvl, fun, typical, terms, vcov.fun, vcov.type, vcov.args, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl) && is.null(vcov.fun)

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
        vcov.args = vcov.args
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
  } else if (se) {
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
get_predictions_multinom <- function(model, fitfram, ci.lvl, linv, typical, terms, fun, ...) {

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

  if (is.data.frame(prdat) || is.matrix(prdat))
    nc <- 1:ncol(prdat)
  else
    nc <- 1

  # Matrix to vector
  fitfram <- prdat %>%
    as.data.frame() %>%
    dplyr::bind_cols(fitfram) %>%
    tidyr::gather(key = "response.level", value = "predicted", !! nc)


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

#' @importFrom prediction prediction
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


get_base_fitfram <- function(model, fitfram, linv, prdat, se, ci.lvl, fun, typical, terms, vcov.fun, vcov.type, vcov.args) {

  # compute ci, two-ways

  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
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


  fitfram$predicted <- linv(.predicted)


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
        vcov.args = vcov.args
      )

    if (!is.null(se.pred)) {
      se.fit <- se.pred$se.fit
      fitfram <- se.pred$fitfram
      se <- TRUE
    } else {
      se.fit <- NULL
      se <- FALSE
    }
  }


  # did user request standard errors? if yes, compute CI

  if (se && !is.null(se.fit)) {
    fitfram$conf.low <- linv(.predicted - stats::qnorm(ci) * se.fit)
    fitfram$conf.high <- linv(.predicted + stats::qnorm(ci) * se.fit)
  } else {
    # No CI
    fitfram$conf.low <- NA
    fitfram$conf.high <- NA
  }

  fitfram
}



# get standard errors of predictions from model matrix and vcov ----

get_se_from_vcov <- function(model,
                             fitfram,
                             typical,
                             terms,
                             fun = NULL,
                             type = "fe",
                             vcov.fun = NULL,
                             vcov.type = NULL,
                             vcov.args = NULL) {

  tryCatch(
    {
      safe_se_from_vcov(
        model,
        fitfram,
        typical,
        terms,
        fun,
        type,
        vcov.fun,
        vcov.type,
        vcov.args
      )
    },
    error = function(x) { NULL },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )

}

#' @importFrom stats model.matrix terms vcov formula
#' @importFrom dplyr arrange n_distinct
#' @importFrom sjstats resp_var model_frame var_names
#' @importFrom rlang parse_expr
#' @importFrom purrr map flatten_chr map_lgl
#' @importFrom sjmisc is_empty
#' @importFrom lme4 VarCorr
safe_se_from_vcov <- function(model,
                              fitfram,
                              typical,
                              terms,
                              fun,
                              type,
                              vcov.fun,
                              vcov.type,
                              vcov.args) {

  mf <- sjstats::model_frame(model, fe.only = FALSE)

  # copy data frame with predictions
  newdata <- get_expanded_data(
    model,
    mf,
    terms,
    typ.fun = typical,
    fac.typical = FALSE,
    pretty.message = FALSE
  )

  # make sure we have enough values to compute CI
  if (any(purrr::map_lgl(newdata,  ~ is.factor(.x) && nlevels(.x) == 1)))
    return(NULL)

  # add response to newdata. in case we have a matrix as outcome
  # (when using "cbind()"), we need to add both variables here

  new.resp <- sjstats::var_names(sjstats::resp_var(model))

  if (!sjmisc::is_empty(string_starts_with(pattern = "cbind(", x = new.resp))) {
    av <- all.vars(stats::formula(model))
    get.cb <- purrr::map_lgl(av, ~ grepl(.x, new.resp, fixed = T))
    new.resp <- av[get.cb]
    newdata <- sjmisc::add_variables(
      newdata,
      data.frame(
        response.val1 = 0,
        response.val2 = 0
      ),
      .after = -1
    )
    colnames(newdata)[1:2] <- new.resp
  } else {
    newdata <- sjmisc::add_variables(newdata, data.frame(response.val = 0), .after = -1)
    # proper column names, needed for getting model matrix
    colnames(newdata)[1] <- new.resp
  }


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


  # check if robust vcov-matrix is requested
  if (!is.null(vcov.fun)) {
    if (!requireNamespace("sandwich", quietly = TRUE)) {
      stop("Package `sandwich` needed for this function. Please install and try again.")
    }
    vcov.fun <- get(vcov.fun, asNamespace("sandwich"))
    vcm <- as.matrix(do.call(vcov.fun, c(list(x = model, type = vcov.type), vcov.args)))
  } else {
    # get variance-covariance-matrix, depending on model type
    if (is.null(fun))
      vcm <- as.matrix(stats::vcov(model))
    else if (fun %in% c("hurdle", "zeroinfl")) {
      vcm <- as.matrix(stats::vcov(model, model = "count"))
    } else if (fun == "betareg")
      vcm <- as.matrix(stats::vcov(model, model = "mean"))
    else if (fun == "truncreg") {
      vcm <- as.matrix(stats::vcov(model))
      # remove sigma from matrix
      vcm <- vcm[1:(nrow(vcm) - 1), 1:(ncol(vcm) - 1)]
    } else
      vcm <- as.matrix(stats::vcov(model))
  }


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

  # for poly-terms, we have no match, so fix this here
  if (sjmisc::is_empty(mm.rows) || !all(terms %in% colnames(mmdf))) {
    inters <- which(sjstats::var_names(colnames(mmdf)) %in% terms)
    mm.rows <- as.numeric(rownames(unique(mmdf[inters])))
  }

  mm <- mm[mm.rows, ]

  if (!is.null(fun) && fun %in% c("polr", "multinom")) {
    keep <- intersect(colnames(mm), colnames(vcm))
    vcm <- vcm[keep, keep]
    mm <- mm[, keep]
  }

  pvar <- diag(mm %*% vcm %*% t(mm))


  # condition on random effect variances
  if (type == "re") {
    sig <- 0
    if (inherits(model, c("merMod", "lmerMod", "glmerMod"))) {
      sig <- sum(attr(lme4::VarCorr(model), "sc"))
    } else if (inherits(model, c("lme", "nlme"))) {
      sig <- model$sigma
    }
    pvar <- pvar + sig^2 + getVarRand(model)
  }

  se.fit <- sqrt(pvar)

  # shorten to length of fitfram
  if (!is.null(fun) && fun %in% c("polr", "multinom"))
    se.fit <- rep(se.fit, each = dplyr::n_distinct(fitfram$response.level))
  else
    se.fit <- se.fit[1:nrow(fitfram)]

  list(fitfram = fitfram, se.fit = se.fit)
}

