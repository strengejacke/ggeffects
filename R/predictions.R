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
                                     condition,
                                     ...) {
  # get link-inverse-function
  linv <- sjstats::link_inverse(model)
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


# predictions for polr ----

#' @importFrom tidyr gather
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
  key_col <- "response.level"
  value_col <- "predicted"

  fitfram <- tidyr::gather(fitfram, !! key_col, !! value_col, !! 1:ncol(prdat))

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
#' @importFrom sjstats model_frame resp_var
#' @importFrom sjmisc var_rename
get_predictions_clmm <- function(model, terms, typical, condition, ci.lvl, linv, ...) {

  if (!requireNamespace("emmeans")) {
    stop("Package `emmeans` required to compute marginal effects for clmm-models.", call. = FALSE)
  }

  values.at <- get_expanded_data(
    model = model,
    mf = sjstats::model_frame(model, fe.only = FALSE),
    terms = terms,
    typ.fun = typical,
    condition = condition,
    pretty.message = FALSE,
    emmeans.only = TRUE
  )

  fitfram <- emmeans::emmeans(
    object = model,
    spec = c(sjstats::resp_var(model), get_clear_vars(terms)),
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
    ci <- (1 + ci.lvl) / 2
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
#' @importFrom sjstats model_frame
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

    mf <- sjstats::model_frame(model)
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

      cat(crayon::red("Error: Confidence intervals could not be computed.\n"))
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

#' @importFrom stats model.matrix coef formula as.formula
#' @importFrom MASS mvrnorm
get_zeroinfl_predictions <- function(model, newdata, nsim = 1000, terms = NULL, typical = NULL, condition = NULL) {
  tryCatch(
    {
      condformula <- stats::as.formula(paste0("~", deparse(stats::formula(model)[[3]][[2]])))
      ziformula <- stats::as.formula(paste0("~", deparse(stats::formula(model)[[3]][[3]])))

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

      pred.condpar.psim <- MASS::mvrnorm(nsim, mu = beta.cond, Sigma = stats::vcov(model, model = "count"))
      pred.cond.psim <- x.cond %*% t(pred.condpar.psim)

      pred.zipar.psim <- MASS::mvrnorm(nsim, mu = beta.zi, Sigma = stats::vcov(model, model = "zero"))
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


get_rows_to_keep <- function(model, newdata, condformula, ziformula, terms, typical, condition) {
  # if formula has a polynomial term, and this term is one that is held
  # constant, model.matrix() with "newdata" will throw an error - so we
  # re-build the newdata-argument by including all values for poly-terms, if
  # these are hold constant.

  const.values <- attr(newdata, "constant.values")
  condformula_string <- deparse(condformula)
  ziformula_string <- deparse(ziformula)

  keep <- NULL

  if (has_poly_term(condformula_string) || has_poly_term(ziformula_string)) {

    polycondcheck <- NULL
    polyzicheck <- NULL

    if (has_poly_term(condformula_string)) {
      polyterm <- get_poly_term(condformula_string)
      if (polyterm %in% names(const.values)) {
        polycondcheck <- polyterm
        terms <- c(terms, sprintf("%s [all]", polyterm))
      }
    }

    if (has_poly_term(ziformula_string)) {
      polyterm <- get_poly_term(ziformula_string)
      if (polyterm %in% names(const.values)) {
        polyzicheck <- polyterm
        terms <- c(terms, sprintf("%s [all]", polyterm))
      }
    }

    newdata <- get_expanded_data(
      model = model,
      mf = sjstats::model_frame(model),
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
        wm <- newdata[[.x]][which.min(abs(newdata[[.x]] - sjstats::typical_value(newdata[[.x]], fun = typical)))]
        as.vector(which(newdata[[.x]] == wm))
      }) %>% unlist()
    }

    if (!is.null(polyzicheck)) {
      keep.zi <- lapply(polyzicheck, function(.x) {
        wm <- newdata[[.x]][which.min(abs(newdata[[.x]] - sjstats::typical_value(newdata[[.x]], fun = typical)))]
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
#' @importFrom sjstats model_family model_frame
get_predictions_glmmTMB <- function(model, fitfram, ci.lvl, linv, type, terms, typical, condition, ...) {
  # does user want standard errors?
  se <- !is.null(ci.lvl) && !is.na(ci.lvl)

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975


  # check if we have zero-inflated model part

  modfam <- sjstats::model_family(model)
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

      mf <- sjstats::model_frame(model)

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

        cat(crayon::red("Error: Confidence intervals could not be computed.\n"))
        if (inherits(prdat.sim, c("error", "simpleError"))) {
          cat(sprintf("* Reason: %s\n", deparse(prdat.sim[[1]])))
          cat(sprintf("* Source: %s\n", deparse(prdat.sim[[2]])))
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


#' @importFrom stats simulate quantile sd
#' @importFrom sjstats model_frame model_family
#' @importFrom rlang syms .data
#' @importFrom dplyr group_by summarize ungroup
simulate_predictions <- function(model, nsim, clean_terms, ci) {
  fitfram <- sjstats::model_frame(model)
  fam <- sjstats::model_family(model)

  if (fam$is_bin | fam$is_ordinal | fam$is_categorical)
    stop("Can't simulate predictions from models with binary, categorical or ordinal outcome. Please use another option for argument `type`.", call. = FALSE)

  sims <- stats::simulate(model, nsim = nsim, re.form = NULL)

  fitfram$predicted <- apply(sims, 1, mean)
  fitfram$conf.low <- apply(sims, 1, stats::quantile, probs = 1 - ci)
  fitfram$conf.high <- apply(sims, 1, stats::quantile, probs = ci)
  fitfram$std.error <- apply(sims, 1, stats::sd)

  grp <- rlang::syms(clean_terms)
  fitfram %>%
    dplyr::group_by(!!! grp) %>%
    dplyr::summarize(
      predicted = mean(.data$predicted),
      conf.low = mean(.data$conf.low),
      conf.high = mean(.data$conf.high),
      std.error = mean(.data$std.error)
    ) %>%
    dplyr::ungroup()
}


#' @importFrom dplyr group_by summarize ungroup left_join filter arrange
#' @importFrom rlang syms .data
#' @importFrom stats quantile sd
get_zeroinfl_fitfram <- function(fitfram, newdata, prdat, sims, ci, clean_terms) {
  fitfram$sort__id <- 1:nrow(fitfram)
  fitfram <- suppressMessages(suppressWarnings(dplyr::left_join(newdata, fitfram)))

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
#' @importFrom lme4 nobars fixef
#' @importFrom stats model.matrix formula
get_glmmTMB_predictions <- function(model, newdata, nsim, terms = NULL, typical = NULL, condition = NULL) {
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

      pred.condpar.psim <- MASS::mvrnorm(n = nsim, mu = beta.cond, Sigma = stats::vcov(model)$cond)
      pred.cond.psim <- x.cond %*% t(pred.condpar.psim)
      pred.zipar.psim <- MASS::mvrnorm(n = nsim, mu = beta.zi, Sigma = stats::vcov(model)$zi)
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
  # predictions for monotonic models

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

    # for multivariate reponse models, we have an array
    # instead of matrix - get CIs for each response

    if (inherits(prdat2, "array")) {
      tmp <- purrr::map_df(1:dim(prdat2)[3], function(.x) {
        as.data.frame(rstantools::predictive_interval(as.matrix(prdat2[, , .x])))
      })
    } else {
      tmp <- rstantools::predictive_interval(prdat2)
    }

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
#' @importFrom tidyr gather
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

#' @importFrom tidyr gather
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



# get standard errors of predictions from model matrix and vcov ----

get_se_from_vcov <- function(model,
                             fitfram,
                             typical,
                             terms,
                             fun = NULL,
                             type = "fe",
                             vcov.fun = NULL,
                             vcov.type = NULL,
                             vcov.args = NULL,
                             condition = NULL) {

  se <- tryCatch(
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
        vcov.args,
        condition
      )
    },
    error = function(x) { NULL },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )

  if (is.null(se))
    message(sprintf("Could not compute confidence intervals, probably due to memory allocation problems. Try using the `n`-tag (with different values) to reduce number of predicted values, e.g. `terms=\"%s [n=10]\"`.", terms[1]))

  se
}

#' @importFrom stats model.matrix terms vcov formula
#' @importFrom dplyr arrange n_distinct
#' @importFrom sjstats resp_var model_frame var_names
#' @importFrom rlang parse_expr
#' @importFrom purrr map flatten_chr map_lgl map2
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
                              vcov.args,
                              condition) {

  mf <- sjstats::model_frame(model, fe.only = FALSE)

  # check random effect terms. We can't compute SE if data has
  # factors with only one level, however, if user conditions on
  # random effects and only conditions on one level, it is indeed
  # possible to calculate SE - so, ignore random effects for the
  # check of one-level-factors only

  re.terms <- sjstats::re_grp_var(model)


  # we can't condition on categorical variables

  if (!is.null(condition)) {
    cn <- names(condition)
    cn.factors <- purrr::map_lgl(cn, ~ is.factor(mf[[.x]]) && !(.x %in% re.terms))
    condition <- condition[!cn.factors]
    if (sjmisc::is_empty(condition)) condition <- NULL
  }


  # copy data frame with predictions
  newdata <- get_expanded_data(
    model,
    mf,
    terms,
    typ.fun = typical,
    fac.typical = FALSE,
    pretty.message = FALSE,
    condition = condition
  )

  # make sure we have enough values to compute CI
  if (any(purrr::map_lgl(colnames(newdata), ~ !(.x %in% re.terms) && is.factor(newdata[[.x]]) && nlevels(newdata[[.x]]) == 1)))
    return(NULL)


  # add response to newdata. in case we have a matrix as outcome
  # (when using "cbind()"), we need to add both variables here.
  # For models fitted with "glmmPQL", the response variable is
  # renamed internally to "zz".

  if (inherits(model, "glmmPQL"))
    new.resp <- "zz"
  else
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
    else if (fun %in% c("hurdle", "zeroinfl", "zerotrunc")) {
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


  # code to compute se of prediction taken from
  # http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
  mm <- stats::model.matrix(stats::terms(model), newdata)

  # here we need to fix some term names, so variable names match the column
  # names from the model matrix. NOTE that depending on the type of contrasts,
  # the naming column names for factors differs: for "contr.sum", column names
  # of factors are named "Species1", "Species2", etc., while for "contr.treatment",
  # column names are "Speciesversicolor", "Speciesvirginica", etc.

  contrs <- attr(mm, "contrasts")

  if (!sjmisc::is_empty(contrs)) {

    # check which contrasts are actually in terms-argument,
    # and which terms also appear in contrasts
    keep.c <- names(contrs) %in% terms
    rem.t <- terms %in% names(contrs)

    # only iterate required terms and contrasts
    contrs <- contrs[keep.c]
    terms <- terms[!rem.t]

    add.terms <- purrr::map2(contrs, names(contrs), function(.x, .y) {
      f <- mf[[.y]]
      if (.x %in% c("contr.sum", "contr.helmert"))
        sprintf("%s%s", .y, 1:(nlevels(f) - 1))
      else if (.x == "contr.poly")
        sprintf("%s%s", .y, c(".L", ".Q", ".C"))
      else
        sprintf("%s%s", .y, levels(f)[2:nlevels(f)])
    }) %>%
      purrr::flatten_chr()

    terms <- c(terms, add.terms)
  }


  # we need all this intersection-stuff to reduce the model matrix and remove
  # duplicated entries. Else, especially for mixed models, we often run into
  # memory allocation problems. The problem is to find the correct rows of
  # the matrix that should be kept, and only take those columns of the
  # matrix for which terms we need standard errors.

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
    pvar <- pvar + getVarRand(model)
  }

  se.fit <- sqrt(pvar)

  # shorten to length of fitfram
  if (!is.null(fun) && fun %in% c("polr", "multinom"))
    se.fit <- rep(se.fit, each = dplyr::n_distinct(fitfram$response.level))
  else
    se.fit <- se.fit[1:nrow(fitfram)]

  list(fitfram = fitfram, se.fit = se.fit)
}

