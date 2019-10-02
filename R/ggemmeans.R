#' @importFrom sjmisc var_rename to_factor remove_empty_cols
#' @importFrom stats confint na.omit
#' @importFrom dplyr arrange
#' @importFrom sjlabelled get_labels as_numeric
#' @importFrom insight find_response get_data model_info
#' @rdname ggpredict
#' @export
ggemmeans <- function(model,
                      terms,
                      ci.lvl = .95,
                      type = c("fe", "fe.zi", "re", "re.zi"),
                      typical = "mean",
                      condition = NULL,
                      back.transform = TRUE,
                      x.as.factor = TRUE,
                      x.cat,
                      ...) {

  if (!requireNamespace("emmeans")) {
    stop("Package `emmeans` required to compute marginal effects for clmm-models.", call. = FALSE)
  }

  # check arguments
  type <- match.arg(type)
  model.name <- deparse(substitute(model))

  if (!missing(x.cat)) x.as.factor <- x.cat

  # check if terms are a formula
  if (!missing(terms) && !is.null(terms) && inherits(terms, "formula")) {
    terms <- all.vars(terms)
  }

  # for gamm/gamm4 objects, we have a list with two items, mer and gam
  # extract just the mer-part then
  if (is.gamm(model) || is.gamm4(model)) model <- model$gam

  # check model family, do we have count model?
  faminfo <- .get_model_info(model)

  # get model frame
  ori.fram <- fitfram <- insight::get_data(model)

  # check terms argument
  terms <- .check_vars(terms, model)
  cleaned.terms <- .get_cleaned_terms(terms)

  expanded_frame <- .get_data_grid(
    model = model, mf = fitfram, terms = terms, typ.fun = typical,
    condition = condition, emmeans.only = TRUE
  )


  # for zero-inflated mixed models, we need some extra handling

  if (faminfo$is_zero_inflated && inherits(model, c("glmmTMB", "MixMod")) && type == "fe.zi") {

    if (inherits(model, "MixMod")) {
      preds <- .ggemmeans_MixMod(model, expanded_frame, cleaned.terms, ...)
    } else {
      preds <- .ggemmeans_glmmTMB(model, expanded_frame, cleaned.terms, ...)
    }

    add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

    if ("nsim" %in% names(add.args))
      nsim <- eval(add.args[["nsim"]])
    else
      nsim <- 1000

    fitfram <- .ggemmeans_zi_predictions(
      model,
      fitfram,
      preds,
      ci.lvl,
      terms,
      cleaned.terms,
      typical,
      condition,
      nsim,
      type
    )
    pmode <- "response"

  } else {

    # get prediction mode, i.e. at which scale predicted
    # values should be returned
    pmode <- .get_prediction_mode_argument(model, faminfo, type)

    if (faminfo$is_ordinal | faminfo$is_categorical) {
      fitfram <- .ggemmeans_predict_ordinal(model, expanded_frame, cleaned.terms, ci.lvl, type, ...)
    } else if (inherits(model, "MCMCglmm")) {
      fitfram <- .ggemmeans_predict_MCMCglmm(model, expanded_frame, cleaned.terms, ci.lvl, pmode, type, ...)
    } else {
      fitfram <- .ggemmeans_predict_generic(model, expanded_frame, cleaned.terms, ci.lvl, pmode, type, ...)
    }

    # fix gam here
    if (inherits(model, "gam") && faminfo$is_zero_inflated) {
      fitfram$predicted <- exp(fitfram$predicted)
      fitfram$conf.low <- exp(fitfram$conf.low)
      fitfram$conf.high <- exp(fitfram$conf.high)
    }
  }

  # return NULL on error
  if (is.null(fitfram)) return(NULL)

  if (faminfo$is_ordinal | faminfo$is_categorical) {
    colnames(fitfram)[1] <- "response.level"
  }

  mydf <- .post_processing_predictions(
    model = model,
    fitfram = fitfram,
    original.model.frame = ori.fram,
    cleaned.terms = cleaned.terms,
    x.as.factor = x.as.factor
  )

  # apply link inverse function
  linv <- insight::link_inverse(model)
  if (!is.null(linv) && (inherits(model, "lrm") || pmode == "link" || (inherits(model, "MixMod") && type != "fe.zi"))) {
    mydf$predicted <- linv(mydf$predicted)
    mydf$conf.low <- linv(mydf$conf.low)
    mydf$conf.high <- linv(mydf$conf.high)
  }

  # check if outcome is log-transformed, and if so,
  # back-transform predicted values to response scale
  mydf <- .back_transform_response(model, mydf, back.transform)

  attr(mydf, "model.name") <- model.name

  .post_processing_labels(
    model = model,
    mydf = mydf,
    original.model.frame = ori.fram,
    expanded_frame = expanded_frame,
    cleaned.terms = cleaned.terms,
    original.terms = terms,
    faminfo = faminfo,
    type = type,
    prediction.interval = attr(fitfram, "prediction.interval", exact = TRUE),
    at.list = .get_data_grid(
      model = model, mf = ori.fram, terms = terms, typ.fun = typical,
      condition = condition, pretty.message = FALSE, emmeans.only = TRUE
    )
  )
}


.get_prediction_mode_argument <- function(model, faminfo, type) {
  if (inherits(model, "betareg"))
    "response"
  else if (inherits(model, c("polr", "clm", "clmm", "clm2", "rms")))
    "prob"
  else if (inherits(model, "lmerMod"))
    "asymptotic"
  else if (inherits(model, "MixMod"))
    "fixed-effects"
  else if (inherits(model, "gls"))
    "satterthwaite"
  else if (faminfo$is_ordinal | faminfo$is_categorical)
    "prob"
  else if (faminfo$is_zero_inflated && type %in% c("fe", "re") && inherits(model, "glmmTMB"))
    "link"
  else if (faminfo$is_zero_inflated && type %in% c("fe.zi", "re.zi"))
    "response"
  else if (faminfo$is_zero_inflated && type %in% c("fe", "re"))
    "count"
  else
    "link"
}
