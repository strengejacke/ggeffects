#' @importFrom sjmisc var_rename to_factor remove_empty_cols
#' @importFrom stats confint na.omit
#' @importFrom dplyr select arrange
#' @importFrom sjlabelled get_labels as_numeric
#' @importFrom insight find_response get_data model_info
#' @rdname ggpredict
#' @export
ggemmeans <- function(model,
                      terms,
                      ci.lvl = .95,
                      type = c("fe", "fe.zi"),
                      typical = "mean",
                      condition = NULL,
                      back.transform = TRUE,
                      x.as.factor = FALSE,
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
  faminfo <- get_model_info(model)

  # get model frame
  ori.fram <- fitfram <- insight::get_data(model)

  # check terms argument
  terms <- check_vars(terms, model)
  cleaned.terms <- get_clear_vars(terms)

  expanded_frame <- get_expanded_data(
    model = model, mf = fitfram, terms = terms, typ.fun = typical,
    condition = condition, emmeans.only = TRUE
  )


  # for zero-inflated mixed models, we need some extra handling

  if (faminfo$is_zeroinf && inherits(model, c("glmmTMB", "MixMod")) && type == "fe.zi") {

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

    fitfram <- .ggemmeans_zi_predictions(model, fitfram, preds, ci.lvl, terms, cleaned.terms, typical, condition, nsim)
    pmode <- "response"

  } else {

    # get prediction mode, i.e. at which scale predicted
    # values should be returned
    pmode <- get_pred_mode(model, faminfo, type)

    if (faminfo$is_ordinal | faminfo$is_categorical) {
      fitfram <- .ggemmeans_predict_ordinal(model, expanded_frame, cleaned.terms, ci.lvl, ...)
    } else if (inherits(model, "MCMCglmm")) {
      fitfram <- .ggemmeans_predict_MCMCglmm(model, expanded_frame, cleaned.terms, ci.lvl, pmode, ...)
    } else {
      fitfram <- .ggemmeans_predict_generic(model, expanded_frame, cleaned.terms, ci.lvl, pmode, ...)
    }
  }

  # return NULL on error
  if (is.null(fitfram)) return(NULL)

  # copy standard errors
  attr(fitfram, "std.error") <- fitfram$std.error

  if (faminfo$is_ordinal | faminfo$is_categorical) {
    colnames(fitfram)[1] <- "response.level"
  }


  # init legend labels
  legend.labels <- NULL

  # get axis titles and labels
  all.labels <- get_all_labels(
    fitfram = ori.fram,
    terms = cleaned.terms,
    fun = get_model_function(model),
    faminfo = faminfo,
    no.transform = FALSE,
    type = type
  )

  # now select only relevant variables: the predictors on the x-axis,
  # the predictions and the originial response vector (needed for scatter plot)

  mydf <- fitfram[, stats::na.omit(match(
    c(cleaned.terms, "predicted", "std.error", "conf.low", "conf.high", "response.level"),
    colnames(fitfram)
  ))]

  # name and sort columns, depending on groups, facet and panel
  mydf <- prepare_columns(mydf, cleaned.terms)

  # convert to factor for proper legend
  mydf <- add_groupvar_labels(mydf, ori.fram, cleaned.terms)
  mydf <- groupvar_to_label(mydf)

  # check if we have legend labels
  legend.labels <- sjlabelled::get_labels(mydf$group)

  # if we had numeric variable w/o labels, these still might be numeric
  # make sure we have factors here for our grouping and facet variables
  if (is.numeric(mydf$group))
    mydf$group <- sjmisc::to_factor(mydf$group)

  if (obj_has_name(mydf, "facet") && is.numeric(mydf$facet)) {
    mydf$facet <- sjmisc::to_factor(mydf$facet)
    attr(mydf, "numeric.facet") <- TRUE
  }


  # remember if x is factor and if we had full data
  x.is.factor <- ifelse(is.factor(mydf$x), "1", "0")
  has.full.data <- "0"

  # x needs to be numeric
  if (!x.as.factor) mydf$x <- sjlabelled::as_numeric(mydf$x)


  # sort values
  # sort values
  mydf <- sjmisc::remove_empty_cols(mydf[order(mydf$x, mydf$group), ])

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

  # set attributes with necessary information
  set_attributes_and_class(
    data = mydf,
    model = model,
    t.title = all.labels$t.title,
    x.title = all.labels$x.title,
    y.title = all.labels$y.title,
    l.title = all.labels$l.title,
    legend.labels = legend.labels,
    x.axis.labels = all.labels$axis.labels,
    faminfo = faminfo,
    x.is.factor = x.is.factor,
    full.data = has.full.data,
    constant.values = attr(expanded_frame, "constant.values", exact = TRUE),
    terms = cleaned.terms,
    ori.terms = terms,
    at.list = get_expanded_data(
      model = model, mf = ori.fram, terms = terms, typ.fun = typical,
      condition = condition, pretty.message = FALSE, emmeans.only = TRUE
    ),
    n.trials = attr(expanded_frame, "n.trials", exact = TRUE)
  )
}


#' @importFrom dplyr case_when
get_pred_mode <- function(model, faminfo, type) {
  dplyr::case_when(
    inherits(model, "betareg") ~ "response",
    inherits(model, c("polr", "clm", "clmm", "clm2", "rms")) ~ "prob",
    inherits(model, "lmerMod") ~ "asymptotic",
    inherits(model, "MixMod") ~ "fixed-effects",
    inherits(model, "gls") ~ "satterthwaite",
    faminfo$is_ordinal | faminfo$is_categorical ~ "prob",
    faminfo$is_zeroinf && type %in% c("fe", "re") && inherits(model, "glmmTMB") ~ "link",
    faminfo$is_zeroinf && type %in% c("fe.zi", "re.zi") ~ "response",
    faminfo$is_zeroinf && type %in% c("fe", "re") ~ "count",
    TRUE ~ "link"
  )
}
