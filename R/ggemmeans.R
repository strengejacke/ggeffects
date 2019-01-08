#' @importFrom sjstats model_family model_frame link_inverse resp_var
#' @importFrom sjmisc var_rename to_factor remove_empty_cols
#' @importFrom stats confint na.omit
#' @importFrom dplyr select arrange
#' @importFrom sjlabelled get_labels as_numeric
#' @rdname ggpredict
#' @export
ggemmeans <- function(model,
                      terms,
                      ci.lvl = .95,
                      typical = "mean",
                      condition = NULL,
                      x.as.factor = FALSE,
                      x.cat,
                      ...) {

  if (!requireNamespace("emmeans")) {
    stop("Package `emmeans` required to compute marginal effects for clmm-models.", call. = FALSE)
  }

  if (!missing(x.cat)) x.as.factor <- x.cat

  # check if terms are a formula
  if (!missing(terms) && !is.null(terms) && inherits(terms, "formula")) {
    terms <- all.vars(terms)
  }

  # for gamm/gamm4 objects, we have a list with two items, mer and gam
  # extract just the mer-part then
  is.gamm <- inherits(model, c("list", "gamm")) && all(names(model %in% c("lme", "gam")))
  is.gamm4 <- inherits(model, "list") && all(names(model %in% c("mer", "gam")))
  if (is.gamm || is.gamm4) model <- model$gam


  # check model family, do we have count model?
  faminfo <- sjstats::model_family(model)

  # create logical for family
  binom_fam <- faminfo$is_bin
  poisson_fam <- faminfo$is_pois
  is_trial <- faminfo$is_trial && inherits(model, "brmsfit")

  # get model frame
  ori.fram <- fitfram <- sjstats::model_frame(model, fe.only = FALSE)

  # check terms argument
  terms <- check_vars(terms, model)
  cleaned.terms <- get_clear_vars(terms)

  expanded_frame <- get_expanded_data(
    model = model, mf = fitfram, terms = terms, typ.fun = typical,
    condition = condition, emmeans.only = TRUE
  )

  if (faminfo$is_ordinal | faminfo$is_categorical)
    cleaned.terms <- c(sjmisc::resp_var(model), cleaned.terms)

  fitfram <- suppressWarnings(
    emmeans::emmeans(
      model,
      specs = cleaned.terms,
      at = expanded_frame,
      ...
    ) %>%
      stats::confint(level = ci.lvl) %>%
      as.data.frame() %>%
      sjmisc::var_rename(
        SE = "std.error",
        emmean = "predicted",
        lower.CL = "conf.low",
        upper.CL = "conf.high",
        prob = "predicted",
        asymp.LCL = "conf.low",
        asymp.UCL = "conf.high"
      )
  )

  # copy standard errors
  attr(fitfram, "std.error") <- fitfram$std.error

  first.col <- 1
  if (faminfo$is_ordinal | faminfo$is_categorical) {
    first.col <- 2
    colnames(fitfram)[1] <- "response.level"
  }


  # init legend labels
  legend.labels <- NULL

  # get axis titles and labels
  all.labels <- get_all_labels(
    fitfram = ori.fram,
    terms = cleaned.terms,
    fun = get_model_function(model),
    binom_fam = binom_fam,
    poisson_fam = poisson_fam,
    no.transform = FALSE,
    type = "fe",
    is_trial = is_trial
  )

  # check for correct terms specification
  if (!all(terms %in% colnames(fitfram))) {
    stop("At least one term specified in `terms` is no valid model term.", call. = FALSE)
  }

  # now select only relevant variables: the predictors on the x-axis,
  # the predictions and the originial response vector (needed for scatter plot)

  cols.to.keep <- stats::na.omit(match(
    c(terms, "predicted", "std.error", "conf.low", "conf.high", "response.level"),
    colnames(fitfram)
  ))

  mydf <- dplyr::select(fitfram, !! cols.to.keep)

  # with or w/o grouping factor?
  if (length(terms) == 1) {
    colnames(mydf)[first.col] <- "x"
    mydf$group <- sjmisc::to_factor(1)
  } else {
    if (length(terms) == 2) {
      colnames(mydf)[first.col:(first.col + 1)] <- c("x", "group")
    } else {
      colnames(mydf)[first.col:(first.col + 2)] <- c("x", "group", "facet")
    }

    # convert to factor for proper legend
    mydf <- add_groupvar_labels(mydf, ori.fram, cleaned.terms)
    mydf <- groupvar_to_label(mydf)

    # check if we have legend labels
    legend.labels <- sjlabelled::get_labels(mydf$group)
  }

  # if we had numeric variable w/o labels, these still might be numeric
  # make sure we have factors here for our grouping and facet variables
  if (is.numeric(mydf$group))
    mydf$group <- sjmisc::to_factor(mydf$group)

  if (obj_has_name(mydf, "facet") && is.numeric(mydf$facet))
    mydf$facet <- sjmisc::to_factor(mydf$facet)


  # remember if x is factor and if we had full data
  x.is.factor <- ifelse(is.factor(mydf$x), "1", "0")
  has.full.data <- "0"

  # x needs to be numeric
  if (!x.as.factor) mydf$x <- sjlabelled::as_numeric(mydf$x)


  # sort values
  mydf <- mydf %>%
    dplyr::arrange(.data$x, .data$group) %>%
    sjmisc::remove_empty_cols()

  # apply link inverse function
  linv <- sjstats::link_inverse(model)
  if (!is.null(linv)) {
    mydf$predicted <- linv(mydf$predicted)
    mydf$conf.low <- linv(mydf$conf.low)
    mydf$conf.high <- linv(mydf$conf.high)
  }

  # check if outcome is log-transformed, and if so,
  # back-transform predicted values to response scale

  rv <- sjstats::resp_var(model)

  if (any(grepl("log\\((.*)\\)", rv))) {

    # do we have log-log models?
    if (grepl("log\\(log\\((.*)\\)\\)", rv)) {
      mydf$predicted <- exp(exp(mydf$predicted))
      mydf$conf.low <- exp(exp(mydf$conf.low))
      mydf$conf.high <- exp(exp(mydf$conf.high))
    } else {
      mydf$predicted <- exp(mydf$predicted)
      mydf$conf.low <- exp(mydf$conf.low)
      mydf$conf.high <- exp(mydf$conf.high)
    }

    message("Model has log-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the log-scale.")
  }


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
    n.trials = attr(expanded_frame, "n.trials", exact = TRUE)
  )
}
