#' @title Get marginal effects from model terms
#' @name ggeffect
#'
#' @description \code{ggeffect()} computes marginal effects of model terms.
#'                It internally calls \code{\link[effects]{Effect}} and
#'                puts the result into tidy data frames.
#'                \code{eff()} is an alias for \code{ggeffect()}
#'
#' @param model A fitted model object, or a list of model objects. Any model
#'          that is supported by the \CRANpkg{effects}-package should work.
#' @param ... Further arguments passed down to \code{\link[effects]{Effect}}.
#' @inheritParams ggpredict
#'
#' @return A tibble (with \code{ggeffects} class attribute) with consistent data columns:
#'         \describe{
#'           \item{\code{x}}{the values of the model predictor to which the effect pertains, used as x-position in plots.}
#'           \item{\code{predicted}}{the predicted values, used as y-position in plots.}
#'           \item{\code{conf.low}}{the lower bound of the confidence interval for the predicted values.}
#'           \item{\code{conf.high}}{the upper bound of the confidence interval for the predicted values.}
#'           \item{\code{group}}{the grouping level from the second term in \code{terms}, used as grouping-aesthetics in plots.}
#'           \item{\code{facet}}{the grouping level from the third term in \code{terms}, used to indicate facets in plots.}
#'         }
#'
#' @note The results of \code{ggeffect()} and \code{ggpredict()} are usually (almost)
#'       identical. It's just that \code{ggpredict()} calls \code{predict()}, while
#'       \code{ggeffect()} calls \code{\link[effects]{Effect}} to compute marginal
#'       effects at the mean. However, results may differ when using factors inside
#'       the formula: in such cases, \code{Effect()} takes the "mean" value of factors
#'       (i.e. computes a kind of "average" value, which represents the proportions
#'       of each factor's category), while \code{ggpredict()} uses the base
#'       (reference) level when holding these predictors at a constant value.
#'
#' @examples
#' data(efc)
#' fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
#' ggeffect(fit, terms = "c12hour")
#'
#' mydf <- ggeffect(fit, terms = c("c12hour", "c161sex"))
#' plot(mydf)
#'
#' @importFrom purrr map
#' @importFrom sjstats pred_vars resp_var
#' @importFrom dplyr if_else case_when bind_rows filter mutate
#' @importFrom tibble as_tibble
#' @importFrom sjmisc is_empty str_contains
#' @importFrom stats na.omit
#' @importFrom effects Effect
#' @importFrom sjlabelled as_numeric
#' @importFrom rlang .data
#' @export
ggeffect <- function(model, terms, ci.lvl = .95, ...) {
  if (inherits(model, "list"))
    purrr::map(model, ~ggeffect_helper(.x, terms, ci.lvl, ...))
  else
    ggeffect_helper(model, terms, ci.lvl, ...)
}


#' @importFrom sjstats model_frame
ggeffect_helper <- function(model, terms, ci.lvl, ...) {
  # check terms argument
  terms <- check_vars(terms)

  # get link-function
  fun <- get_model_function(model)

  # get model frame
  fitfram <- sjstats::model_frame(model)

  # get model family
  faminfo <- get_glm_family(model)

  # create logical for family
  poisson_fam <- faminfo$is_pois
  binom_fam <- faminfo$is_bin


  # check whether we have an argument "transformation" for effects()-function
  # in this case, we need another default title, since we have
  # non-transformed effects
  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  # check whether we have a "transformation" argument
  t.add <- which(names(add.args) == "transformation")
  # if we have a "transformation" argument, and it's NULL,
  # no transformation of scale
  no.transform <- !sjmisc::is_empty(t.add) && is.null(eval(add.args[[t.add]]))


  # check if we have specific levels in square brackets
  x.levels <- get_xlevels_vector(terms)

  # clear argument from brackets
  terms <- get_clear_vars(terms)


  # prepare getting unique values of predictors,
  # which are passed to the allEffects-function
  xl <- list()

  # create levels for all terms of interest
  for (t in terms) {
    # get unique values
    dummy <- list(x = sort(unique(stats::na.omit(fitfram[[t]]))))
    # name list, needed for effect-function
    names(dummy) <- t
    # create list for "xlevels" argument of allEffects fucntion
    xl <- c(xl, dummy)
  }

  # compute marginal effects for each model term
  eff <- effects::Effect(focal.predictors = terms, mod = model, xlevels = xl, confidence.level = ci.lvl, ...)


  # get term, for which effects were calculated
  t <- eff$term

  # build data frame, with raw values
  # predicted response and lower/upper ci
  tmp <-
    data.frame(
      x = eff$x[[terms[1]]],
      y = eff$fit,
      lower = eff$lower,
      upper = eff$upper
    )

  if (fun == "glm" && !no.transform) {
    tmp <- dplyr::mutate(
      tmp,
      y = eff$transformation$inverse(eta = .data$y),
      lower = eff$transformation$inverse(eta = .data$lower),
      upper = eff$transformation$inverse(eta = .data$upper)
    )
  }


  # define column names
  cnames <- c("x", "predicted", "conf.low", "conf.high", "group")

  # init legend labels
  legend.labels <- NULL

  # get axis titles and labels
  all.labels <- get_all_labels(fitfram, terms, get_model_function(model), binom_fam, poisson_fam, no.transform)


  # with or w/o grouping factor?
  if (length(terms) == 1) {
    # convert to factor for proper legend
    tmp$group <- sjmisc::to_factor(1)
  } else if (length(terms) == 2) {
    tmp <- dplyr::mutate(tmp, group = sjmisc::to_factor(eff$x[[terms[2]]]))
  } else {
    tmp <- dplyr::mutate(
      tmp,
      group = sjmisc::to_factor(eff$x[[terms[2]]]),
      facet = sjmisc::to_factor(eff$x[[terms[3]]])
    )
    cnames <- c(cnames, "facet")
  }


  # if we have any x-levels, go on and filter
  if (!sjmisc::is_empty(x.levels) && !is.null(x.levels)) {
    # slice data, only select observations that have specified
    # levels for the grouping variables
    filter.remove <- tmp$group %in% x.levels[[1]]
    tmp <- dplyr::filter(tmp, !! filter.remove)

    # slice data, only select observations that have specified
    # levels for the facet variables
    if (length(x.levels) > 1) {
      filter.remove <- tmp$facet %in% x.levels[[2]]
      tmp <- dplyr::filter(tmp, !! filter.remove)
    }
  }


  # label grouping variables, for axis and legend labels in plot
  if (length(terms) > 1) {
    # grouping variable may not be labelled
    # do this here, so we convert to labelled factor later
    tmp <- add_groupvar_labels(tmp, fitfram, terms)

    # convert to factor for proper legend
    tmp <- groupvar_to_label(tmp)

    # check if we have legend labels
    legend.labels <- sjlabelled::get_labels(tmp$group, attr.only = FALSE, drop.unused = TRUE)
  }


  # cpnvert to tibble
  mydf <- tibble::as_tibble(tmp)

  # add raw data as well
  attr(mydf, "rawdata") <- get_raw_data(model, fitfram, terms)

  # set attributes with necessary information
  mydf <-
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
      x.is.factor = ifelse(is.factor(fitfram[[t]]), "1", "0"),
      full.data = "0"
    )

  # set consistent column names
  colnames(mydf) <- cnames

  # make x numeric
  mydf$x <- sjlabelled::as_numeric(mydf$x, keep.labels = FALSE)

  mydf
}


#' @rdname ggeffect
#' @export
eff <- function(model, terms, ci.lvl = .95, ...) {
  ggeffect(model, terms, ci.lvl, ...)
}
