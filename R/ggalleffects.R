utils::globalVariables("x")

#' @title Get marginal effects for all model predictors
#' @name ggalleffects
#'
#' @description \code{ggalleffects()} computes marginal effects of model terms.
#'                It internally calls \code{\link[effects]{allEffects}} and
#'                puts the result into tidy data frames.
#'
#' @param model A fitted model object, or a list of model objects. Any model
#'          that is supported by the \CRANpkg{effects}-package should work.
#' @param terms Character vector with term names of selected variables from
#'          \code{model}, which should be used to compute marginal effects.
#'          If \code{terms = NULL}, marginal effects for all model terms are
#'          returned.
#' @param ... Further arguments passed down to \code{\link[effects]{allEffects}}.
#' @inheritParams ggpredict
#'
#' @return A list of tibbles (with \code{ggeffects} class attribute) with consistent
#'           data columns. The list contains one tibble per model term. Columns are:
#'         \describe{
#'           \item{\code{x}}{the values of the model predictor to which the effect pertains, used as x-position in plots.}
#'           \item{\code{predicted}}{the predicted values, used as y-position in plots.}
#'           \item{\code{conf.low}}{the lower bound of the confidence interval for the predicted values.}
#'           \item{\code{conf.high}}{the upper bound of the confidence interval for the predicted values.}
#'         }
#'
#' @note Interaction effects are not included in the return value, i.e. \code{ggalleffects()}
#'       does not compute marginal effects for interaction terms. Use \code{\link{gginteraction}}
#'       to create tidy data frames especially for interaction terms.
#'
#' @examples
#' data(efc)
#' fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
#' ggalleffects(fit)
#'
#' library(ggplot2)
#' fit <- lm(barthtot ~ c12hour + neg_c_7 + c160age, data = efc)
#' mydf <- get_complete_df(ggalleffects(fit))
#'
#' ggplot(mydf, aes(x, predicted)) +
#'   geom_line() +
#'   facet_wrap(~group, scale = "free_x", ncol = 1)
#'
#' @importFrom purrr map
#' @importFrom sjstats pred_vars resp_var model_family model_frame
#' @importFrom dplyr if_else case_when bind_rows filter
#' @importFrom tibble as_tibble
#' @importFrom sjmisc is_empty str_contains
#' @importFrom sjlabelled get_label get_labels as_label
#' @importFrom stats na.omit
#' @importFrom effects allEffects
#' @export
ggalleffects <- function(model, terms = NULL, ci.lvl = .95, ...) {
  if (inherits(model, "list"))
    purrr::map(model, ~ggalleffects_helper(.x, terms, ci.lvl, ...))
  else
    ggalleffects_helper(model, terms, ci.lvl, ...)
}


#' @importFrom sjstats model_frame
#' @importFrom sjlabelled as_numeric
ggalleffects_helper <- function(model, terms, ci.lvl, ...) {
  # get link-function
  fun <- get_model_function(model)

  # get model frame
  fitfram <- sjstats::model_frame(model)

  # get model family
  faminfo <- sjstats::model_family(model)

  # create logical for family
  poisson_fam <- faminfo$is_pois
  binom_fam <- faminfo$is_bin

  # retrieve all terms and term name, excluding intercept,
  # both as they appear as column name and as real variable name
  all.terms <- colnames(fitfram)[-1]
  all.pred.names <- sjstats::pred_vars(model)[seq_len(length(all.terms))]
  valid.names <- NULL

  # Retrieve response for automatic title
  resp <- fitfram[[1]]
  resp.col <- sjstats::resp_var(model)


  # check whether we have an argument "transformation" for effects()-function
  # in this case, we need another default title, since we have
  # non-transformed effects
  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  # check whether we have a "transformation" argument
  t.add <- which(names(add.args) == "transformation")
  # if we have a "transformation" argument, and it's NULL,
  # no transformation of scale
  no.transform <- !sjmisc::is_empty(t.add) && is.null(eval(add.args[[t.add]]))


  # check for family, and set appropriate scale-title
  # if we have transformation through effects-package,
  # check if data is on original or transformed scale
  ysc <- get_title_labels(fun, binom_fam, poisson_fam, no.transform)

  # set y-axis-title
  y.title <-
    paste(sprintf("Predicted %s for", ysc),
          sjlabelled::get_label(resp, def.value = resp.col))


  # select specific estimates?
  if (!is.null(terms)) {
    # check if we have specific levels in square brackets
    x.levels <- get_xlevels_vector(terms)

    # clear argument from brackets
    terms <- get_clear_vars(terms)

    # get columns to remove
    remcols <- match(terms, all.pred.names)

    # remove all terms that should not be returned by function
    if (!sjmisc::is_empty(remcols)) {
      all.terms <- all.terms[remcols]
      all.pred.names <- all.pred.names[remcols]
    }

  } else {
    x.levels <- NULL
  }

  # prepare getting unique values of predictors,
  # which are passed to the allEffects-function
  xl <- list()

  # create levels for all terms of interest
  for (t in all.terms) {
    # get unique values
    dummy <- list(x = sort(unique(stats::na.omit(fitfram[[t]]))))
    # name list, needed for effect-function
    names(dummy) <- t
    # create list for "xlevels" argument of allEffects fucntion
    xl <- c(xl, dummy)
  }

  # compute marginal effects for each model term
  eff <- effects::allEffects(model, xlevels = xl, confidence.level = ci.lvl, ...)

  # remove spaces from model terms, required, because 'effects()' removes
  # them, too, else we don't match the model term ("log(term + 1)" => "log(term+1)")
  all.terms <- gsub(" ", "", all.terms, fixed = TRUE)

  # select specific terms only
  eff <- eff[which(names(eff) %in% all.terms | names(eff) %in% all.pred.names)]

  # init final df
  mydat <- list()
  # interaction term found?
  int.found <- FALSE

  # iterate all effects
  for (i in seq_len(length(eff))) {
    # get term, for which effects were calculated
    t <- eff[[i]]$term
    valid.names <- c(valid.names, t)

    # check if we have interaction term
    # these are ignored in this case.
    if (sjmisc::str_contains(t, pattern = c(":", "*"), logic = "not")) {
      # build data frame, with raw values
      # predicted response and lower/upper ci
      if (fun == "glm" && !no.transform) {
        tmp <-
          data.frame(
            x = eff[[i]]$x[[t]],
            y = eff[[i]]$transformation$inverse(eta = eff[[i]]$fit),
            lower = eff[[i]]$transformation$inverse(eta = eff[[i]]$lower),
            upper = eff[[i]]$transformation$inverse(eta = eff[[i]]$upper),
            grp = t
          )
      } else {
        tmp <-
          data.frame(
            x = eff[[i]]$x[[t]],
            y = eff[[i]]$fit,
            lower = eff[[i]]$lower,
            upper = eff[[i]]$upper,
            grp = t
          )
      }

      # get possible labels
      if (t %in% colnames(fitfram))
        tmp_lab <- sjlabelled::get_labels(fitfram[[t]])
      else
        tmp_lab <- NULL

      # check if we have correct amount of labels
      if (length(tmp_lab) != nrow(tmp)) tmp_lab <- NULL

      # if we still have no labels, copy values from variable
      if (is.null(tmp_lab))
        tmp_lab <- as.character(suppressWarnings(sjlabelled::as_label(tmp$x, add.non.labelled = T)))

      # check if x is a factor...
      x_is_factor <- anyNA(suppressWarnings(as.numeric(tmp_lab)))

      # if we have unlabelled-numeric, don't use labels
      if (!x_is_factor) tmp_lab <- NULL

      if (x_is_factor) {
        tmp$x <- sjlabelled::as_label(tmp$x, drop.na = T)
      } else {
        tmp$x <- sjlabelled::as_numeric(tmp$x, keep.labels = F)
      }

      # cpnvert to tibble
      mydf <- tibble::as_tibble(tmp)

      # if we have any x-levels, go on and filter
      if (!sjmisc::is_empty(x.levels) && !is.null(x.levels)) {
        # get names of covariates that should be filtered
        x.lvl.names <- names(x.levels)

        # is current term in any user-level-defined term?
        x.lvl.match <- match(t, x.lvl.names)
        # slice data, only select observations that have specified
        # levels for the grouping variables
        if (!is.na(x.lvl.match))
          mydf <- dplyr::filter(mydf, x %in% x.levels[[x.lvl.match]])
      }

      # add raw data as well
      attr(mydf, "rawdata") <- get_raw_data(model, fitfram, t)

      # set attributes with necessary information
      mydf <-
        set_attributes_and_class(
          data = mydf,
          model = model,
          t.title = "Marginal effects of model predictors",
          x.title = sjlabelled::get_label(fitfram[[t]], def.value = t),
          y.title = y.title,
          l.title = NULL,
          legend.labels = NULL,
          x.axis.labels = tmp_lab,
          faminfo = faminfo,
          x.is.factor = ifelse(is.factor(mydf$x) | x_is_factor, "1", "0"),
          full.data = "0"
        )

      # set consistent column names
      colnames(mydf) <- c("x", "predicted", "conf.low", "conf.high", "group")

      mydat[[length(mydat) + 1]] <- mydf
    } else {
      int.found <- TRUE
    }
  }

  # check if we have only moderation and no single higher order terms
  if (sjmisc::is_empty(mydat)) {
    warning("Model has no higher order terms (except for possible interaction terms). There are no effects that can be plotted. Consider using `gginteraction()` if model has interaction terms.", call. = F)
    return(NULL)
  }

  # tell user that interaction terms are ignored
  if (int.found) {
    message("Interaction terms in model have been ignored. Use `gginteraction()` to get marginal effects of interaction terms.")
  }

  # name elements
  names(mydat) <- valid.names

  # class attribute, for plot method
  class(mydat) <- c("ggalleffects", class(mydat))
  mydat
}
