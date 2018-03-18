utils::globalVariables("x")

#' @title Get marginal effects for two-way interactions from models
#' @name gginteraction
#'
#' @description \code{gginteraction()} computes marginal effects of interaction terms.
#'                It internally calls \code{\link[effects]{effect}} and
#'                puts the result into tidy data frames.
#'
#' @param model A fitted model object, or a list of model objects. Any model
#'          that is supported by the \CRANpkg{effects}-package should work.
#' @param mdrt.values Indicates which values of the moderator variable should be
#'          used to calculate marginal effects of the interaction.
#'          \describe{
#'            \item{\code{"minmax"}}{(default) minimum and maximum values (lower and upper bounds) of the moderator are used to plot the interaction between independent variable and moderator.}
#'            \item{\code{"meansd"}}{uses the mean value of the moderator as well as one standard deviation below and above mean value to plot the effect of the moderator on the independent variable.}
#'            \item{\code{"zeromax"}}{is similar to the \code{"minmax"} option, however, \code{0} is always used as minimum value for the moderator. This may be useful for predictors that don't have an empirical zero-value, but absence of moderation should be simulated by using 0 as minimum.}
#'            \item{\code{"quart"}}{calculates and uses the quartiles (lower, median and upper) of the moderator value.}
#'            \item{\code{"all"}}{uses all values of the moderator variable. Note that this option only applies to \code{type = "eff"}, for numeric moderator values.}
#'          }
#' @param swap.pred Logical, if \code{TRUE}, the predictor (defining the x-position)
#'          and the moderator (defining the groups) in an interaction are
#'          swapped. By default, the first interaction term is considered as
#'          moderator and the second term is used to define the x-position.
#' @param ... Further arguments passed down to \code{\link[effects]{effect}}.
#' @inheritParams ggeffect
#' @inheritParams ggpredict
#'
#' @return A tibble (with \code{ggeffects} class attribute) with consistent data columns:
#'         \describe{
#'           \item{\code{x}}{the values of the model predictor to which the effect pertains, used as x-position in plots.}
#'           \item{\code{predicted}}{the predicted values, used as y-position in plots.}
#'           \item{\code{conf.low}}{the lower bound of the confidence interval for the predicted values.}
#'           \item{\code{conf.high}}{the upper bound of the confidence interval for the predicted values.}
#'           \item{\code{group}}{the name of \code{x}, used as grouping-aesthetics in plots.}
#'         }
#'
#' @note \code{gginteraction()} only computes marginal effects for interaction terms,
#'       in particular two-way interactions. Use \code{\link{ggeffect}} for
#'       marginal effects for simple model terms. Or use \code{\link{ggpredict}}
#'       for predictions from any model terms, including two- or three-way
#'       interactions.
#'
#' @examples
#' data(efc)
#' efc$c172code <- sjmisc::to_factor(efc$c172code)
#' fit <- lm(barthtot ~ c12hour + c161sex + c172code * neg_c_7, data = efc)
#' gginteraction(fit)
#'
#' # this would give the same results
#' ggpredict(fit, terms = c("neg_c_7", "c172code"))
#'
#' library(ggplot2)
#' ggplot(gginteraction(fit), aes(x, predicted, colour = group)) +
#'   geom_line()
#'
#' dat <- gginteraction(fit)
#' ggplot(dat, aes(x, predicted, colour = group)) +
#'   geom_line() +
#'   labs(
#'     colour = get_legend_title(dat),
#'     x = get_x_title(dat),
#'     y = get_y_title(dat),
#'     title = get_title(dat)
#'   )  +
#'   scale_color_manual(
#'     values = c("red", "green", "blue"),
#'     labels = get_legend_labels(dat)
#'   )
#'
#' # use continuous term on x-axis, but use values mean +/- sd as groups
#' dat <- gginteraction(fit, mdrt.values = "meansd", swap.pred = TRUE)
#' ggplot(dat, aes(x, predicted, colour = group)) + geom_line()
#'
#' @importFrom sjmisc is_empty trim is_num_fac
#' @importFrom stats formula na.omit model.frame quantile terms sd
#' @importFrom sjstats resp_var
#' @importFrom dplyr case_when
#' @importFrom effects effect
#' @importFrom sjlabelled as_numeric
#' @export
gginteraction <- function(model, mdrt.values = "minmax", swap.pred = FALSE, ci.lvl = .95, x.as.factor = FALSE, ...) {
  if (inherits(model, "list"))
    purrr::map(model, ~gginteraction_helper(.x, mdrt.values, swap.pred, ci.lvl, x.as.factor, ...))
  else
    gginteraction_helper(model, mdrt.values, swap.pred, ci.lvl, x.as.factor, ...)
}


#' @importFrom sjstats model_frame
gginteraction_helper <- function(model, mdrt.values, swap.pred, ci.lvl, x.as.factor, ...) {
  # get link-function
  fun <- get_model_function(model)

  # get model frame
  fitfram <- sjstats::model_frame(model)

  # get model family
  faminfo <- get_glm_family(model)

  # create logical for family
  poisson_fam <- faminfo$is_pois
  binom_fam <- faminfo$is_bin

  # additional arguments for 'effects()'-function?
  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  # check whether we have a "transformation" argument
  t.add <- which(names(add.args) == "transformation")
  # if we have a "transformation" argument, and it's NULL,
  # no transformation of scale
  no.transform <- !sjmisc::is_empty(t.add) && is.null(eval(add.args[[t.add]]))

  # save model formula as character
  mod.terms <-
    sub(":", "*", attr(stats::terms(model), "term.labels"), fixed = T)

  # replace : with *, to get consistent pattern
  intpos <- grep("*", mod.terms, fixed = T)

  # any interaction term found?
  if (length(intpos) == 0) {
    warning("No interaction term found in fitted model.", call. = FALSE)
    return(invisible(NULL))
  }

  # more than 1 interaction term not supported
  if (length(intpos) > 1) {
    warning("`ggeffect()` does not works for models with more than one interaction term.", call. = FALSE)
    return(invisible(NULL))
  }

  # remove all terms but interaction term
  mod.terms <- mod.terms[intpos]

  # split terms at "*", to get both interaction terms
  iterms <- sjmisc::trim(unlist(strsplit(mod.terms, "*", fixed = T)))

  # get names of predictor and moderator terms of interaction
  pred_x.name <- iterms[ifelse(isTRUE(swap.pred), 1, 2)]
  moderator.name <- iterms[ifelse(isTRUE(swap.pred), 2, 1)]

  # check whether terms are factor or not
  x_is_factor <- is.factor(fitfram[[pred_x.name]]) || (length(unique(stats::na.omit(fitfram[[pred_x.name]]))) < 3)
  mod_is_factor <- is.factor(fitfram[[moderator.name]])

  # check for moderator values, but only, if moderator
  # is no factor value. In this case, we can choose
  # the values for continuous moderator intentionally,
  # e.g. only min/max, or mean and sd. We don't need these
  # values for categorical moderator values.
  if (!mod_is_factor) {
    # retrieve moderator value
    modval <- fitfram[[moderator.name]]
    # retrieve predictor value
    predval <- fitfram[[pred_x.name]]

    # Check whether moderator value has enough unique values
    # for quartiles
    mdrt.values <- mv_check(mdrt.values, modval)

    # we have more than two values, so re-calculate effects, just using
    # min and max value of moderator.
    if (mdrt.values == "minmax") {
      # retrieve min and max values
      mv.min <- min(modval, na.rm = T)
      mv.max <- max(modval, na.rm = T)
      # re-compute effects, prepare xlevels
      xl1 <- list(x = c(mv.min, mv.max))
      # we have more than two values, so re-calculate effects, just using
      # 0 and max value of moderator.
    } else if (mdrt.values == "zeromax") {
      # retrieve max values
      mv.max <- max(modval, na.rm = T)
      # re-compute effects, prepare xlevels
      xl1 <- list(x = c(0, mv.max))
      # compute mean +/- sd
    } else if (mdrt.values == "meansd") {
      # retrieve mean and sd
      mv.mean <- round(mean(modval, na.rm = T), 2)
      mv.sd <- round(stats::sd(modval, na.rm = T), 2)
      # re-compute effects, prepare xlevels
      xl1 <- list(x = c(mv.mean - mv.sd, mv.mean, mv.mean + mv.sd))
    } else if (mdrt.values == "all") {
      # re-compute effects, prepare xlevels
      xl1 <- list(x = as.vector(unique(sort(modval, na.last = NA))))
    } else if (mdrt.values == "quart") {
      # re-compute effects, prepare xlevels
      xl1 <- list(x = as.vector(stats::quantile(modval, na.rm = T)))
    }

    # change list name to moderator value name
    names(xl1) <- moderator.name
    # add values of interaction term
    # first, get all unqiue values
    prvl <- sort(unique(stats::na.omit(predval)))
    # add them to list as well
    xl2 <- list(y = prvl)
    # change list name
    names(xl2) <- pred_x.name

    # compute effects
    eff.tmp <-
      effects::effect(mod.terms,
                      model,
                      xlevels = c(xl1, xl2),
                      confidence.level = ci.lvl,
                      ...)
    # reset data frame
    intdf <- data.frame(eff.tmp)
  } else if (!x_is_factor) {
    # check for predictor values on x-axis. if it
    # is no factor, select whole range of possible values.
    predval <- fitfram[[pred_x.name]]
    # add values of interaction term
    # first, get all unqiue values
    prvl <- sort(unique(stats::na.omit(predval)))
    # add them to list as well
    xl <- list(x = prvl)
    # change list name
    names(xl) <- pred_x.name

    # compute effects
    eff.tmp <-
      effects::effect(mod.terms,
                      model,
                      xlevels = xl,
                      confidence.level = ci.lvl,
                      ...)
    # reset data frame
    intdf <- data.frame(eff.tmp)
  } else {
    # compute effects
    eff.tmp <-
      effects::effect(mod.terms,
                      model,
                      confidence.level = ci.lvl,
                      ...)
    # reset data frame
    intdf <- data.frame(eff.tmp)
  }


  # change column names
  if (swap.pred) {
    colnames(intdf) <- c("x", "group", "predicted", "conf.low", "conf.high")
  } else {
    colnames(intdf) <- c("group", "x", "predicted", "conf.low", "conf.high")
  }


  # make sure x is numeric
  if (!x.as.factor) intdf$x <- sjlabelled::as_numeric(intdf$x, keep.labels = F)

  # effects-package creates "NA" factor levels, which
  # need to be removed
  intdf <- droplevels(intdf)

  # get all axis labels
  all.labels <- get_all_labels(fitfram, c(pred_x.name, moderator.name), fun, binom_fam, poisson_fam, no.transform)

  # grouping variable may not be labelled
  # do this here, so we convert to labelled factor later
  intdf <- add_groupvar_labels(intdf, fitfram, c(pred_x.name, moderator.name))

  # convert to factor for proper legend
  intdf <- groupvar_to_label(intdf)

  # check if we have legend labels
  legend.labels <- sjlabelled::get_labels(intdf$group, attr.only = FALSE, drop.unused = TRUE)

  # sort columns
  intdf <-
    intdf[, match(c("x", "predicted", "conf.low", "conf.high", "group"),
                  colnames(intdf))]

  # set attributes with necessary information
  set_attributes_and_class(data = intdf,
                           model = model,
                           t.title = all.labels$t.title,
                           x.title = all.labels$x.title,
                           y.title = all.labels$y.title,
                           l.title = all.labels$l.title,
                           legend.labels = legend.labels,
                           x.axis.labels = all.labels$axis.labels,
                           faminfo = faminfo,
                           x.is.factor = ifelse(x_is_factor, "1", "0"),
                           full.data = "0"
                           )
}


#' @importFrom stats quantile
mv_check <- function(mdrt.values, x) {
  mvc <- length(unique(as.vector(stats::quantile(x, na.rm = T))))
  if (mdrt.values == "quart" && mvc < 3) {
    # tell user that quart won't work
    message("Could not compute quartiles, too small range of moderator variable. Defaulting `mdrt.values` to `minmax`.")
    mdrt.values <- "minmax"
  }
  mdrt.values
}
