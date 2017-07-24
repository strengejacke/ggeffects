utils::globalVariables(c("observed", "predicted"))

#' @title Get marginal effects from model terms
#' @name ggpredict
#'
#' @description \code{ggpredict()} computes predicted (fitted) values for the
#'                response, at the margin of specific values from certain model terms,
#'                where additional model terms indicate the grouping structure.
#'                \code{ggaverage()} computes the average predicted values.
#'                The result is returned as tidy data frame.
#'                \cr \cr
#'                \code{mem()} is an alias for \code{ggpredict()} (marginal effects
#'                at the mean), \code{ame()} is an alias for \code{ggaverage()}
#'                (average marginal effects).
#'
#' @param model A fitted model object, or a list of model objects. Any model
#'          that supports common methods like \code{predict()}, \code{family()}
#'          or \code{model.frame()} should work.
#' @param terms Character vector with the names of those terms from \code{model},
#'          for which marginal effects should be displayed. At least one term
#'          is required to calculate effects, maximum length is three terms,
#'          where the second and third term indicate the groups, i.e. predictions
#'          of first term are grouped by the levels of the second (and third)
#'          term. Indicating levels in square brackets allows for selecting
#'          only specific groups. Term name and levels in brackets must be
#'          separated by a whitespace character, e.g.
#'          \code{terms = c("age", "education [1,3]")}. See 'Examples'.
#'          All remaining covariates that are not specified in \code{terms}
#'          are held constant (if \code{full.data = FALSE}, the default)
#'          or are set to the values from the observations (i.e. are kept
#'          as they happen to be; see 'Details').
#' @param ci.lvl Numeric, the level of the confidence intervals. For \code{ggpredict()},
#'          use \code{ci.lvl = NA}, if confidence intervals should not be calculated
#'          (for instance, due to computation time).
#' @param type Character, only applies for mixed effects models. Indicates
#'          whether predicted values should be conditioned on random effects
#'          (\code{type = "re"}) or fixed effects only (\code{type = "fe"},
#'          the default).
#' @param full.data Logical, if \code{TRUE}, the returned data frame contains
#'          predictions for all observations. This data frame also has columns
#'          for residuals and observed values, and can also be used to plot a
#'          scatter plot of all data points or fitted values.
#'          If \code{FALSE} (the default), the returned data frame only contains
#'          predictions for all combinations of unique values of the model's
#'          predictors. Residuals and observed values are set to \code{NA}.
#'          Usually, this argument is only used internally by \code{ggaverage()}.
#' @param typical Character vector, naming the function to be applied to the
#'           covariates over which the effect is "averaged". The default is "mean".
#'           See \code{\link[sjstats]{typical_value}} for options.
#' @param ... Further arguments passed down to \code{predict()}.
#'
#' @details Currently supported model-objects are: \code{lm, glm, glm.nb, lme, lmer,
#'          glmer, glmer.nb, nlmer, glmmTMB, gam, vgam, gamm, gamm4, betareg, gls,
#'          gee, plm, lrm, polr, hurdle, zeroinfl, svyglm, svyglm.nb, truncreg, coxph}.
#'          Other models not listed here are passed to a generic predict-function
#'          and might work as well, or maybe with \code{ggeffect()}, which
#'          effectively does the same as \code{ggpredict()}.
#'          \cr \cr
#'          If \code{full.data = FALSE}, \code{expand.grid()} is called
#'          on all unique combinations of \code{model.frame(model)[, terms]} and
#'          used as \code{newdata}-argument for \code{predict()}. In this case,
#'          all remaining covariates that are not specified in \code{terms} are
#'          held constant. Numeric values are set to the mean (unless changed
#'          with the \code{typical}-argument), factors are set to their
#'          reference level and character vectors to their mode (most common
#'          element).
#'          \cr \cr
#'          \code{ggaverage()} computes the average predicted values, by calling
#'          \code{ggpredict()} with \code{full.data = TRUE}, where argument
#'          \code{newdata = model.frame(model)} is used in \code{predict()}.
#'          Hence, predictions are made on the model data. In this case, all
#'          remaining covariates that are not specified in \code{terms} are
#'          \emph{not} held constant, but vary between observations (and are
#'          kept as they happen to be). The predicted values are then averaged
#'          for each group (if any).
#'          \cr \cr
#'          Thus, \code{ggpredict()} can be considered as calculating marginal
#'          effects at the mean, while \code{ggaverage()} computes average
#'          marginal effects.
#'
#' @note Since data for \code{ggaverage()} comes from the model frame, not all
#'       possible combinations of values in \code{terms} might be present in the data,
#'       thus lines or confidence bands from \code{plot()} might not span over
#'       the complete x-axis-range.
#'       \cr \cr
#'       There are some limitations for certain model objects. For example,
#'       it is currently only possible to compute predicted risk scores for
#'       \code{coxph}-models, but not expected number of events nor survival
#'       probabilities.
#'       \cr \cr
#'       \code{polr}-models have an additional column \code{response.level},
#'       which indicates with which level of the response variable the predicted
#'       values are associated.
#'
#' @return A tibble (with \code{ggeffects} class attribute) with consistent data columns:
#'         \describe{
#'           \item{\code{x}}{the values of the first term in \code{terms}, used as x-position in plots.}
#'           \item{\code{predicted}}{the predicted values of the response, used as y-position in plots.}
#'           \item{\code{conf.low}}{the lower bound of the confidence interval for the predicted values.}
#'           \item{\code{conf.high}}{the upper bound of the confidence interval for the predicted values.}
#'           \item{\code{observed}}{if \code{full.data = TRUE}, this columns contains the observed values (the response vector).}
#'           \item{\code{residuals}}{if \code{full.data = TRUE}, this columns contains residuals.}
#'           \item{\code{group}}{the grouping level from the second term in \code{terms}, used as grouping-aesthetics in plots.}
#'           \item{\code{facet}}{the grouping level from the third term in \code{terms}, used to indicate facets in plots.}
#'         }
#'         For proportional odds logistic regression (see \code{\link[MASS]{polr}}),
#'         an additional column \code{response.level} is returned, which indicates
#'         the grouping of predictions based on the level of the model's response.
#'
#' @examples
#' data(efc)
#' fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
#'
#' ggpredict(fit, terms = "c12hour")
#' ggpredict(fit, terms = "c12hour", full.data = TRUE)
#' ggpredict(fit, terms = c("c12hour", "c172code"))
#' ggpredict(fit, terms = c("c12hour", "c172code", "c161sex"))
#'
#' # to plot ggeffects-objects, you can use the 'plot()'-function.
#' # the following examples show how to build your ggplot by hand.
#'
#' # plot predicted values, remaining covariates held constant
#' library(ggplot2)
#' mydf <- ggpredict(fit, terms = "c12hour")
#' ggplot(mydf, aes(x, predicted)) +
#'   geom_line() +
#'   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)
#'
#' # with "full.data = TRUE", remaining covariates vary between
#' # observations, so fitted values can be plotted
#' mydf <- ggpredict(fit, terms = "c12hour", full.data = TRUE)
#' ggplot(mydf, aes(x, predicted)) + geom_point()
#'
#' # you can add a smoothing-geom to show the linear trend of fitted values
#' ggplot(mydf, aes(x, predicted)) +
#'   geom_smooth(method = "lm", se = FALSE) +
#'   geom_point()
#'
#' # three variables, so we can use facets and groups
#' mydf <- ggpredict(
#'   fit,
#'   terms = c("c12hour", "c161sex", "c172code"),
#'   full.data = TRUE
#' )
#' ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
#'   stat_smooth(method = "lm", se = FALSE) +
#'   facet_wrap(~facet, ncol = 2)
#'
#' # average marginal effects
#' mydf <- ggaverage(fit, terms = c("c12hour", "c172code"))
#' ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
#'   stat_smooth(method = "lm", se = FALSE)
#'
#' # select specific levels for grouping terms
#' mydf <- ggpredict(fit, terms = c("c12hour", "c172code [1,3]", "c161sex"))
#' ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
#'   stat_smooth(method = "lm", se = FALSE) +
#'   facet_wrap(~facet) +
#'   labs(
#'     y = get_y_title(mydf),
#'     x = get_x_title(mydf),
#'     colour = get_legend_title(mydf)
#'   )
#'
#' # level indication also works for factors with non-numeric levels
#' # and in combination with numeric levels for other variables
#' library(sjmisc)
#' data(efc)
#' efc$c172code <- to_label(efc$c172code)
#' fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
#' ggpredict(fit, terms = c("c12hour",
#'   "c172code [low level of education, high level of education]",
#'   "c161sex [1]"))
#'
#' # use categorical value on x-axis, use axis-labels, add error bars
#' dat <- ggpredict(fit, terms = c("c172code", "c161sex"))
#' ggplot(dat, aes(x, predicted, colour = group)) +
#'   geom_point(position = position_dodge(.1)) +
#'   geom_errorbar(
#'     aes(ymin = conf.low, ymax = conf.high),
#'     position = position_dodge(.1)
#'   ) +
#'   scale_x_continuous(breaks = 1:3, labels = get_x_labels(dat))
#'
#' # 3-way-interaction with 2 continuous variables
#' data(efc)
#' # make categorical
#' efc$c161sex <- to_factor(efc$c161sex)
#' fit <- lm(neg_c_7 ~ c12hour * barthtot * c161sex, data = efc)
#' # select only levels 30, 50 and 70 from continuous variable Barthel-Index
#' dat <- ggpredict(fit, terms = c("c12hour", "barthtot [30,50,70]", "c161sex"))
#' ggplot(dat, aes(x = x, y = predicted, colour = group)) +
#'   stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
#'   facet_wrap(~facet) +
#'   labs(
#'     colour = get_legend_title(dat),
#'     x = get_x_title(dat),
#'     y = get_y_title(dat),
#'     title = get_title(dat)
#'   )
#'
#' # or with ggeffects' plot-method
#' \dontrun{
#' plot(dat, ci = FALSE)}
#'
#' @importFrom stats predict predict.glm na.omit model.frame
#' @importFrom dplyr "%>%" select mutate case_when arrange_ n_distinct
#' @importFrom sjmisc to_value to_factor to_label is_num_fac remove_empty_cols
#' @importFrom tibble has_name as_tibble
#' @importFrom purrr map
#' @export
ggpredict <- function(model, terms, ci.lvl = .95, type = c("fe", "re"), full.data = FALSE, typical = "mean", ...) {
  # check arguments
  type <- match.arg(type)


  # for gamm4 objects, we have a list with two items, mer and gam
  # extract just the mer-part then
  if (inherits(model, "list") && all(names(model %in% c("mer", "gam")))) {
    model <- model$mer
    class(model) <- "lmerMod"
  }

  if (inherits(model, "list"))
    purrr::map(model, ~ggpredict_helper(.x, terms, ci.lvl, type, full.data, typical, ...))
  else
    ggpredict_helper(model, terms, ci.lvl, type, full.data, typical, ...)
}


# workhorse that computes the predictions
# and creates the tidy data frames
ggpredict_helper <- function(model, terms, ci.lvl, type, full.data, typical, ...) {
  # check class of fitted model
  fun <- get_predict_function(model)

  # check terms argument
  terms <- check_vars(terms)

  # check model family, do we have count model?
  faminfo <- get_glm_family(model)

  # create logical for family
  binom_fam <- faminfo$is_bin
  poisson_fam <- faminfo$is_pois

  # get model frame
  fitfram <- get_model_frame(model, fe.only = FALSE)


  # expand model frame to grid of unique combinations, if
  # user not requested full data
  if (full.data) {
    expanded_frame <- get_sliced_data(fitfram, terms)
  } else {
    expanded_frame <- get_expanded_data(model, fitfram, terms, typical)
  }

  # save original frame, for labels
  ori.mf <- fitfram

  # clear argument from brackets
  terms <- get_clear_vars(terms)


  # compute predictions here -----
  fitfram <- select_prediction_method(fun, model, expanded_frame, ci.lvl, type, binom_fam, ...)


  # init legend labels
  legend.labels <- NULL

  # get axis titles and labels
  all.labels <-
    get_all_labels(ori.mf,
                   terms,
                   get_model_function(model),
                   binom_fam,
                   poisson_fam,
                   FALSE)

  # check for correct terms specification
  if (!all(terms %in% colnames(fitfram))) {
    stop("At least one term specified in `terms` is no valid model term.", call. = FALSE)
  }

  # now select only relevant variables: the predictors on the x-axis,
  # the predictions and the originial response vector (needed for scatter plot)
  mydf <-
    dplyr::select(fitfram, na.omit(match(
      c(terms, "predicted", "conf.low", "conf.high", "response.level"),
      colnames(fitfram)
    )))


  # no full data for certain models
  if (full.data && fun == "polr") {
    message("Argument `full.data` is not supported for this regression model.")
    full.data <- FALSE
  }


  # for full data, we can also get observed and residuals
  if (full.data) {
    mydf <- dplyr::mutate(mydf,
      observed = sjmisc::to_value(fitfram[[1]], start.at = 0, keep.labels = F),
      residuals = observed - predicted
    )
  } else {
    mydf <- dplyr::mutate(mydf, observed = NA, residuals = NA)
  }


  # with or w/o grouping factor?
  if (length(terms) == 1) {
    colnames(mydf)[1] <- "x"
    # convert to factor for proper legend
    mydf$group <- sjmisc::to_factor(1)
  } else {
    # name data depending on whether we have a facet-variable or not
    if (length(terms) == 2) {
      # for some models, like MASS::polr, we have an additional
      # column for the response category. So maximun ncol is 8, not 7
      max_value <- ifelse(fun == "polr", 8, 7)
      colnames(mydf)[1:2] <- c("x", "group")
      # reorder columns
      mydf <- mydf[, c(1, 3:max_value, 2)]
    } else {
      # for some models, like MASS::polr, we have an additional
      # column for the response category. So maximun ncol is 8, not 7
      max_value <- ifelse(fun == "polr", 9, 8)
      colnames(mydf)[1:3] <- c("x", "group", "facet")
      # reorder columns
      mydf <- mydf[, c(1, 4:max_value, 2:3)]
    }

    # if we have no full data, grouping variable may not be labelled
    # do this here, so we convert to labelled factor later
    if (!full.data) mydf <- add_groupvar_labels(mydf, ori.mf, terms)

    # convert to factor for proper legend
    mydf <- groupvar_to_label(mydf)

    # check if we have legend labels
    legend.labels <- sjlabelled::get_labels(mydf$group)
  }

  # if we had numeric variable w/o labels, these still might be numeric
  # make sure we have factors here for our grouping and facet variables
  if (is.numeric(mydf$group))
    mydf$group <- sjmisc::to_factor(mydf$group)

  if (tibble::has_name(mydf, "facet") && is.numeric(mydf$facet))
    mydf$facet <- sjmisc::to_factor(mydf$facet)


  # remember if x is factor and if we had full data
  x.is.factor <- ifelse(is.factor(mydf$x), "1", "0")
  has.full.data <- ifelse(full.data, "1", "0")

  # x needs to be numeric
  mydf$x <- sjmisc::to_value(mydf$x)

  # to tibble
  mydf <- mydf %>%
    tibble::as_tibble() %>%
    dplyr::arrange_("x", "group") %>%
    sjmisc::remove_empty_cols()

  # add raw data as well
  attr(mydf, "rawdata") <- get_raw_data(model, ori.mf, terms)

  # set attributes with necessary information
  set_attributes_and_class(data = mydf,
                           model = model,
                           t.title = all.labels$t.title,
                           x.title = all.labels$x.title,
                           y.title = all.labels$y.title,
                           l.title = all.labels$l.title,
                           legend.labels = legend.labels,
                           x.axis.labels = all.labels$axis.labels,
                           faminfo = faminfo,
                           x.is.factor = x.is.factor,
                           full.data = has.full.data)
}


#' @rdname ggpredict
#' @export
mem <- function(model, terms, ci.lvl = .95, type = c("fe", "re"), full.data = FALSE, typical = "mean", ...) {
  ggpredict(model, terms, ci.lvl, type, full.data, typical, ...)
}
