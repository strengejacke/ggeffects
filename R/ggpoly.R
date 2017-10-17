#' @title Get marginal effects for polynomial terms
#' @name ggpoly
#'
#' @description \code{ggpoly()} computes marginal effects for polynomial terms.
#'              The result is returned as tidy data frame.
#'
#' @param model A fitted model object, or a list of model objects. Any model
#'          that is supported by the \CRANpkg{effects}-package should work.
#' @param poly.term Name of the polynomial term in \code{model}, as string.
#' @inheritParams gginteraction
#' @inheritParams ggeffect
#' @inheritParams ggpredict
#'
#' @return A tibble (with \code{ggeffects} class attribute) with consistent data columns:
#'         \describe{
#'           \item{\code{x}}{the values of the first term in \code{terms}, used as x-position in plots.}
#'           \item{\code{predicted}}{the predicted values, used as y-position in plots.}
#'           \item{\code{conf.low}}{the lower bound of the confidence interval for the predicted values.}
#'           \item{\code{conf.high}}{the upper bound of the confidence interval for the predicted values.}
#'           \item{\code{group}}{the grouping level from the second term in \code{terms}, used as grouping-aesthetics in plots.}
#'         }
#'
#' @note \code{ggpoly()} is just an alternative call to \code{ggpredict()} for
#'       polynomial model terms. It may work for certain models that are not
#'       yet supported by \code{ggpredict()}. Otherwise, there should be no
#'       difference in the results from \code{ggpoly()} and \code{ggpredict()}.
#'
#' @examples
#' data(efc)
#' fit <- lm(
#'   tot_sc_e ~ c12hour + e42dep + e17age + I(e17age^2) + I(e17age^3),
#'   data = efc
#' )
#' dat <- ggpoly(fit, "e17age")
#'
#' # this would give the same result
#' ggpredict(fit, "e17age")
#'
#' library(ggplot2)
#' ggplot(dat, aes(x, predicted)) +
#'   stat_smooth(se = FALSE) +
#'   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .15) +
#'   labs(x = get_x_title(dat), y = get_y_title(dat))
#'
#' \dontrun{
#' # or:
#' plot(dat)}
#'
#' @importFrom effects effect
#' @export
ggpoly <- function(model, poly.term, ci.lvl = .95, ...) {
  if (inherits(model, "list"))
    purrr::map(model, ~ggpoly_helper(.x, poly.term, ci.lvl, ...))
  else
    ggpoly_helper(model, poly.term, ci.lvl, ...)
}


#' @importFrom sjstats model_frame
ggpoly_helper <- function(model, poly.term, ci.lvl, ...) {
  # get model frame
  mf <- sjstats::model_frame(model)

  # check model family, do we have count model?
  faminfo <- get_glm_family(model)

  # get model data column names
  cn <- colnames(mf)
  xl <- NULL

  # get variable label for response
  y.title <- sjlabelled::get_label(mf[[1]], def.value = cn[1])

  # get variable label for response
  x.title <- sjlabelled::get_label(mf[[poly.term]], def.value = poly.term)

  # argument check: poly.term required and
  # polynomial must be found in model
  if (!missing(poly.term) && !is.null(poly.term)) {
    # check for simple poly term, using I(x^2) + I(x^3) etc.
    poly.found <- any(cn == poly.term)

    # found poly? If yes, get range
    if (poly.found) {
      xl <- list(x = sort(unique(stats::na.omit(mf[[poly.term]]))))
    } else {
      # not found? than check for poly term, using poly(x, degree = 3)
      if (!poly.found) {
        # find term names
        pt <- unique(sub("^poly\\(([^,)]*).*", "\\1", cn))
        # found poly-term?
        poly.found <- any(pt == poly.term)
      }

      # not found? last try, looking for splines
      if (!poly.found) {
        # find term names
        pt <- unique(sub("^bs\\(([^,)]*).*", "\\1", cn))
        # found poly-term?
        poly.found <- any(pt == poly.term)
      }
    }

    # no polynomial term found...
    if (!poly.found) {
      stop("`poly.term` not given, or not found in model. Please check name of polynomial term.", call. = FALSE)
      # xl already defined? If not, do it now!
    } else if (is.null(xl)) {
      pr <- range(mf[[poly.term]], na.rm = TRUE)
      # create levels paramater for effect method
      xl <- list(x = seq(pr[1], pr[2]))
    }
  } else {
    stop("`poly.term` must be specified.", call. = FALSE)
  }

  # compute marginal effects of polynomial
  names(xl) <- poly.term
  eff <- effects::effect(poly.term, model, xlevels = xl, ...)

  # build data frame, with raw values
  # from polynomial term, predicted response
  # and lower/upper ci
  mydf <-
    data.frame(
      x = eff$x[[poly.term]],
      predicted = eff$transformation$inverse(eta = eff$fit),
      conf.low = eff$transformation$inverse(eta = eff$lower),
      conf.high = eff$transformation$inverse(eta = eff$upper),
      group = as.factor(1)
    )

  # add raw data as well
  attr(mydf, "rawdata") <- get_raw_data(model, mf, poly.term)

  # set attributes with necessary information
  set_attributes_and_class(data = mydf,
                           model = model,
                           t.title = "Marginal effects for polynomial term",
                           x.title = x.title,
                           y.title = y.title,
                           l.title = NULL,
                           legend.labels = NULL,
                           x.axis.labels = NULL,
                           faminfo = faminfo,
                           x.is.factor = "0",
                           full.data = "0")
}
