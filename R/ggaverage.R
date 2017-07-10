utils::globalVariables(c("conf.low", "conf.high", "datacol", "models"))

#' @rdname ggpredict
#' @importFrom dplyr group_by_ summarise arrange_ mutate ungroup select_
#' @importFrom stats predict loess lm
#' @importFrom tidyr nest unnest
#' @importFrom sjmisc var_rename
#' @export
ggaverage <- function(model, terms, ci.lvl = .95, type = c("fe", "re"), typical = "mean", ...) {
  # get predictions for full data
  dat <- ggpredict(model, terms, ci.lvl, type, full.data = TRUE, typical, ...)

  # remove columns with obs and resid
  dat <- dplyr::select_(dat, "-observed", "-residuals")

  # do summary, depending on third group
  if (tibble::has_name(dat, "facet")) {
    dat <- dplyr::group_by_(dat, "x", "group", "facet")
  } else {
    dat <- dplyr::group_by_(dat, "x", "group")
  }

  # is x a factor?
  xif <- attr(dat, "x.is.factor", exact = TRUE)
  x_is_factor <- !is.null(xif) && xif == "1"


  # check if x is a factor. We can then simply take mean values for
  # standard error and confidence intervals. For continuous variables,
  # we have no linear trend for the predicted values, and hence no "proper"
  # confidence bands that can be plotted with geom_ribbon.
  if (x_is_factor) {
    zus <- get_average_values(dat)
  } else {
    zus <- get_smoothed_avg(dat)
  }


  # to tibble
  zus <- tibble::as_tibble(zus)


  # add back attributes. therefore, we first copy the attributes from the
  # original data frame and delete the common attributes, like class etc.
  # and then add attributes to our final df
  a <- attributes(dplyr::ungroup(dat))
  a[c("names", "row.names", "class", "dim", "dimnames")] <- NULL
  attributes(zus) <- c(attributes(zus), a)
  # no full data for averages
  attr(zus, "full.data") <- "0"

  # add class attribute
  class(zus) <- c("ggeffects", class(zus))

  zus
}


#' @rdname ggpredict
#' @export
ame <- function(model, terms, ci.lvl = .95, type = c("fe", "re"), typical = "mean", ...) {
  ggaverage(model, terms, ci.lvl, type, typical, ...)
}


# this method simply computes the mean values of predictions, se and ci
# to get average marginal effects for categorical variables
# As categorical variables do not necessarily need to follow a "linear"
# (or other distribution based) trend, we can simply calculate the mean
get_average_values <- function(dat) {
  # get average values for predictions, SE and CI
  zus <- dplyr::summarise(
    dat,
    predicted = mean(predicted),
    conf.low = mean(conf.low, na.rm = TRUE),
    conf.high = mean(conf.high, na.rm = TRUE)
  ) %>%
    dplyr::ungroup()

  # sort columns
  if (tibble::has_name(dat, "facet")) {
    zus <- zus[, c(1, 4:6, 2:3)]
  } else {
    zus <- zus[, c(1, 3:5, 2)]
  }

  zus
}


# this method prepares the data to get smoothed predictions for
# average effects
get_smoothed_avg <- function(dat) {
  # get average prediction. this is quite strait forward...
  zus <- dplyr::summarise(dat, predicted = mean(predicted)) %>% dplyr::ungroup()

  if (tibble::has_name(dat, "facet")) {
    zus <- zus %>%
      dplyr::arrange_("x", "group", "facet") %>%
      dplyr::group_by_("group", "facet")
  } else {
    zus <- zus %>%
      dplyr::arrange_("x", "group") %>%
      dplyr::group_by_("group")
  }


  # get family and link function, for smoother
  fam <- attr(dat, "family", exact = TRUE)
  link <- attr(dat, "link", exact = TRUE)


  # since average marginal effects may vary in their slopes by group,
  # we need to predict the values for each group separately.
  if (fam == "gaussian" && link == "identity") {
    # for linear models, compute linear trend
    zus <- zus %>%
      tidyr::nest(.key = datacol) %>%
      dplyr::mutate(
        models = purrr::map(datacol, ~stats::lm(
          formula = predicted ~ x,
          data = .x
        )))
  } else {
    # furthermore, we can't compute a glm on predicted values of a glm - so we use
    # instead a local smoother to achieve predicted values for average effects
    zus <- zus %>%
      tidyr::nest(.key = datacol) %>%
      dplyr::mutate(
        models = purrr::map(datacol, ~stats::loess(
          formula = predicted ~ x,
          family = "symmetric",
          degree = 1,
          data = .x
        )))
  }


  # now compute new predictions of average marginal effects
  zus <- zus %>%
    dplyr::mutate(avgpred = purrr::map(models, ~as.vector(stats::predict(.x)))) %>%
    dplyr::select_("-models") %>%
    tidyr::unnest() %>%
    dplyr::ungroup()


  zus$conf.low <- NA
  zus$conf.high <- NA

  # remove standard predicted, and replace with avg predicted
  zus <- zus %>%
    dplyr::select_("-predicted") %>%
    sjmisc::var_rename(avgpred = "predicted")

  # re-arrange columns
  if (tibble::has_name(dat, "facet"))
    zus <- zus[, c(4, 3, 5:6, 1:2)]
  else
    zus <- zus[, c(3, 2, 4:5, 1)]

  zus
}
