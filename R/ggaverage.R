#' @rdname ggpredict
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @export
ggaverage <- function(model,
                      terms,
                      ci.lvl = .95,
                      type = c("fe", "re", "fe.zi", "re.zi"),
                      typical = "mean",
                      condition = NULL,
                      ppd = FALSE,
                      x.as.factor = FALSE,
                      vcov.fun = NULL,
                      vcov.type = NULL,
                      vcov.args = NULL,
                      interval = c("confidence", "prediction"),
                      x.cat,
                      ...) {

  type <- match.arg(type)
  if (!missing(x.cat)) x.as.factor <- x.cat

  # get predictions for full data
  dat <- ggpredict(
    model = model,
    terms = terms,
    ci.lvl = ci.lvl,
    type = type,
    full.data = TRUE,
    typical = typical,
    ppd = ppd,
    x.as.factor = x.as.factor,
    condition = condition,
    vcov.fun = vcov.fun,
    vcov.type = vcov.type,
    vcov.args = vcov.args,
    interval = interval,
    ...
  )

  # remove columns with obs and resid
  rem.col <- which(is.na(match(colnames(dat), c("observed", "residuals"))))
  dat <- dplyr::select(dat, !! rem.col)

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


  # add back attributes. therefore, we first copy the attributes from the
  # original data frame and delete the common attributes, like class etc.
  # and then add attributes to our final df
  a <- attributes(dat)
  # a[c("names", "row.names", "class", "dim", "dimnames")] <- NULL
  a[names(a) %in% names(attributes(zus))] <- NULL
  attributes(zus) <- c(attributes(zus), a)
  # no full data for averages
  attr(zus, "full.data") <- "0"

  # add class attribute
  class(zus) <- c("ggeffects", class(zus))

  zus
}


# this method simply computes the mean values of predictions, se and ci
# to get average marginal effects for categorical variables
# As categorical variables do not necessarily need to follow a "linear"
# (or other distribution based) trend, we can simply calculate the mean
#' @importFrom dplyr group_by summarise ungroup
get_average_values <- function(dat) {
  # do summary, depending on third group
  if (obj_has_name(dat, "facet")) {
    dat <- dplyr::group_by(dat, .data$x, .data$group, .data$facet)
  } else {
    dat <- dplyr::group_by(dat, .data$x, .data$group)
  }

  # get average values for predictions, SE and CI
  zus <- dplyr::summarise(
    dat,
    predicted = mean(.data$predicted),
    conf.low = mean(.data$conf.low, na.rm = TRUE),
    conf.high = mean(.data$conf.high, na.rm = TRUE)
  ) %>%
    dplyr::ungroup()

  # sort columns
  if (obj_has_name(dat, "facet")) {
    zus <- zus[, c(1, 4:6, 2:3)]
  } else {
    zus <- zus[, c(1, 3:5, 2)]
  }

  zus
}


# this method prepares the data to get smoothed predictions for
# average effects
#' @importFrom dplyr group_by summarise ungroup arrange mutate select
#' @importFrom purrr map
#' @importFrom stats predict loess lm
#' @importFrom sjmisc var_rename
get_smoothed_avg <- function(dat) {
  # do summary, depending on third group
  if (obj_has_name(dat, "facet")) {
    tmp <- dplyr::group_by(dat, .data$x, .data$group, .data$facet)
  } else {
    tmp <- dplyr::group_by(dat, .data$x, .data$group)
  }

  # get average prediction. this is quite strait forward...
  zus <- tmp %>%
    dplyr::summarise(predicted = mean(.data$predicted)) %>%
    dplyr::ungroup()

  if (obj_has_name(dat, "facet")) {
    zus <- zus %>%
      dplyr::arrange(.data$x, .data$group, .data$facet) %>%
      dplyr::group_by(.data$group, .data$facet)
  } else {
    zus <- zus %>%
      dplyr::arrange(.data$x, .data$group) %>%
      dplyr::group_by(.data$group)
  }


  # get family and link function, for smoother
  fam <- attr(dat, "family", exact = TRUE)
  link <- attr(dat, "link", exact = TRUE)


  # since average marginal effects may vary in their slopes by group,
  # we need to predict the values for each group separately.
  if (fam == "gaussian" && link == "identity") {
    # for linear models, compute linear trend
    zus <- zus %>%
      .nest(cn = "datacol") %>%
      dplyr::mutate(
        models = purrr::map(.data$datacol, ~stats::lm(
          formula = predicted ~ x,
          data = .x
        )))
  } else {
    # furthermore, we can't compute a glm on predicted values of a glm - so we use
    # instead a local smoother to achieve predicted values for average effects
    zus <- zus %>%
      .nest(cn = "datacol") %>%
      dplyr::mutate(
        models = purrr::map(.data$datacol, ~stats::loess(
          formula = predicted ~ x,
          family = "symmetric",
          degree = 1,
          data = .x
        )))
  }


  # now compute new predictions of average marginal effects
  zus <- zus %>%
    dplyr::mutate(avgpred = purrr::map(.data$models, ~as.vector(stats::predict(.x)))) %>%
    dplyr::select(-.data$models) %>%
    .unnest("datacol", "avgpred") %>%
    dplyr::ungroup()


  zus$conf.low <- NA
  zus$conf.high <- NA

  # remove standard predicted, and replace with avg predicted
  zus <- zus %>%
    dplyr::select(-.data$predicted) %>%
    sjmisc::var_rename(avgpred = "predicted")

  # re-arrange columns
  if (obj_has_name(dat, "facet"))
    zus <- zus[, c(4, 3, 5:6, 1:2)]
  else
    zus <- zus[, c(3, 2, 4:5, 1)]

  zus
}
