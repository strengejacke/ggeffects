#' @rdname ggpredict
#'
#' @importFrom purrr map map2
#' @importFrom sjstats pred_vars resp_var model_family model_frame link_inverse
#' @importFrom dplyr if_else case_when bind_rows mutate
#' @importFrom tibble as_tibble
#' @importFrom sjmisc is_empty str_contains
#' @importFrom stats na.omit
#' @importFrom effects Effect
#' @importFrom sjlabelled as_numeric
#' @importFrom rlang .data
#' @export
ggeffect <- function(model, terms, ci.lvl = .95, x.as.factor = FALSE, ...) {
  if (inherits(model, "list"))
    res <- purrr::map(model, ~ggeffect_helper(.x, terms, ci.lvl, x.as.factor, ...))
  else {
    if (missing(terms) || is.null(terms)) {
      predictors <- sjstats::pred_vars(model)
      res <- purrr::map(
        predictors,
        function(.x) {
          tmp <- ggeffect_helper(model, terms = .x, ci.lvl, x.as.factor,...)
          tmp$group <- .x
          tmp
        }
      )
      names(res) <- predictors
      class(res) <- c("ggalleffects", class(res))
    } else {
      res <- ggeffect_helper(model, terms, ci.lvl, x.as.factor, ...)
    }
  }

  res
}


#' @importFrom sjstats model_frame
ggeffect_helper <- function(model, terms, ci.lvl, x.as.factor, ...) {
  # check terms argument
  terms <- check_vars(terms)

  # get model frame
  fitfram <- sjstats::model_frame(model)

  # get model family
  faminfo <- sjstats::model_family(model)

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
  x.levels <- get_xlevels_vector(terms, fitfram)

  # clear argument from brackets
  terms <- get_clear_vars(terms)

  # fix remaining x-levels
  xl.remain <- which(!(terms %in% names(x.levels)))
  if (!sjmisc::is_empty(xl.remain)) {
    xl <- prettify_data(xl.remain, fitfram, terms)
    names(xl) <- terms[xl.remain]
    x.levels <- c(x.levels, xl)
  }

  # restore inital order of focal predictors
  x.levels <- x.levels[match(terms, names(x.levels))]

  # compute marginal effects for each model term
  eff <- effects::Effect(
    focal.predictors = terms,
    mod = model,
    xlevels = x.levels,
    confidence.level = ci.lvl,
    ...
  )

  # build data frame, with raw values
  # predicted response and lower/upper ci

  if (inherits(model, c("polr", "clm"))) {
    eff.logits <- as.data.frame(eff$logit)
    tmp <- cbind(eff$x, eff.logits)
    ft <- (ncol(tmp) - ncol(eff.logits) + 1):ncol(tmp)
    tmp <- tidyr::gather(tmp, key = "response.level", value = "predicted", !! ft)

    colnames(tmp)[1] <- "x"
    if (length(terms) > 1) colnames(tmp)[2] <- "group"
    if (length(terms) > 2) colnames(tmp)[3] <- "facet"

    if (!is.null(ci.lvl) && !is.na(ci.lvl))
      ci <- 1 - ((1 - ci.lvl) / 2)
    else
      ci <- .975

    eff.se.logits <- as.data.frame(eff$se.logit)
    tmp2 <- tidyr::gather(eff.se.logits, key = "response.level", value = "se")
    tmp2$conf.low <- tmp$predicted - stats::qnorm(ci) * tmp2$se
    tmp2$conf.high <- tmp$predicted + stats::qnorm(ci) * tmp2$se

    tmp <- dplyr::bind_cols(tmp, tmp2[, c("conf.low", "conf.high")])
    tmp$response.level <- substr(tmp$response.level, 7, max(nchar(tmp$response.level)))
  } else {
    tmp <-
      data.frame(
        x = eff$x[[terms[1]]],
        predicted = eff$fit,
        conf.low = eff$lower,
        conf.high = eff$upper
      )

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
    }

    if (is.numeric(eff$data[[terms[1]]])) tmp <- tmp[order(tmp$x), ]
  }


  if (!no.transform) {
    linv <- sjstats::link_inverse(model)
    tmp$predicted <- linv(tmp$predicted)
    tmp$conf.low <- linv(tmp$conf.low)
    tmp$conf.high <- linv(tmp$conf.high)
  }


  # init legend labels
  legend.labels <- NULL

  # get axis titles and labels
  all.labels <- get_all_labels(
    fitfram,
    terms,
    get_model_function(model),
    binom_fam,
    poisson_fam,
    no.transform
  )


  # slice data, only select observations that have specified
  # levels for the grouping variables
  filter.keep <- tmp$x %in% x.levels[[1]]
  tmp <- tmp[filter.keep, , drop = FALSE]

  # slice data, only select observations that have specified
  # levels for the facet variables
  if (length(x.levels) > 1) {
    filter.keep <- tmp$group %in% x.levels[[2]]
    tmp <- tmp[filter.keep, , drop = FALSE]
  }

  # slice data, only select observations that have specified
  # levels for the facet variables
  if (length(x.levels) > 2) {
    filter.keep <- tmp$facet %in% x.levels[[3]]
    tmp <- tmp[filter.keep, , drop = FALSE]
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


  x_v <- fitfram[[eff$term]]
  if (is.null(x_v))
    xif <- ifelse(is.factor(tmp$x), "1", "0")
  else
    xif <- ifelse(is.factor(x_v), "1", "0")

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
      x.is.factor = xif,
      full.data = "0"
    )

  # make x numeric
  if (!x.as.factor) mydf$x <- sjlabelled::as_numeric(mydf$x, keep.labels = FALSE)

  mydf
}
