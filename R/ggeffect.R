#' @rdname ggpredict
#'
#' @importFrom stats na.omit
#' @importFrom sjlabelled as_numeric
#' @importFrom insight find_predictors link_inverse print_color
#' @export
ggeffect <- function(model, terms, ci.lvl = .95, ...) {

  if (!requireNamespace("effects", quietly = TRUE)) {
    message("Package `effects` is not available, but needed for `ggeffect()`. Either install package `effects`, or use `ggpredict()`. Calling `ggpredict()` now.", call. = FALSE)
    return(ggpredict(model = model, terms = terms, ci.lvl = ci.lvl))
  }

  model_name <- deparse(substitute(model))

  # check if terms are a formula
  if (!missing(terms) && !is.null(terms) && inherits(terms, "formula")) {
    terms <- all.vars(terms)
  }

  if (inherits(model, "list")  && !inherits(model, c("bamlss", "maxLik"))) {
    res <- lapply(model, function(.x) ggeffect_helper(.x, terms, ci.lvl, ...))
  } else {
    if (missing(terms) || is.null(terms)) {
      predictors <- insight::find_predictors(model, effects = "fixed", component = "conditional", flatten = TRUE)
      res <- lapply(
        predictors,
        function(.x) {
          tmp <- ggeffect_helper(model, terms = .x, ci.lvl, ...)
          if (!is.null(tmp)) tmp$group <- .x
          tmp
        }
      )
      no_results <- sapply(res, is.null)
      res <- .compact_list(res)
      if (!is.null(res) && !.is_empty(res)) {
        names(res) <- predictors[!no_results]
        class(res) <- c("ggalleffects", class(res))
      } else {
        res <- NULL
      }
    } else {
      res <- ggeffect_helper(model, terms, ci.lvl, ...)
    }
  }

  if (!is.null(res)) {
    attr(res, "model.name") <- model_name
  }
  res
}


ggeffect_helper <- function(model, terms, ci.lvl, ...) {

  # check terms argument
  original_terms <- terms <- .check_vars(terms, model)
  cleaned_terms <- .clean_terms(terms)

  # get model frame
  original_model_frame <- insight::get_data(model)

  # get model family
  model_info <- .get_model_info(model)

  # check whether we have an argument "transformation" for effects()-function
  # in this case, we need another default title, since we have
  # non-transformed effects
  additional_dot_args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  # check whether we have a "transformation" argument
  t.add <- which(names(additional_dot_args) == "transformation")
  # if we have a "transformation" argument, and it's NULL,
  # no transformation of scale
  no.transform <- !.is_empty(t.add) && is.null(eval(additional_dot_args[[t.add]]))


  # check if we have specific levels in square brackets
  at_values <- .get_representative_values(terms, original_model_frame)

  # clear argument from brackets
  terms <- .clean_terms(terms)

  # check for character vectors, transform to factor
  is_char <- sapply(terms, function(.i) is.character(original_model_frame[[.i]]))
  if (any(is_char)) {
    for (.i in terms[is_char]) {
      original_model_frame[[.i]] <- as.factor(original_model_frame[[.i]])
    }
  }

  # fix remaining x-levels
  conditional_terms <- which(!(terms %in% names(at_values)))
  if (!.is_empty(conditional_terms)) {
    xl <- .prettify_data(conditional_terms, original_model_frame, terms)
    names(xl) <- terms[conditional_terms]
    at_values <- c(at_values, xl)
  }

  # restore inital order of focal predictors
  at_values <- at_values[match(terms, names(at_values))]

  # compute marginal effects for each model term
  eff <- tryCatch(
    {
      suppressWarnings(
        effects::Effect(
          focal.predictors = terms,
          mod = model,
          xlevels = at_values,
          confidence.level = ci.lvl,
          ...
        )
      )
    },
    error = function(e) {
      insight::print_color("Can't compute marginal effects, 'effects::Effect()' returned an error.\n\n", "red")
      cat(sprintf("Reason: %s\n", e$message))
      cat("You may try 'ggpredict()' or 'ggemmeans()'.\n\n")
      NULL
    }
  )

  # return NULL on error
  if (is.null(eff)) return(NULL)

  # build data frame, with raw values
  # predicted response and lower/upper ci

  if (inherits(model, c("polr", "clm", "clm2", "clmm", "clmm2", "multinom"))) {

    # for categorical outcomes, we need to gather the data
    # from effects to get a single data frame

    eff.logits <- as.data.frame(eff$logit, stringsAsFactors = FALSE)
    tmp <- cbind(eff$x, eff.logits)
    ft <- (ncol(tmp) - ncol(eff.logits) + 1):ncol(tmp)
    tmp <- .gather(tmp, names_to = "response.level", values_to = "predicted", colnames(tmp)[ft])

    fx.term <- eff$term

    colnames(tmp)[1] <- "x"
    if (length(terms) > 1) colnames(tmp)[2] <- "group"
    if (length(terms) > 2) colnames(tmp)[3] <- "facet"

    if (!is.null(ci.lvl) && !is.na(ci.lvl))
      ci <- 1 - ((1 - ci.lvl) / 2)
    else
      ci <- .975

    # same for standard errors. we need to gather all data frames together,
    # compute CI manually and then also fix column names.

    eff.se.logits <- as.data.frame(eff$se.logit)
    tmp2 <- .gather(eff.se.logits, names_to = "response.level", values_to = "se", colnames(eff.se.logits))
    tmp2$conf.low <- tmp$predicted - stats::qnorm(ci) * tmp2$se
    tmp2$conf.high <- tmp$predicted + stats::qnorm(ci) * tmp2$se
    tmp2$std.error <- tmp2$se

    tmp <- cbind(tmp, tmp2[, c("std.error", "conf.low", "conf.high")])
    tmp$response.level <- substr(tmp$response.level, 7, max(nchar(tmp$response.level)))
  } else {

    # check for multi response

    .ne <- names(eff)
    .mv <- insight::find_response(model, combine = FALSE)

    if (length(.ne) == length(.mv) && all.equal(.ne, .mv)) {
      l <- lapply(names(eff), function(.x) {
        tmpl <- data.frame(
          x = eff[[.x]]$x[[terms[1]]],
          predicted = eff[[.x]]$fit,
          std.error = eff[[.x]]$se,
          conf.low = eff[[.x]]$lower,
          conf.high = eff[[.x]]$upper,
          response.level = .x,
          stringsAsFactors = FALSE
        )
        .create_eff_group(tmpl, terms, eff, sub = .x)
      })
      tmp <- do.call(rbind, l)
      fx.term <- eff[[1]]$term
    } else {
      tmp <-
        data.frame(
          x = eff$x[[terms[1]]],
          predicted = eff$fit,
          std.error = eff$se,
          conf.low = eff$lower,
          conf.high = eff$upper,
          stringsAsFactors = FALSE
        )

      tmp <- .create_eff_group(tmp, terms, eff, sub = NULL)

      # effects-package keeps the order of numeric value as they are
      # returned by "unique()", so we want to sort the data frame
      # in the order of ascending values

      if (is.numeric(eff$data[[terms[1]]])) tmp <- tmp[order(tmp$x), , drop = FALSE]
      fx.term <- eff$term
    }
  }


  if (!no.transform) {
    linv <- insight::link_inverse(model)
    tmp$predicted <- linv(tmp$predicted)
    tmp$conf.low <- linv(tmp$conf.low)
    tmp$conf.high <- linv(tmp$conf.high)
  }


  # init legend labels
  legend.labels <- NULL

  # get axis titles and labels
  all.labels <- .get_axis_titles_and_labels(
    original_model_frame,
    terms,
    .get_model_function(model),
    model_info = model_info,
    no.transform,
    type = NULL
  )


  # slice data, only select observations that have specified
  # levels for the grouping variables

  # for numeric values with many decimal places, we need to round
  if (.frac_length(tmp$x) > 5)
    filter.keep <- round(tmp$x, 5) %in% round(at_values[[1]], 5)
  else
    filter.keep <- tmp$x %in% at_values[[1]]

  tmp <- tmp[filter.keep, , drop = FALSE]

  # slice data, only select observations that have specified
  # levels for the facet variables
  if (length(at_values) > 1) {
    filter.keep <- tmp$group %in% at_values[[2]]
    tmp <- tmp[filter.keep, , drop = FALSE]
  }

  # slice data, only select observations that have specified
  # levels for the facet variables
  if (length(at_values) > 2) {
    filter.keep <- tmp$facet %in% at_values[[3]]
    tmp <- tmp[filter.keep, , drop = FALSE]
  }


  # label grouping variables, for axis and legend labels in plot
  if (length(terms) > 1) {
    # grouping variable may not be labelled
    # do this here, so we convert to labelled factor later
    tmp <- .add_labels_to_groupvariable(tmp, original_model_frame, terms)

    # convert to factor for proper legend
    tmp <- .groupvariable_to_labelled_factor(tmp)

    # check if we have legend labels
    legend.labels <- sjlabelled::get_labels(tmp$group, attr.only = FALSE, drop.unused = TRUE)
  }


  # convert to data frame
  result <- as.data.frame(tmp, stringsAsFactors = FALSE)

  if (is.null(result$group)) {
    result$group <- as.factor(1)
  }

  if (length(terms) > 1) {
    attr(result, "continuous.group") <- is.numeric(original_model_frame[[terms[2]]]) & is.null(attr(original_model_frame[[terms[2]]], "labels"))
  } else {
    attr(result, "continuous.group") <- FALSE
  }

  # add raw data as well
  attr(result, "rawdata") <- .get_raw_data(model, original_model_frame, terms)


  x_v <- original_model_frame[[fx.term]]
  if (is.null(x_v))
    xif <- ifelse(is.factor(tmp$x), "1", "0")
  else
    xif <- ifelse(is.factor(x_v), "1", "0")

  attr(result, "x.is.factor") <- xif

  # set attributes with necessary information
  .set_attributes_and_class(
    data = result,
    model = model,
    t.title = all.labels$t.title,
    x.title = all.labels$x.title,
    y.title = all.labels$y.title,
    l.title = all.labels$l.title,
    legend.labels = legend.labels,
    x.axis.labels = all.labels$axis.labels,
    model_info = model_info,
    terms = cleaned_terms,
    original_terms = original_terms,
    ci.lvl = ci.lvl
  )
}


.create_eff_group <- function(tmp, terms, eff, sub) {

  if (!is.null(sub)) {
    fx <- eff[[sub]]
  } else {
    fx <- eff
  }

  if (isTRUE(fx$variables[[1]]$is.factor)) {
    tmp$x <- factor(tmp$x, levels = fx$variables[[1]]$levels)
  }

  # with or w/o grouping factor?
  if (length(terms) == 1) {
    # convert to factor for proper legend
    tmp$group <- as.factor(1)
  } else if (length(terms) == 2) {
    tmp$group <- if (isTRUE(fx$variables[[2]]$is.factor)) {
      factor(fx$x[[terms[2]]], levels = fx$variables[[2]]$levels)
    } else {
      as.factor(fx$x[[terms[2]]])
    }
  } else {
    tmp$group <- if (isTRUE(fx$variables[[2]]$is.factor)) {
      factor(fx$x[[terms[2]]], levels = fx$variables[[2]]$levels)
    } else {
      as.factor(fx$x[[terms[2]]])
    }
    tmp$facet <- if (isTRUE(fx$variables[[3]]$is.factor)) {
      factor(fx$x[[terms[3]]], levels = fx$variables[[3]]$levels)
    } else {
      as.factor(fx$x[[terms[3]]])
    }
  }

  tmp
}
