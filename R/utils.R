#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' @keywords internal
data_frame <- function(...) {
  x <- data.frame(..., stringsAsFactors = FALSE)
  rownames(x) <- NULL
  x
}


# get color palette
#' @keywords internal
#' @importFrom scales brewer_pal grey_pal
get_colors <- function(geom.colors, collen) {
  # check for corrct color argument
  if (!is.null(geom.colors)) {
    # check for color brewer palette
    if (is.brewer.pal(geom.colors[1]) && collen == 1) {
      geom.colors <- "black"
    } else if (is.brewer.pal(geom.colors[1])) {
      geom.colors <- scales::brewer_pal(palette = geom.colors[1])(collen)
    } else if (geom.colors[1] %in% names(ggeffects_colors)) {
      geom.colors <- ggeffects_pal(palette = geom.colors[1], n = collen)
    } else if (geom.colors[1] == "gs") {
      geom.colors <- scales::grey_pal()(collen)
      # do we have correct amount of colours?
    } else if (geom.colors[1] == "bw") {
      geom.colors <- rep("black", times = collen)
      # do we have correct amount of colours?
    } else if (length(geom.colors) > collen) {
      # shorten palette
      geom.colors <- geom.colors[1:collen]
    } else if (length(geom.colors) < collen) {
      # warn user abount wrong color palette
      warning(sprintf("Insufficient length of color palette provided. %i color values needed.", collen), call. = F)
      # set default palette
      geom.colors <- scales::brewer_pal(palette = "Set1")(collen)
    }
  } else {
    geom.colors <- scales::brewer_pal(palette = "Set1")(collen)
  }

  geom.colors
}


# check whether a color value is indicating
# a color brewer palette
#' @keywords internal
is.brewer.pal <- function(pal) {
  bp.seq <- c("BuGn", "BuPu", "GnBu", "OrRd", "PuBu", "PuBuGn", "PuRd", "RdPu",
              "YlGn", "YlGnBu", "YlOrBr", "YlOrRd", "Blues", "Greens", "Greys",
              "Oranges", "Purples", "Reds")

  bp.div <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu",
              "RdYlGn", "Spectral")

  bp.qul <- c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
              "Set2", "Set3")

  bp <- c(bp.seq, bp.div, bp.qul)

  any(bp == pal)
}


#' @keywords internal
check_vars <- function(terms, model) {
  if (missing(terms) || is.null(terms)) {
    stop("`terms` needs to be a character vector with at least one predictor names: one term used for the x-axis, more optional terms as grouping factors.", call. = F)
  }

  # check for correct length of vector
  if (length(terms) > 4) {
    message("`terms` must have not more than four values. Using first four values now.")
    terms <- terms[1:4]
  }

  if (!is.null(model)) {
    tryCatch(
      {
        pv <- insight::find_predictors(model, effects = "all", component = "all", flatten = TRUE)
        clean.terms <- get_clear_vars(terms)
        for (i in clean.terms) {
          if (!(i %in% pv)) {
            insight::print_color(sprintf("`%s` was not found in model terms. Maybe misspelled?\n", i), "red")
          }

        }
      },
      error = function(x) { NULL }
    )
  }

  terms
}


#' @importFrom dplyr filter
#' @importFrom stats complete.cases
#' @importFrom sjlabelled as_label as_numeric
get_raw_data <- function(model, mf, terms) {
  # for matrix variables, don't return raw data
  if (any(purrr::map_lgl(mf, is.matrix)) && !inherits(model, c("coxph", "coxme")))
    return(NULL)

  # remove missings from model frame
  mf <- dplyr::filter(mf, stats::complete.cases(mf))

  if (!all(insight::find_response(model, combine = FALSE) %in% colnames(mf)))
    return(NULL)

  # get response and x-value
  response <- insight::get_response(model)
  x <- sjlabelled::as_numeric(mf[[terms[1]]])

  # for cox-models, modify response
  if (inherits(model, "coxph")) {
    response <- response[[2]]
  }

  # add optional grouping variable
  if (length(terms) > 1) {
    group <-
      sjlabelled::as_label(
        mf[[terms[2]]],
        prefix = FALSE,
        drop.na = TRUE,
        drop.levels = !is.numeric(mf[[terms[2]]])
      )
  } else {
    group <- sjmisc::to_factor(1)
  }

  # return all as data.frame
  tryCatch(
    {
      data_frame(response = response, x = x, group = group)
    },
    error = function(x) { NULL },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )
}


#' @importFrom purrr map
#' @importFrom dplyr n_distinct
#' @importFrom stats na.omit
prettify_data <- function(xl.remain, fitfram, terms, use.all = FALSE) {
  purrr::map(xl.remain, function(.x) {
    pr <- fitfram[[terms[.x]]]
    if (is.numeric(pr)) {
      if (.x > 1 && dplyr::n_distinct(pr, na.rm = TRUE) >= 10)
        rprs_values(pr)
      else if (dplyr::n_distinct(pr, na.rm = TRUE) < 20 || isTRUE(use.all))
        sort(stats::na.omit(unique(pr)))
      else
        pretty_range(pr)
    } else if (is.factor(pr))
      levels(droplevels(pr))
    else
      stats::na.omit(unique(pr))
  })
}


#' @importFrom insight get_variance_random n_obs find_parameters
#' @importFrom stats deviance
#' @keywords internal
getVarRand <- function(x) {
  tryCatch(
    {
      if (inherits(x, c("merMod", "rlmerMod", "lmerMod", "glmerMod", "glmmTMB", "stanreg", "MixMod"))) {
        re.var <- insight::get_variance_random(x)
      } else if (inherits(x, c("lme", "nlme"))) {
        re.var <- x$sigma^2
      } else {
        re.var <- stats::deviance(x) / (insight::n_obs(x) - length(insight::find_parameters(x)[["conditional"]]))
      }
      re.var
    },
    error = function(x) { 0 }
  )
}


#' @keywords internal
.has_splines <- function(model) {
  form <- .get_pasted_formula(model)
  if (is.null(form)) return(FALSE)

  any(
    grepl("s\\(([^,)]*)", form) | grepl("bs\\(([^,)]*)", form) |
      grepl("ns\\(([^,)]*)", form) | grepl("pspline\\(([^,)]*)", form) |
      grepl("poly\\(([^,)]*)", form)
  )
}


#' @keywords internal
.has_poly <- function(model) {
  form <- .get_pasted_formula(model)
  if (is.null(form)) return(FALSE)
  any(grepl("I\\(.*?\\^.*?\\)", form) | grepl("poly\\(([^,)]*)", form))
}


#' @keywords internal
.has_log <- function(model) {
  any(.get_log_terms(model))
}


#' @keywords internal
.get_log_terms <- function(model) {
  form <- .get_pasted_formula(model)
  if (is.null(form)) return(FALSE)
  grepl("log\\(([^,)]*).*", form)
}


#' @importFrom insight find_variables
#' @keywords internal
.get_pasted_formula <- function(model) {
  tryCatch(
    {
      unlist(compact_list(insight::find_variables(model)[c("conditional", "random", "instruments")]))
    },
    error = function(x) { NULL }
  )
}


#' @keywords internal
.has_poly_term <- function(x) {
  any(grepl("poly\\(([^,)]*)", x))
}


#' @keywords internal
.uses_all_tag <- function(terms) {
  tags <- unlist(regmatches(
    terms,
    gregexpr(
      pattern = "\\[(.*)\\]",
      text = terms,
      perl = T
    )
  ))

  "[all]" %in% tags
}


#' @keywords internal
frac_length <- function(x) {
  if (is.numeric(x)) {
    max(nchar(gsub(pattern = "(.\\.)(.*)", "\\2", sprintf("%f", abs(x) %% 1))))
  } else
    0
}


is.whole <- function(x) {
  (is.numeric(x) && all(floor(x) == x, na.rm = T)) || is.character(x) || is.factor(x)
}


#' @keywords internal
.get_poly_term <- function(x) {
  p <- "(.*)poly\\(([^,]*)[^)]*\\)(.*)"
  sub(p, "\\2", x)
}


#' @keywords internal
.get_poly_degree <- function(x) {
  p <- "(.*)poly\\(([^,]*)([^)])*\\)(.*)"
  tryCatch(
    {
      as.numeric(sub(p, "\\3", x))
    },
    error = function(x) { 1 }
  )
}


#' @importFrom stats formula
is_brms_trial <- function(model) {
  is.trial <- FALSE

  if (inherits(model, "brmsfit") && is.null(stats::formula(model)$responses)) {
    is.trial <- tryCatch({
      rv <- deparse(stats::formula(model)$formula[[2L]], width.cutoff = 500L)
      sjmisc::trim(sub("(.*)\\|(.*)\\(([^,)]*).*", "\\2", rv)) %in% c("trials", "resp_trials")
    },
    error = function(x) {
      FALSE
    }
    )
  }

  is.trial
}


get_model_info <- function(model) {
  faminfo <- insight::model_info(model)
  if (insight::is_multivariate(model)) faminfo <- faminfo[[1]]
  faminfo$is_brms_trial <- is_brms_trial(model)
  faminfo
}


compact_list <- function(x) x[!sapply(x, function(i) length(i) == 0 || is.null(i) || any(i == "NULL"))]
