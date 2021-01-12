data_frame <- function(...) {
  x <- data.frame(..., stringsAsFactors = FALSE)
  rownames(x) <- NULL
  x
}




.check_vars <- function(terms, model) {
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
        clean.terms <- .clean_terms(terms)
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



#' @importFrom insight clean_names print_color
.offset_term <- function(model, verbose = TRUE) {
  tryCatch({
    off <- .safe_deparse(model$call$offset)
    if (identical(off, "NULL")) {
      return(NULL)
    }
    cleaned_off <- insight::clean_names(off)
    if (!identical(off, cleaned_off) && isTRUE(verbose)) {
      insight::print_color(sprintf("Model uses a transformed offset term. Predictions may not be correct. Please apply transformation of offset term to the data before fitting the model and use 'offset=%s' in the model formula.\n", cleaned_off), "red")
    }
    cleaned_off
  },
  error = function(e) {
    NULL
  })
}



#' @importFrom stats complete.cases
#' @importFrom sjlabelled as_label as_numeric
.get_raw_data <- function(model, mf, terms) {
  # for matrix variables, don't return raw data
  if (any(sapply(mf, is.matrix)) && !inherits(model, c("coxph", "coxme")))
    return(NULL)

  if (!all(insight::find_response(model, combine = FALSE) %in% colnames(mf)))
    return(NULL)

  # get response and x-value
  response <- insight::get_response(model)
  x <- sjlabelled::as_numeric(mf[[terms[1]]])

  # for cox-models, modify response
  if (inherits(model, "coxph")) {
    response <- response[[2]]
  }

  # back-transform log-transformed response?
  rv <- insight::find_terms(model)[["response"]]

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
    group <- as.factor(1)
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


#' @importFrom stats na.omit
.prettify_data <- function(conditional_terms, original_model_frame, terms, use_all_values = FALSE, show_pretty_message = FALSE) {
  lapply(conditional_terms, function(.x) {
    pr <- original_model_frame[[terms[.x]]]
    if (is.numeric(pr)) {
      if (.x > 1 && .n_distinct(pr) >= 10)
        values_at(pr)
      else if (.n_distinct(pr) < 20 || isTRUE(use_all_values)) {
        sort(stats::na.omit(unique(pr)))
      } else {
        if (show_pretty_message) {
          message(sprintf("Data were 'prettified'. Consider using `terms=\"%s [all]\"` to get smooth plots.", terms[.x]))
          show_pretty_message <- FALSE
        }
        pretty_range(pr)
      }
    } else if (is.factor(pr))
      levels(droplevels(pr))
    else
      stats::na.omit(unique(pr))
  })
}


#' @importFrom insight get_variance_random get_sigma model_info
.get_residual_variance <- function(x) {
  tryCatch(
    {
      info <- insight::model_info(x)
      if (info$is_mixed || inherits(x, c("merMod", "rlmerMod", "lmerMod", "glmerMod", "glmmTMB", "stanreg", "MixMod"))) {
        re.var <- insight::get_variance_random(x)
      } else if (inherits(x, c("lme", "nlme"))) {
        re.var <- x$sigma^2
      } else {
        re.var <- insight::get_sigma(x)
        if (is.null(re.var)) {
          re.var <- 0
        }
      }
      re.var
    },
    error = function(x) { 0 }
  )
}



.frac_length <- function(x) {
  if (is.numeric(x)) {
    max(nchar(gsub(pattern = "(.\\.)(.*)", "\\2", sprintf("%f", abs(x) %% 1))))
  } else
    0
}



is.whole <- function(x) {
  (is.numeric(x) && all(floor(x) == x, na.rm = T)) || is.character(x) || is.factor(x)
}



.get_poly_term <- function(x) {
  p <- "(.*)poly\\(([^,]*)[^)]*\\)(.*)"
  sub(p, "\\2", x)
}



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
      rv <- .safe_deparse(stats::formula(model)$formula[[2L]])
      trimws(sub("(.*)\\|(.*)\\(([^,)]*).*", "\\2", rv)) %in% c("trials", "resp_trials")
    },
    error = function(x) {
      FALSE
    }
    )
  }

  is.trial
}



.get_model_info <- function(model) {
  faminfo <- insight::model_info(model)
  if (insight::is_multivariate(model)) faminfo <- faminfo[[1]]
  faminfo$is_brms_trial <- is_brms_trial(model)
  faminfo
}


#' @importFrom stats complete.cases
.compact_list <- function(x) {
  if (is.data.frame(x)) {
    x <- x[stats::complete.cases(x), ]
  }
  x[!sapply(x, function(i) length(i) == 0 || is.null(i) || any(i == "NULL"))]
}


.safe_deparse <- function(string) {
  paste0(sapply(deparse(string, width.cutoff = 500), trimws, simplify = TRUE), collapse = " ")
}



is.gamm <- function(x) {
  inherits(x, c("list", "gamm")) && all(names(x) %in% c("lme", "gam"))
}


is.gamm4 <- function(x) {
  inherits(x, "list") && all(names(x) %in% c("mer", "gam"))
}



.n_distinct <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  length(unique(x))
}



# select rows where values in "variable" match "value"
.select_rows <- function(data, variable, value) {
  data[which(data[[variable]] == value), , drop = FALSE]
}

# remove column
.remove_column <- function(data, variables) {
  a <- attributes(data)
  if (!length(variables) || is.null(variables)) return(data)
  if (is.numeric(variables)) variables <- colnames(data)[variables]
  data <- data[, -which(colnames(data) %in% variables), drop = FALSE]
  remaining <- setdiff(names(a), names(attributes(data)))
  if (length(remaining)) attributes(data) <- c(attributes(data), a[remaining])
  data
}


.convert_numeric_factors <- function(x) {
  num_facs <- sapply(x, .is_numeric_factor)
  if (any(num_facs)) {
    x[num_facs] <- lapply(x[num_facs], function(i) as.numeric(as.character(i)))
  }
  x
}



.is_numeric_factor <- function(x) {
  is.factor(x) && !anyNA(suppressWarnings(as.numeric(levels(x))))
}
