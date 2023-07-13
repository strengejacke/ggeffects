.data_frame <- function(...) {
  x <- data.frame(..., stringsAsFactors = FALSE)
  rownames(x) <- NULL
  x
}


.check_vars <- function(terms, model) {
  if (missing(terms) || is.null(terms)) {
    insight::format_error("`terms` needs to be a character vector with at least one predictor name: one term used for the x-axis, more optional terms as grouping factors.")
  }

  # do we have a list? Just use names then
  if (is.list(terms)) {
    terms <- names(terms)
  }

  # check for correct length of vector
  if (length(terms) > 4) {
    insight::format_alert("`terms` must have not more than four values. Using first four values now.")
    terms <- terms[1:4]
  }

  out_msg <- NULL
  if (!is.null(model)) {
    msg <- tryCatch(
      {
        pv <- insight::find_predictors(model, effects = "all", component = "all", flatten = TRUE)
        clean.terms <- .clean_terms(terms)
        if (!all(clean.terms %in% pv)) {
          out_msg <- c(
            "Some of the specified `terms` were not found in the model.",
            .misspelled_string(pv, clean.terms, "Maybe misspelled?")
          )
        }
        out_msg
      },
      error = function(x) NULL
    )
  }

  if (!is.null(out_msg)) {
    insight::format_error(out_msg)
  }

  terms
}


.offset_term <- function(model, verbose = TRUE) {
  tryCatch({
    off <- insight::safe_deparse(model$call$offset)
    if (identical(off, "NULL")) {
      return(NULL)
    }
    cleaned_off <- insight::clean_names(off)
    if (!identical(off, cleaned_off) && isTRUE(verbose) && !inherits(model, "glmmTMB")) {
      insight::format_warning(sprintf("Model uses a transformed offset term. Predictions may not be correct. Please apply transformation of offset term to the data before fitting the model and use 'offset(%s)' in the model formula.", cleaned_off))
    }
    cleaned_off
  },
  error = function(e) {
    NULL
  })
}


.get_raw_data <- function(model, mf, terms) {
  # for matrix variables, don't return raw data
  if (any(vapply(mf, is.matrix, TRUE)) && !inherits(model, c("coxph", "coxme")))
    return(NULL)

  if (!all(insight::find_response(model, combine = FALSE) %in% colnames(mf)))
    return(NULL)

  # get response and x-value
  response <- insight::get_response(model)

  # for cox-models, modify response
  if (inherits(model, "coxph")) {
    response <- response[[2]]
  }

  # back-transform log-transformed response?
  rv <- insight::find_terms(model)[["response"]]

  # character vectors to factors
  for (i in terms) {
    if (is.character(mf[[i]])) {
      mf[[i]] <- factor(mf[[i]], levels = unique(mf[[i]]))
    }
  }
  x <- .factor_to_numeric(mf[[terms[1]]])


  # add optional grouping variable
  if (length(terms) > 1) {
    group <-
      .as_label(
        mf[[terms[2]]],
        prefix = FALSE,
        drop.na = TRUE,
        drop.levels = !is.numeric(mf[[terms[2]]])
      )
  } else {
    group <- as.factor(1)
  }

  if (length(terms) > 2) {
    facet <-
      .as_label(
        mf[[terms[3]]],
        prefix = FALSE,
        drop.na = TRUE,
        drop.levels = !is.numeric(mf[[terms[3]]])
      )
  } else {
    facet <- as.factor(1)
  }

  # return all as data.frame
  tryCatch(
    {
      .data_frame(response = response, x = x, group = group, facet = facet)
    },
    error = function(x) NULL,
    warning = function(x) NULL,
    finally = function(x) NULL
  )
}


.prettify_data <- function(conditional_terms, original_model_frame, terms,
                           use_all_values = FALSE, show_pretty_message = FALSE) {
  lapply(conditional_terms, function(.x) {
    pr <- original_model_frame[[terms[.x]]]
    if (is.numeric(pr)) {
      if (.x > 1 && .n_distinct(pr) >= 10) {
        values_at(pr)
      } else if (.n_distinct(pr) < 20 || isTRUE(use_all_values)) {
        sort(stats::na.omit(unique(pr)))
      } else {
        if (show_pretty_message) {
          insight::format_alert(sprintf(
            "Data were 'prettified'. Consider using `terms=\"%s [all]\"` to get smooth plots.", terms[.x]
          ))
          show_pretty_message <- FALSE
        }
        pretty_range(pr)
      }
    } else if (is.factor(pr)) {
      levels(droplevels(pr))
    } else {
      stats::na.omit(unique(pr))
    }
  })
}


.get_residual_variance <- function(x) {
  out <- .safe(insight::get_sigma(x, ci = NULL, verbose = FALSE)^2, 0)
  if (!length(out)) {
    return(0)
  }
  out
}


.frac_length <- function(x) {
  if (is.numeric(x)) {
    max(nchar(gsub(pattern = "(.\\.)(.*)", "\\2", sprintf("%f", abs(x) %% 1))))
  } else {
    0
  }
}


is.whole <- function(x) {
  (is.numeric(x) && isTRUE(all.equal(x, round(x)))) || is.character(x) || is.factor(x)
}


is.whole.number <- function(x) {
  (is.numeric(x) && isTRUE(all.equal(x, round(x))))
}


.get_poly_term <- function(x) {
  p <- "(.*)poly\\(([^,]*)[^)]*\\)(.*)"
  sub(p, "\\2", x)
}


.get_poly_degree <- function(x) {
  p <- "(.*)poly\\(([^,]*)([^)])*\\)(.*)"
  tryCatch(as.numeric(sub(p, "\\3", x)), error = function(x) 1)
}


is_brms_trial <- function(model) {
  is.trial <- FALSE

  if (inherits(model, "brmsfit") && is.null(stats::formula(model)$responses)) {
    is.trial <- tryCatch({
      rv <- insight::safe_deparse(stats::formula(model)$formula[[2L]])
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


.compact_list <- function(x) {
  if (is.data.frame(x)) {
    x <- x[stats::complete.cases(x), ]
  }
  x[!vapply(x, function(i) length(i) == 0 || is.null(i) || any(i == "NULL"), TRUE)]
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
  num_facs <- vapply(x, .is_numeric_factor, TRUE)
  if (any(num_facs)) {
    x[num_facs] <- lapply(x[num_facs], function(i) as.numeric(as.character(i)))
  }
  x
}


.is_numeric_factor <- function(x) {
  is.factor(x) && !anyNA(suppressWarnings(as.numeric(levels(x))))
}


.factor_to_numeric <- function(x, lowest = NULL) {
  if (is.numeric(x)) {
    return(x)
  }

  if (is.logical(x)) {
    return(as.numeric(x))
  }

  if (anyNA(suppressWarnings(as.numeric(as.character(stats::na.omit(x)))))) {
    if (is.character(x)) {
      x <- as.factor(x)
    }
    x <- droplevels(x)
    levels(x) <- 1:nlevels(x)
  }

  out <- as.numeric(as.character(x))

  if (!is.null(lowest)) {
    difference <- min(out) - lowest
    out <- out - difference
  }

  out
}


.check_returned_se <- function(se.pred) {
  !is.null(se.pred) && length(se.pred) > 0 && !is.null(se.pred$se.fit) && length(se.pred$se.fit) > 0
}


.mvrnorm <- function(n = 1, mu, Sigma, tol = 1e-06) {
  p <- length(mu)
  if (!all(dim(Sigma) == c(p, p))) {
    insight::format_error("Incompatible arguments to calculate multivariate normal distribution.")
  }
  eS <- eigen(Sigma, symmetric = TRUE)
  ev <- eS$values
  if (!all(ev >= -tol * abs(ev[1L]))) {
    insight::format_error("`Sigma` is not positive definite.")
  }
  X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*%
    t(matrix(stats::rnorm(p * n), n))
  nm <- names(mu)
  dn <- dimnames(Sigma)
  if (is.null(nm) && !is.null(dn)) {
    nm <- dn[[1L]]
  }
  dimnames(X) <- list(nm, NULL)
  if (n == 1) {
    drop(X)
  } else {
    t(X)
  }
}


.safe <- function(code, on_error = NULL) {
  tryCatch(code, error = function(e) on_error)
}


.misspelled_string <- function(source, searchterm, default_message = NULL) {
  if (is.null(searchterm) || length(searchterm) < 1) {
    return(default_message)
  }
  # used for many matches
  more_found <- ""
  # init default
  msg <- ""
  # remove matching strings
  same <- intersect(source, searchterm)
  searchterm <- setdiff(searchterm, same)
  source <- setdiff(source, same)
  # guess the misspelled string
  possible_strings <- unlist(lapply(searchterm, function(s) {
    source[.fuzzy_grep(source, s)] # nolint
  }), use.names = FALSE)
  if (length(possible_strings)) {
    msg <- "Did you mean "
    if (length(possible_strings) > 1) {
      # make sure we don't print dozens of alternatives for larger data frames
      if (length(possible_strings) > 5) {
        more_found <- sprintf(
          " We even found %i more possible matches, not shown here.",
          length(possible_strings) - 5
        )
        possible_strings <- possible_strings[1:5]
      }
      msg <- paste0(msg, "one of ", toString(paste0("\"", possible_strings, "\"")))
    } else {
      msg <- paste0(msg, "\"", possible_strings, "\"")
    }
    msg <- paste0(msg, "?", more_found)
  } else {
    msg <- default_message
  }
  # no double white space
  insight::trim_ws(msg)
}


.fuzzy_grep <- function(x, pattern, precision = NULL) {
  if (is.null(precision)) {
    precision <- round(nchar(pattern) / 3)
  }
  if (precision > nchar(pattern)) {
    return(NULL)
  }
  p <- sprintf("(%s){~%i}", pattern, precision)
  grep(pattern = p, x = x, ignore.case = FALSE)
}


.dynEval <- function(x,
                     ifnotfound = NULL,
                     minframe = 1L,
                     inherits = FALSE,
                     remove_n_top_env = 0) {
  n <- sys.nframe() - remove_n_top_env
  x <- insight::safe_deparse(x)
  while (n > minframe) {
    n <- n - 1L
    env <- sys.frame(n)
    r <- try(eval(str2lang(x), envir = env), silent = TRUE)
    if(!inherits(r, "try-error") && !is.null(r)) {
      return(r)
    }
  }
  ifnotfound
}
