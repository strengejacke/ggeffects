# test_predictions helper ------------------------


.call_me <- function(fun, fun_args, dot_args, include_random) {
  # concatenate all arguments
  all_args <- .compact_list(c(fun_args, dot_args))
  # since ".compact_list" removes NULL objects, we add it back for mixed models
  if (include_random) {
    all_args$re.form <- NULL
    # avoid message
    out <- suppressMessages(suppressWarnings(do.call(get(fun, asNamespace("marginaleffects")), all_args)))
  } else {
    out <- do.call(get(fun, asNamespace("marginaleffects")), all_args)
  }
  # clean column names
  colnames(out)[tolower(colnames(out)) == "hypothesis"] <- "term"
  # return
  out
}


.is_custom_hypothesis <- function(x) {
  !is.null(x) &&
    length(x) == 1 &&
    is.character(x) &&
    grepl("=", x, fixed = TRUE) &&
    grepl("\\bb\\d+\\b", x)
}


.validate_by_argument <- function(by, datagrid) {
  # check for valid by-variable
  if (!is.null(by)) {
    # all by-terms need to be in data grid
    if (!all(by %in% colnames(datagrid))) {
      insight::format_error(
        paste0("Variable(s) `", toString(by[!by %in% colnames(datagrid)]), "` not found in data grid.")
      )
    }
    # by-terms must be categorical
    by_factors <- vapply(datagrid[by], is.factor, TRUE)
    if (!all(by_factors)) {
      insight::format_error(
        "All variables in `by` must be categorical.",
        paste0(
          "The following variables in `by` are not categorical: ",
          toString(paste0("`", by[!by_factors], "`"))
        )
      )
    }
  }
  by
}


.collapse_levels <- function(out, datagrid, focal, by) {
  # remove by-terms from focal terms
  if (!is.null(by)) {
    focal <- focal[!focal %in% by]
  }
  # iterate all focal terms, these are the column names in "out"
  for (i in focal) {
    flag_dash <- FALSE
    # for factors, we need to check whether factor levels contain "-"
    # if so, we need to replace it, else "strplit()" won't work"
    if (is.factor(datagrid[[i]])) {
      l <- levels(datagrid[[i]])
      dash_levels <- grepl("-", l, fixed = TRUE)
      if (any(dash_levels)) {
        for (j in l[dash_levels]) {
          # replace by a - hopefully - unique character, later revert
          out[[i]] <- gsub(j, gsub("-", "#~#", j, fixed = TRUE), out[[i]], fixed = TRUE)
          flag_dash <- TRUE
        }
      }
    }
    level_pairs <- strsplit(out[[i]], "-", fixed = TRUE)
    all_same <- vapply(level_pairs, function(j) {
      all(j == j[1])
    }, TRUE)
    if (any(all_same)) {
      out[[i]][all_same] <- vapply(level_pairs[all_same], unique, character(1))
    }
    # revert replacement
    if (flag_dash) {
      out[[i]] <- gsub("#~#", "-", out[[i]], fixed = TRUE)
      flag_dash <- FALSE
    }
  }
  out
}


.fix_comma_levels <- function(terms, datagrid, focal) {
  for (i in focal) {
    if (is.factor(datagrid[[i]])) {
      l <- levels(datagrid[[i]])
      comma_levels <- grepl(",", l, fixed = TRUE)
      if (any(comma_levels)) {
        for (j in l[comma_levels]) {
          # replace by a - hopefully - unique character, later revert
          terms <- gsub(j, gsub(",", "#*#", j, fixed = TRUE), terms, fixed = TRUE)
        }
      }
    }
  }
  terms
}


.extract_labels <- function(full_comparisons, focal, test, old_labels) {
  # now we have both names of predictors and their levels
  beta_rows <- full_comparisons[focal]
  beta_rows[] <- lapply(beta_rows, as.character)
  # extract coefficient numbers from "test" string, which are
  # equivalent to row numbers
  pos <- gregexpr("(b[0-9]+)", test)[[1]]
  len <- attributes(pos)$match.length
  row_number <- unlist(lapply(seq_along(pos), function(i) {
    substring(test, pos[i] + 1, pos[i] + len[i] - 1)
  }))
  # sort rownumbers, largest first. Else, we may have "b1" and "b13", and
  # if we replace "b1" by a label "foo", "b13" is also replaced and becomes
  # "foo3" (see #312)
  row_number <- row_number[order(as.numeric(row_number), decreasing = TRUE)]
  # loop through rows, and replace "b<d>" with related string
  for (i in row_number) {
    label <- paste0(
      colnames(beta_rows),
      paste0("[", as.vector(unlist(beta_rows[i, ], use.names = FALSE)), "]"),
      collapse = ","
    )
    old_labels <- gsub(paste0("b", i), label, old_labels, fixed = TRUE)
  }
  # remove whitespace around operators, but not inside brackets
  tokens <- c("=", "-", "\\+", "/", "\\*")
  replacements <- c("=", "-", "+", "/", "*")
  for (i in seq_along(tokens)) {
    pattern <- paste0(tokens[i], "(?![^\\[]*\\])")
    old_labels <- gsub(pattern, paste0(" ", replacements[i], " "), old_labels, perl = TRUE)
  }

  old_labels
}


.get_zi_prediction_type <- function(model, type) {
  # sanity check - for pooled predictions, we cannot retrieve the model
  if (!insight::is_model(model)) {
    return("response")
  }
  if (inherits(model, "glmmTMB")) {
    types <- c("conditional", "zprob")
  } else {
    types <- c("count", "zero")
  }
  switch(type,
    conditional = ,
    count = ,
    fixed = types[1],
    zi_prob = ,
    zero = ,
    zprob = types[2],
    "response"
  )
}


.scale_label <- function(minfo, scale) {
  scale_label <- NULL
  if (minfo$is_binomial || minfo$is_ordinal || minfo$is_multinomial) {
    scale_label <- switch(scale,
      response = "probabilities",
      link = "log-odds",
      oddsratios = "odds ratios",
      probs = ,
      probability = "probabilities",
      NULL
    )
  } else if (minfo$is_count) {
    scale_label <- switch(scale,
      response = "counts",
      link = "log-mean",
      irr = "incident rate ratios",
      count = ,
      conditional = "conditional means",
      zero = ,
      zprob = ,
      zi_prob = ,
      probs = ,
      probability = "probabilities",
      NULL
    )
  } else if (minfo$is_orderedbeta) {
    scale_label <- switch(scale,
      response = "proportions",
      link = "log-proportions",
      probs = ,
      probability = "probabilities",
      NULL
    )
  }
  scale_label
}


# p-value adjustment -------------------

.p_adjust <- function(params, p_adjust, datagrid, focal, statistic = NULL, df = Inf, verbose = TRUE) {
  # exit on NULL, or if no p-adjustment requested
  if (is.null(p_adjust) || identical(p_adjust, "none")) {
    return(params)
  }

  all_methods <- c(tolower(stats::p.adjust.methods), "tukey", "sidak")

  # needed for rank adjustment
  focal_terms <- datagrid[focal]
  rank_adjust <- prod(vapply(focal_terms, insight::n_unique, numeric(1)))

  # only proceed if valid argument-value
  if (tolower(p_adjust) %in% all_methods) {
    if (tolower(p_adjust) %in% tolower(stats::p.adjust.methods)) {
      # base R adjustments
      params$p.value <- stats::p.adjust(params$p.value, method = p_adjust)
    } else if (tolower(p_adjust) == "tukey") {
      if (!is.null(statistic)) {
        # tukey adjustment
        params$p.value <- suppressWarnings(stats::ptukey(
          sqrt(2) * abs(statistic),
          rank_adjust,
          df,
          lower.tail = FALSE
        ))
        # for specific contrasts, ptukey might fail, and the tukey-adjustement
        # could just be simple p-value calculation
        if (all(is.na(params$p.value))) {
          params$p.value <- 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE)
        }
      } else if (verbose) {
        insight::format_alert("No test-statistic found. P-values were not adjusted.")
      }
    } else if (tolower(p_adjust) == "sidak") {
      # sidak adjustment
      params$p.value <- 1 - (1 - params$p.value)^rank_adjust
    }
  } else if (verbose) {
    insight::format_alert(paste0("`p_adjust` must be one of ", toString(all_methods)))
  }
  params
}
