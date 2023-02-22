#' @title (Pairwise) comparisons between predictions
#' @name hypothesis_test
#'
#' @description Function to test differences of adjusted predictions for
#'   statistical significance. This is usually called contrasts or (pairwise)
#'   comparisons.
#'
#' @param model A fitted model object, or an object of class `ggeffects`.
#' @param test Hypothesis to test. By default, pairwise-comparisons are
#'   conducted. See section _Introduction into contrasts and pairwise comparisons_.
#' @param terms Character vector with the names of the focal terms from `model`,
#'   for which contrasts or comparisons should be displayed. At least one term
#'   is required, maximum length is three terms. If the first focal term is numeric,
#'   contrasts or comparisons for the *slopes* of this numeric predictor are
#'   computed (possibly grouped by the levels of further categorical focal
#'   predictors).
#' @param p_adjust Character vector, if not `NULL`, indicates the method to
#'   adjust p-values. See [`stats::p.adjust()`] for details. Further possible
#'   adjustment methods are `"tukey"` or `"sidak"`. Some caution is necessary
#'   when adjusting p-value for multiple comparisons. See also section
#'   _P-value adjustment_ below.
#' @param df Degrees of freedom that will be used to compute the p-values and
#'   confidence intervals from the test statistic. If `NULL`, degrees of freedom
#'   will be extracted from the model using [`insight::get_df()`] with `type = "wald"`.
#' @param verbose Toggle messages and warnings.
#' @param ... Arguments passed down to [`data_grid()`] when creating the reference
#'   grid.
#'
#' @section Introduction into contrasts and pairwise comparisons:
#'
#' There are many ways to test contrasts or pairwise comparisons. A
#' detailed introduction with many (visual) examples is shown in
#' [this vignette](https://strengejacke.github.io/ggeffects/articles/introduction_comparisons.html).
#'
#' @section P-value adjustment for multiple comparisons:
#'
#' Note that p-value adjustment for methods supported by `p.adjust()` (see also
#' `p.adjust.methods`), each row is considered as one set of comparisons, no
#' matter which `test` was specified. That is, for instance, when `hypothesis_test()`
#' returns eight rows of predictions (when `test = NULL`), and `p_adjust = "bonferroni"`,
#' the p-values are adjusted in the same way as if we had a test of pairwise
#' comparisons (`test = "pairwise"`) where eight rows of comparisons are
#' returned. For methods `"tukey"` or `"sidak"`, a rank adjustment is done
#' based on the number of combinations of levels from the focal predictors
#' in `terms`. Thus, the latter two methods may be useful for certain tests
#' only, in particular pairwise comparisons.
#'
#' @return A data frame containing predictions (e.g. for `test = NULL`),
#' contrasts or pairwise comparisons of adjusted predictions or estimated
#' marginal means.
#'
#' @examples
#' if (requireNamespace("marginaleffects")) {
#'   data(efc)
#'   efc$c172code <- as.factor(efc$c172code)
#'   efc$c161sex <- as.factor(efc$c161sex)
#'   levels(efc$c161sex) <- c("male", "female")
#'   m <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
#'
#'   # direct computation of comparisons
#'   hypothesis_test(m, "c172code")
#'
#'   # passing a `ggeffects` object
#'   pred <- ggpredict(m, "c172code")
#'   hypothesis_test(pred)
#'
#'   # test for slope
#'   hypothesis_test(m, "c12hour")
#'
#'   # interaction - contrasts by groups
#'   m <- lm(barthtot ~ c12hour + c161sex * c172code + neg_c_7, data = efc)
#'   hypothesis_test(m, c("c161sex", "c172code"), test = NULL)
#'
#'   # interaction - pairwise comparisons by groups
#'   hypothesis_test(m, c("c161sex", "c172code"))
#'
#'   # p-value adjustment
#'   hypothesis_test(m, c("c161sex", "c172code"), p_adjust = "tukey")
#'
#'   # specific comparisons
#'   hypothesis_test(m, c("c161sex", "c172code"), test = "b2 = b1")
#'
#'   # interaction - slope by groups
#'   m <- lm(barthtot ~ c12hour + neg_c_7 * c172code + c161sex, data = efc)
#'   hypothesis_test(m, c("neg_c_7", "c172code"))
#' }
#' @export
hypothesis_test <- function(model, ...) {
  UseMethod("hypothesis_test")
}

#' @rdname hypothesis_test
#' @export
hypothesis_test.default <- function(model,
                                    terms = NULL,
                                    test = "pairwise",
                                    p_adjust = NULL,
                                    df = NULL,
                                    verbose = TRUE,
                                    ...) {
  insight::check_if_installed("marginaleffects")

  # only model objects are supported...
  if (!insight::is_model_supported(model)) {
    insight::format_error(
      paste0("Objects of class `", class(model)[1], "` are not yet supported.")
    )
  }

  # for mixed models, we need different handling later...
  need_average_predictions <- insight::is_mixed_model(model)

  # we want contrasts or comparisons for these focal predictors...
  focal <- .clean_terms(terms)

  # check if we have a mixed model - in this case, we need to ensure that our
  # random effect variable (group factor) is included in the grid
  if (need_average_predictions) {
    random_group <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)
    if (!all(random_group %in% terms)) {
      terms <- unique(c(terms, random_group))
    }
  }
  grid <- data_grid(model, terms, ...)

  # comparisons only make sense if we have at least two predictors, or if
  # we have one categorical
  focal_numeric <- vapply(grid[focal], is.numeric, TRUE)
  focal_other <- !focal_numeric
  hypothesis_label <- NULL

  # extract degrees of freedom
  if (is.null(df)) {
    df <- .get_df(model)
  }

  # if *first* focal predictor is numeric, compute average slopes
  if (isTRUE(focal_numeric[1])) {

    # testing slopes =====

    # just the "trend" (slope) of one focal predictor
    if (length(focal) == 1) {
      # argument "test" will be ignored for average slopes
      test <- NULL
      .comparisons <- marginaleffects::avg_slopes(model, variables = focal, df = df)
      out <- data.frame(x_ = "slope", stringsAsFactors = FALSE)
      colnames(out) <- focal

    } else {
      # "trends" (slopes) of numeric focal predictor by group levels
      # of other focal predictor
      .comparisons <- marginaleffects::slopes(
        model,
        variables = focal[1],
        by = focal[2:length(focal)],
        newdata = grid,
        hypothesis = test,
        df = df
      )

      # for pairwise comparisons, we need to extract contrasts
      if (!is.null(test) && all(test == "pairwise")) {

        ## pairwise comparisons of slopes -----

        # if we find a comma in the terms column, we have two categorical predictors
        if (any(grepl(",", .comparisons$term, fixed = TRUE))) {
          contrast_terms <- data.frame(
            do.call(rbind, strsplit(.comparisons$term, " - ", fixed = TRUE)),
            stringsAsFactors = FALSE
          )

          # split and recombine term names
          pairs1 <- unlist(strsplit(contrast_terms[[1]], ",", fixed = TRUE))
          pairs2 <- unlist(strsplit(contrast_terms[[2]], ",", fixed = TRUE))
          contrast_pairs <- paste0(
            insight::trim_ws(pairs1),
            "-",
            insight::trim_ws(pairs2)
          )

          # create data frame
          out <- data.frame(
            x_ = "slope",
            x__ = contrast_pairs[c(TRUE, FALSE)],
            x___ = contrast_pairs[c(FALSE, TRUE)],
            stringsAsFactors = FALSE
          )
        } else {

          out <- data.frame(
            x_ = "slope",
            x__ = gsub(" ", "", .comparisons$term, fixed = TRUE),
            stringsAsFactors = FALSE
          )
        }
        colnames(out) <- focal

      } else if (is.null(test)) {

        ## contrasts of slopes -----

        # if we have simple slopes without pairwise comparisons, we can
        # copy the information directly from the marginaleffects-object
        grid_categorical <- as.data.frame(
          .comparisons[focal[2:length(focal)]],
          stringsAsFactors = FALSE
        )
        out <- cbind(data.frame(x_ = "slope", stringsAsFactors = FALSE), grid_categorical)
        colnames(out) <- focal

      } else {

        ## hypothesis testing of slopes -----

        # if we have specific comparisons of estimates, like "b1 = b2", we
        # want to replace these shortcuts with the full related predictor names
        # and levels
        if (any(grepl("b[0-9]+", .comparisons$term))) {
          # re-compute comoparisons for all combinations, so we know which
          # estimate refers to which combination of predictor levels
          .full_comparisons <- marginaleffects::slopes(
            model,
            variables = focal[1],
            by = focal[2:length(focal)],
            hypothesis = NULL,
            df = df
          )
          # replace "hypothesis" labels with names/levels of focal predictors
          hypothesis_label <- .extract_labels(
            full_comparisons = .full_comparisons,
            focal = focal[2:length(focal)],
            test = test,
            old_labels = .comparisons$term
          )
        }
        # we have a specific hypothesis, like "b3 = b4". We just copy that information
        out <- data.frame(Hypothesis = .comparisons$term, stringsAsFactors = FALSE)
      }
    }
    estimate_name <- ifelse(is.null(test), "Slope", "Contrast")

  } else {

    # testing groups (factors) ======

    if (need_average_predictions) {
      .comparisons <- marginaleffects::avg_predictions(
        model,
        variables = sapply(focal, function(i) unique(grid[[i]])),
        newdata = grid,
        hypothesis = test,
        df = df
      )
    } else {
      .comparisons <- marginaleffects::predictions(
        model,
        newdata = grid,
        hypothesis = test,
        df = df
      )
    }

    # pairwise comparisons - we now extract the group levels from the "term"
    # column and create separate columns for contrats of focal predictors
    if (!is.null(test) && all(test == "pairwise")) {

      ## pairwise comparisons of group levels -----

      # for "predictions()", term name is "Row 1 - Row 2" etc. For
      # "avg_predictions()", we have "level_a1, level_b1 - level_a2, level_b1"
      # etc. we first want to have a data frame, where each column is one
      # combination of levels, so we split at "," and/or "-".
      contrast_terms <- data.frame(
        do.call(rbind, strsplit(.comparisons$term, "(,|-)")),
        stringsAsFactors = FALSE
      )
      contrast_terms[] <- lapply(contrast_terms, function(i) {
        insight::trim_ws(gsub("Row", "", i, fixed = TRUE))
      })

      if (need_average_predictions) {
        # for "avg_predictions()", we already have the correct labels of factor
        # levels, we just need to re-arrange, so that each column represents a
        # pairwise combination of factor levels for each factor
        out <- as.data.frame(lapply(seq_along(focal), function(i) {
          tmp <- contrast_terms[, seq(i, ncol(contrast_terms), by = length(focal))]
          unlist(lapply(seq_len(nrow(tmp)), function(j) {
            .contrasts <- as.character(unlist(tmp[j, ]))
            .contrasts_string <- paste(.contrasts, collapse = "-")
          }))
        }), stringsAsFactors = FALSE)
      } else {
        # for "predictions()", we now have the row numbers. We can than extract
        # the factor levels from the data of the data grid, as row numbers in 
        # "contrast_terms" correspond to rows in "grid".
        out <- as.data.frame(lapply(focal, function(i) {
          unlist(lapply(seq_len(nrow(contrast_terms)), function(j) {
            .contrasts <- grid[[i]][as.numeric(unlist(contrast_terms[j, ]))]
            .contrasts_string <- paste(.contrasts, collapse = "-")
          }))
        }), stringsAsFactors = FALSE)
      }
      # the final result is a data frame with one column per focal predictor,
      # and the pairwise combinations of factor levels are the values
      colnames(out) <- focal

    } else if (is.null(test)) {

      ## contrasts of group levels -----

      # we have simple contrasts - we can just copy from the data frame
      # returned by "marginaleffects"
      out <- as.data.frame(.comparisons[focal], stringsAsFactors = FALSE)

    } else {

      ## hypothesis testing of group levels -----

      # if we have specific comparisons of estimates, like "b1 = b2", we
      # want to replace these shortcuts with the full related predictor names
      # and levels
      if (any(grepl("b[0-9]+", .comparisons$term))) {
        # re-compute comoparisons for all combinations, so we know which
        # estimate refers to which combination of predictor levels
        if (need_average_predictions) {
          .full_comparisons <- marginaleffects::avg_predictions(
            model,
            variables = sapply(focal, function(i) unique(grid[[i]])),
            newdata = grid,
            hypothesis = NULL,
            df = df
          )
        } else {
          .full_comparisons <- marginaleffects::predictions(
            model,
            newdata = grid,
            hypothesis = NULL,
            df = df
          )
        }
        # replace "hypothesis" labels with names/levels of focal predictors
        hypothesis_label <- .extract_labels(
          full_comparisons = .full_comparisons,
          focal = focal,
          test = test,
          old_labels = .comparisons$term
        )
      }
      # we have a specific hypothesis, like "b3 = b4". We just copy that information
      out <- data.frame(Hypothesis = .comparisons$term, stringsAsFactors = FALSE)
    }
    estimate_name <- ifelse(is.null(test), "Predicted", "Contrast")
  }

  # yield message for non-Gaussian models
  if (identical(estimate_name, "Contrast") &&
        verbose &&
        identical(attributes(.comparisons)$type, "link") &&
        !insight::model_info(model)$is_linear) {
    insight::format_alert("Contrasts are presented on the link-scale.")
  }

  # further results
  out[[estimate_name]] <- .comparisons$estimate
  out$conf.low <- .comparisons$conf.low
  out$conf.high <- .comparisons$conf.high
  out$p.value <- .comparisons$p.value

  # p-value adjustment?
  if (!is.null(p_adjust)) {
    out <- .p_adjust(out, p_adjust, .comparisons$statistic, grid, focal, df, verbose)
  }

  class(out) <- c("ggcomparisons", "data.frame")
  attr(out, "ci") <- 0.95
  attr(out, "test") <- test
  attr(out, "hypothesis_label") <- hypothesis_label
  out
}


#' @rdname hypothesis_test
#' @export
hypothesis_test.ggeffects <- function(model, test = "pairwise", p_adjust = NULL, df = NULL, verbose = TRUE, ...) {
  # retrieve focal predictors
  focal <- attributes(model)$original.terms
  # retrieve relevant information and generate data grid for predictions
  model <- .get_model_object(model)
  hypothesis_test.default(model, terms = focal, test = test, p_adjust = p_adjust, df = df, verbose = verbose, ...)
}


# helper ------------------------

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
  # loop through rows, and replace "b<d>" with related string
  for (i in row_number) {
    label <- paste0(
      colnames(beta_rows),
      paste0("[", as.vector(unlist(beta_rows[i, ])), "]"),
      collapse = ","
    )
    old_labels <- gsub(paste0("b", i), label, old_labels, fixed = TRUE)
  }
  pattern <- c("=", "-", "+", "/", "*")
  for (p in pattern) {
    old_labels <- gsub(p, paste0(" ", p, " "), old_labels, fixed = TRUE)
  }
  old_labels
}


# methods ----------------------------

#' @export
format.ggcomparisons <- function(x, ...) {
  insight::format_table(insight::standardize_names(x), ...)
}

#' @export
print.ggcomparisons <- function(x, ...) {
  test_pairwise <- identical(attributes(x)$test, "pairwise")
  x <- format(x, ...)
  slopes <- vapply(x, function(i) all(i == "slope"), TRUE)
  if (any(slopes)) {
    x[slopes] <- NULL
    caption <- c(paste0("# Linear trend for ", names(slopes)[slopes]), "blue")
  } else if (test_pairwise) {
    caption <- c("# Pairwise comparisons", "blue")
  } else {
    caption <- NULL
  }
  footer <- attributes(x)$hypothesis_label
  if (!is.null(footer)) {
    footer <- insight::format_message(paste0("Tested hypothesis: ", footer))
    footer <- paste0("\n", footer, "\n")
  }
  cat(insight::export_table(x, title = caption, footer = footer, ...))
}


# p-value adjustment -------------------

.p_adjust <- function(params, p_adjust, statistic = NULL, grid, focal, df = Inf, verbose = TRUE) {
  all_methods <- c(tolower(stats::p.adjust.methods), "tukey", "sidak")

  # needed for rank adjustment
  focal_terms <- grid[focal]
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
        insight::format_warning("No test-statistic found. No p-values were adjusted.")
      }
    } else if (tolower(p_adjust) == "sidak") {
      # sidak adjustment
      params$p.value <- 1 - (1 - params$p.value)^rank_adjust
    }
  } else if (verbose) {
    insight::format_warning(paste0("`p_adjust` must be one of ", toString(all_methods)))
  }
  params
}
