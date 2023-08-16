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
#' @param by Character vector specifying the names of predictors to condition on.
#'   Hypothesis test is then carried out for focal terms by each level of `by`
#'   variables. This is useful especially for interaction terms, where we want
#'   to test the interaction within "groups". `by` is only relevant for
#'   categorical predictors.
#' @param scale Character string, indicating the scale on which the contrasts
#'   or comparisons are represented. Can be `"response"` (default), which would
#'   return contrasts on the response scale (e.g. for logistic regression, as
#'   probabilities); `"link"` to return contrasts on scale of the linear predictors;
#'   `"probability"` (or `"probs"`) returns contrasts on the probability scale
#'   (which is required for some model classes, like `MASS::polr()`); or a
#'   transformation function like `"exp"` or `"log"`, to return transformed
#'   (exponentiated respectively logarithmic) contrasts.
#'   **Note:** If the `scale` argument is not supported by the provided `model`,
#'   it is automaticaly changed to a supported scale-type (a message is printed
#'   when `verbose = TRUE`).
#' @param equivalence ROPE's lower and higher bounds. Should be `"default"` or
#'   a vector of length two (e.g., c(-0.1, 0.1)). If `"default"`,
#'   [`bayestestR::rope_range()`] is used. Instead of using the `equivalence`
#'   argument, it is also possible to call the `equivalence_test()` method
#'   directly. This requires the **parameters** package to be loaded. When
#'   using `equivalence_test()`, two more columns with information about the
#'   ROPE coverage and decision on H0 are added. Furthermore, it is possible
#'   to `plot()` the results from `equivalence_test()`. See
#'   [`bayestestR::equivalence_test()`] resp. [`parameters::equivalence_test.lm()`]
#'   for details.
#' @param p_adjust Character vector, if not `NULL`, indicates the method to
#'   adjust p-values. See [`stats::p.adjust()`] for details. Further possible
#'   adjustment methods are `"tukey"` or `"sidak"`. Some caution is necessary
#'   when adjusting p-value for multiple comparisons. See also section
#'   _P-value adjustment_ below.
#' @param df Degrees of freedom that will be used to compute the p-values and
#'   confidence intervals. If `NULL`, degrees of freedom will be extracted from
#'   the model using [`insight::get_df()`] with `type = "wald"`.
#' @param ci_level Numeric, the level of the confidence intervals.
#' @param collapse_levels Logical, if `TRUE`, term labels that refer to identical
#'   levels are no longer separated by "-", but instead collapsed into a unique
#'   term label (e.g., `"level a-level a"` becomes `"level a"`). See 'Examples'.
#' @param verbose Toggle messages and warnings.
#' @param ci.lvl Deprecated, please use `ci_level`.
#' @param ... Arguments passed down to [`data_grid()`] when creating the reference
#'   grid and to [`marginaleffects::predictions()`] resp. [`marginaleffects::slopes()`].
#'   For instance, arguments `type` or `transform` can be used to back-transform
#'   comparisons and contrasts to different scales. See examples at the bottom of
#'   [this vignette](https://strengejacke.github.io/ggeffects/articles/introduction_comparisons.html)
#'   for further details.
#'
#' @seealso There is also an `equivalence_test()` method in the **parameters**
#'   package ([`parameters::equivalence_test.lm()`]), which can be used to
#'   test contrasts or comparisons for practical equivalence. This method also
#'   has a `plot()` method, hence it is possible to do something like:
#'   ```
#'   library(parameters)
#'   ggpredict(model, focal_terms) |>
#'     equivalence_test() |>
#'     plot()
#'  ```
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
#' \dontrun{
#' if (requireNamespace("marginaleffects") && interactive()) {
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
#'   # interaction - collapse unique levels
#'   hypothesis_test(m, c("c161sex", "c172code"), collapse_levels = TRUE)
#'
#'   # p-value adjustment
#'   hypothesis_test(m, c("c161sex", "c172code"), p_adjust = "tukey")
#'
#'   # not all comparisons, only by specific group levels
#'   hypothesis_test(m, "c172code", by = "c161sex")
#'
#'   # specific comparisons
#'   hypothesis_test(m, c("c161sex", "c172code"), test = "b2 = b1")
#'
#'   # interaction - slope by groups
#'   m <- lm(barthtot ~ c12hour + neg_c_7 * c172code + c161sex, data = efc)
#'   hypothesis_test(m, c("neg_c_7", "c172code"))
#' }
#' }
#' @export
hypothesis_test <- function(model, ...) {
  UseMethod("hypothesis_test")
}

#' @rdname hypothesis_test
#' @export
hypothesis_test.default <- function(model,
                                    terms = NULL,
                                    by = NULL,
                                    test = "pairwise",
                                    equivalence = NULL,
                                    scale = "response",
                                    p_adjust = NULL,
                                    df = NULL,
                                    ci_level = 0.95,
                                    collapse_levels = FALSE,
                                    verbose = TRUE,
                                    ci.lvl = ci_level,
                                    ...) {
  # check if we have the appropriate package version installed
  insight::check_if_installed("marginaleffects", minimum_version = "0.10.0")

  # when model is a "ggeffects" object, due to environment issues, "model"
  # can be NULL (in particular in tests), thus check for NULL
  if (is.null(model)) {
    insight::format_error("`model` not found. Try to directly pass the model object to `hypothesis_test()`.")
  }

  # only model objects are supported...
  if (!insight::is_model_supported(model)) {
    insight::format_error(
      paste0("Objects of class `", class(model)[1], "` are not yet supported.")
    )
  }

  # process arguments
  if (!missing(ci.lvl)) {
    insight::format_warning("Argument `ci.lvl` is deprecated. Please use `ci_level` instead.")
    ci_level <- ci.lvl
  }
  # ci-level cannot be NA, set to default value then
  if (is.na(ci_level)) {
    ci_level <- 0.95
  }
  miss_scale <- missing(scale)

  # evaluate dots - remove conflicting additional arguments. We have our own
  # "scale" argument that modulates the "type" and "transform" arguments
  # in "marginaleffects"
  dot_args <- list(...)
  dot_args$transform <- NULL
  dot_args$type <- NULL
  # check scale
  scale <- match.arg(scale, choices = c("response", "link", "probability", "probs", "exp", "log"))
  if (scale == "response") {
    dot_args$type <- "response"
  } else if (scale == "link") {
    dot_args$type <- "link"
  } else if (scale %in% c("probability", "probs")) {
    dot_args$type <- "probs"
  } else if (scale == "exp") {
    dot_args$transform <- "exp"
  } else if (scale == "log") {
    dot_args$transform <- "ln"
  }

  # make sure we have a valid type-argument...
  dot_args$type <- .sanitize_type_argument(model, dot_args$type, verbose = ifelse(miss_scale, FALSE, verbose))

  minfo <- insight::model_info(model)

  # for mixed models, we need different handling later...
  need_average_predictions <- insight::is_mixed_model(model)
  msg_intervals <- FALSE

  # by-variables are included in terms
  if (!is.null(by)) {
    terms <- unique(c(terms, by))
  }

  # we want contrasts or comparisons for these focal predictors...  
  focal <- .clean_terms(terms)

  # check if we have a mixed model - in this case, we need to ensure that our
  # random effect variable (group factor) is included in the grid
  if (need_average_predictions) {
    random_group <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)
    if (!all(random_group %in% terms)) {
      terms <- unique(c(terms, random_group))
    }
    # tell user that intervals for comparisons are confidence, not prediction intervals
    if (verbose && any(random_group %in% focal)) {
      msg_intervals <- TRUE
    }
  }
  grid <- data_grid(model, terms, ...)

  # check for valid by-variable
  if (!is.null(by)) {
    # all by-terms need to be in data grid
    if (!all(by %in% colnames(grid))) {
      insight::format_error(
        paste0("Variable(s) `", toString(by[!by %in% colnames(grid)]), "` not found in data grid.")
      )
    }
    # by-terms must be categorical
    by_factors <- vapply(grid[by], is.factor, TRUE)
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

  by_arg <- NULL
  # for models with ordinal/categorical outcome, we need focal terms and
  # response variable as "by" argument
  if (minfo$is_ordinal || minfo$is_multinomial) {
    by_arg <- unique(c(focal, insight::find_response(model)))
  }

  # sanity check - variable names in the grid should not "mask" standard names
  # from the marginal effects output
  reserved <- c(
    "rowid", "group", "term", "contrast", "estimate",
    "std.error", "statistic", "conf.low", "conf.high", "p.value",
    "p.value.nonsup", "p.value.noninf", "type"
  )
  invalid_names <- reserved %in% colnames(grid)
  if (any(invalid_names)) {
    insight::format_error(
      "Some variable names in the model are not allowed when using `hyothesis_test()` because they are reserved by the internally used {.pkg marginaleffects} package.",
      paste0("Please rename following variables and fit your model again: ", toString(paste0("`", reserved[invalid_names], "`")))
    )
  }

  # comparisons only make sense if we have at least two predictors, or if
  # we have one categorical
  focal_numeric <- vapply(grid[focal], is.numeric, TRUE)
  focal_other <- !focal_numeric
  hypothesis_label <- rope_range <- NULL

  # do we want an equivalence-test?
  if (!is.null(equivalence)) {
    if (all(equivalence == "default")) {
      insight::check_if_installed("bayestestR")
      rope_range <- bayestestR::rope_range(model)
    } else {
      rope_range <- equivalence
    }
  }

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
      # prepare argument list for "marginaleffects::slopes"
      # we add dot-args later, that modulate the scale of the contrasts
      args <- list(
        model,
        variables = focal,
        df = df,
        conf_level = ci_level
      )
      .comparisons <- do.call(
        get("avg_slopes", asNamespace("marginaleffects")),
        c(args, dot_args)
      )
      out <- data.frame(x_ = "slope", stringsAsFactors = FALSE)
      colnames(out) <- focal

    } else {
      # prepare argument list for "marginaleffects::slopes"
      # we add dot-args later, that modulate the scale of the contrasts
      args <- list(
        model,
        variables = focal[1],
        by = focal[2:length(focal)],
        newdata = grid,
        hypothesis = test,
        df = df,
        conf_level = ci_level
      )
      # "trends" (slopes) of numeric focal predictor by group levels
      # of other focal predictor
      .comparisons <- do.call(
        get("slopes", asNamespace("marginaleffects")),
        c(args, dot_args)
      )

      ## here comes the code for extracting nice term labels ==============

      # for pairwise comparisons, we need to extract contrasts
      if (!is.null(test) && all(test == "pairwise")) {

        ## pairwise comparisons of slopes -----

        # before we extract labels, we need to check whether any factor level
        # contains a "," - in this case, strplit() will not work properly
        .comparisons$term <- .fix_comma_levels(.comparisons$term, grid, focal)

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

          # create data frame - since we have two categorical predictors at
          # this point (and one numerical), we create a data frame with three
          # columns (one per focal term).
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
          # prepare argument list for "marginaleffects::slopes"
          # we add dot-args later, that modulate the scale of the contrasts
          args <- list(
            model,
            variables = focal[1],
            by = focal[2:length(focal)],
            hypothesis = NULL,
            df = df,
            conf_level = ci_level
          )
          # re-compute comoparisons for all combinations, so we know which
          # estimate refers to which combination of predictor levels
          .full_comparisons <- do.call(
            get("slopes", asNamespace("marginaleffects")),
            c(args, dot_args)
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

    by_variables <- focal # for average predictions
    if (need_average_predictions) {
      # marginaleffects handles single and multiple variables differently here
      if (length(focal) > 1) {
        by_variables <- sapply(focal, function(i) unique(grid[[i]]), simplify = FALSE)
      }
      # prepare argument list for "marginaleffects::avg_predictions"
      # we add dot-args later, that modulate the scale of the contrasts
      args <- list(
        model,
        variables = by_variables,
        newdata = grid,
        hypothesis = test,
        df = df,
        conf_level = ci_level
      )
      fun <- "avg_predictions"
    } else {
      # prepare argument list for "marginaleffects::predictions"
      # we add dot-args later, that modulate the scale of the contrasts
      args <- list(
        model,
        by = by_arg,
        newdata = grid,
        hypothesis = test,
        df = df,
        conf_level = ci_level
      )
      fun <- "predictions"
    }
    .comparisons <- do.call(
      get(fun, asNamespace("marginaleffects")),
      c(args, dot_args)
    )

    ## here comes the code for extracting nice term labels ==============

    # pairwise comparisons - we now extract the group levels from the "term"
    # column and create separate columns for contrats of focal predictors
    if (!is.null(test) && all(test == "pairwise")) {

      ## pairwise comparisons of group levels -----

      # for "predictions()", term name is "Row 1 - Row 2" etc. For
      # "avg_predictions()", we have "level_a1, level_b1 - level_a2, level_b1"
      # etc. we first want to have a data frame, where each column is one
      # combination of levels, so we split at "," and/or "-".

      # before we extract labels, we need to check whether any factor level
      # contains a "," - in this case, strplit() will not work properly
      .comparisons$term <- .fix_comma_levels(.comparisons$term, grid, focal)

      contrast_terms <- data.frame(
        do.call(rbind, strsplit(.comparisons$term, "(,| - )")),
        stringsAsFactors = FALSE
      )
      contrast_terms[] <- lapply(contrast_terms, function(i) {
        # remove certain chars
        for (j in c("(", ")", "Row")) {
          i <- gsub(j, "", i, fixed = TRUE)
        }
        insight::trim_ws(i)
      })

      if (need_average_predictions) {
        # for "avg_predictions()", we already have the correct labels of factor
        # levels, we just need to re-arrange, so that each column represents a
        # pairwise combination of factor levels for each factor
        out <- as.data.frame(lapply(seq_along(focal), function(i) {
          tmp <- contrast_terms[seq(i, ncol(contrast_terms), by = length(focal))]
          unlist(lapply(seq_len(nrow(tmp)), function(j) {
            .contrasts <- as.character(unlist(tmp[j, ]))
            .contrasts_string <- paste(.contrasts, collapse = "-")
          }))
        }), stringsAsFactors = FALSE)
      } else {
        # check whether we have row numbers, or (e.g., for polr or ordinal models)
        # factor levels. When we have row numbers, we coerce them to numeric and
        # extract related factor levels. Else, in case of ordinal outcomes, we
        # should already have factor levels...
        if (all(vapply(contrast_terms, function(i) anyNA(as.numeric(i)), TRUE)) || minfo$is_ordinal || minfo$is_multinomial) {
          out <- as.data.frame(lapply(focal, function(i) {
            unlist(lapply(seq_len(nrow(contrast_terms)), function(j) {
              .contrasts_string <- paste(unlist(contrast_terms[j, ]), collapse = "-")
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
          args <- list(
            model,
            variables = by_variables,
            newdata = grid,
            hypothesis = NULL,
            df = df,
            conf_level = ci_level
          )
          fun <- "avg_predictions"
        } else {
          args <- list(
            model,
            newdata = grid,
            hypothesis = NULL,
            df = df,
            conf_level = ci_level
          )
          fun <- "predictions"
        }
        .full_comparisons <- do.call(
          get(fun, asNamespace("marginaleffects")),
          c(args, dot_args)
        )

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

  # add result from equivalence test
  if (!is.null(rope_range)) {
    .comparisons <- marginaleffects::hypotheses(.comparisons, equivalence = rope_range, conf_level = ci_level)
    .comparisons$p.value <- .comparisons$p.value.equiv
  }

  # further results
  out[[estimate_name]] <- .comparisons$estimate
  out$conf.low <- .comparisons$conf.low
  out$conf.high <- .comparisons$conf.high
  out$p.value <- .comparisons$p.value

  # for pairwise comparisons, we may have comparisons inside one level when we
  # have multiple focal terms, like "1-1" and "a-b". In this case, the comparison
  # of 1 to 1 ("1-1") is just the contrast for the level "1", we therefore can
  # collpase that string
  if (isTRUE(collapse_levels)) {
    out <- .collapse_levels(out, grid, focal)
  }

  # replace back commas
  for (i in focal) {
    # in ".fix_comma_levels()", we replaced "," by "#*#", and now
    # we need to revert this, to preserve original level strings
    if (any(grepl("#*#", out[[i]], fixed = TRUE))) {
      out[[i]] <- gsub("#*#", ",", out[[i]], fixed = TRUE)
    }
  }

  # filter by-variables?
  if (!is.null(by)) {
    for (by_factor in by) {
      # values in "by" are character vectors, which are saved as "level-level".
      # we now extract the unique values, and filter the data frame
      unique_values <- unique(grid[[by_factor]])
      by_levels <- paste0(unique_values, "-", unique_values)
      keep_rows <- out[[by_factor]] %in% c(by_levels, unique_values)
      # filter final data frame
      out <- out[keep_rows, , drop = FALSE]
      # but we also need to filter the ".comparisons" data frame
      .comparisons <- .comparisons[keep_rows, , drop = FALSE]
      # finally, replace "level-level" just by "level"
      for (i in seq_along(by_levels)) {
        out[[by_factor]] <- gsub(
          by_levels[i],
          unique_values[i],
          out[[by_factor]],
          fixed = TRUE
        )
      }
    }
    # remove by-terms from focal terms
    focal <- focal[!focal %in% by]
  }

  # p-value adjustment?
  if (!is.null(p_adjust)) {
    out <- .p_adjust(out, p_adjust, .comparisons$statistic, grid, focal, df, verbose)
  }

  # add back response levels?
  if ("group" %in% colnames(.comparisons)) {
    out <- cbind(
      data.frame(response.level = .comparisons$group, stringsAsFactors = FALSE),
      out
    )
  } else if (minfo$is_ordinal || minfo$is_multinomial) {
    resp_levels <- levels(insight::get_response(model))
    if (!is.null(resp_levels) && all(rowMeans(sapply(resp_levels, grepl, .comparisons$term, fixed = TRUE)) > 0)) {
      colnames(out)[seq_along(focal)] <- paste0("Response Level by ", paste0(focal, collapse = " & "))
      if (length(focal) > 1) {
        out[2:length(focal)] <- NULL
      }
    }
  }

  class(out) <- c("ggcomparisons", "data.frame")
  attr(out, "ci_level") <- ci_level
  attr(out, "test") <- test
  attr(out, "p_adjust") <- p_adjust
  attr(out, "df") <- df
  attr(out, "rope_range") <- rope_range
  attr(out, "scale") <- scale
  attr(out, "scale_label") <- .scale_label(minfo, scale)
  attr(out, "linear_model") <- minfo$is_linear
  attr(out, "hypothesis_label") <- hypothesis_label
  attr(out, "estimate_name") <- estimate_name
  attr(out, "msg_intervals") <- msg_intervals
  attr(out, "verbose") <- verbose
  out
}


#' @rdname hypothesis_test
#' @export
hypothesis_test.ggeffects <- function(model,
                                      by = NULL,
                                      test = "pairwise",
                                      equivalence = NULL,
                                      p_adjust = NULL,
                                      df = NULL,
                                      collapse_levels = FALSE,
                                      verbose = TRUE,
                                      ...) {
  # retrieve focal predictors
  focal <- attributes(model)$original.terms
  # retrieve focal predictors
  ci_level <- attributes(model)$ci.lvl
  # retrieve relevant information and generate data grid for predictions
  model <- .get_model_object(model)

  hypothesis_test.default(
    model,
    terms = focal,
    by = by,
    test = test,
    equivalence = equivalence,
    p_adjust = p_adjust,
    df = df,
    ci_level = ci_level,
    collapse_levels = collapse_levels,
    verbose = verbose,
    ...
  )
}


# helper ------------------------


.collapse_levels <- function(out, grid, focal) {
  # iterate all focal terms, these are the column names in "out"
  for (i in focal) {
    flag_dash <- FALSE
    # for factors, we need to check whether factor levels contain "-"
    # if so, we need to replace it, else "strplit()" won't work"
    if (is.factor(grid[[i]])) {
      l <- levels(grid[[i]])
      dash_levels <- grepl("-", l, fixed = TRUE)
      if (any(dash_levels)) {
        for (j in l[dash_levels]) {
          # replace by a - hopefully - unique character, later revert
          out[[i]] <- gsub(j, gsub("-", "#~#", j, fixed = TRUE), out[[i]], fixed = TRUE)
          flag_dash <- TRUE
        }
      }
    }
    pairs <- strsplit(out[[i]], "-", fixed = TRUE)
    all_same <- vapply(pairs, function(j) {
      all(j == j[1])
    }, TRUE)
    if (any(all_same)) {
      out[[i]][all_same] <- vapply(pairs[all_same], unique, character(1))
    }
    # revert replacement
    if (flag_dash) {
      out[[i]] <- gsub("#~#", "-", out[[i]], fixed = TRUE)
      flag_dash <- FALSE
    }
  }
  out
}


.fix_comma_levels <- function(terms, grid, focal) {
  for (i in focal) {
    if (is.factor(grid[[i]])) {
      l <- levels(grid[[i]])
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
  pattern <- c("=", "-", "+", "/", "*")
  for (p in pattern) {
    old_labels <- gsub(p, paste0(" ", p, " "), old_labels, fixed = TRUE)
  }
  old_labels
}


.scale_label <- function(minfo, scale) {
  scale_label <- NULL
  if (minfo$is_binomial || minfo$is_ordinal || minfo$is_multinomial) {
    scale_label <- switch(scale,
      response = "probabilities",
      link = "log-odds",
      exp = "odds ratios",
      probs = ,
      probability = "probabilities",
      NULL
    )
  } else if (minfo$is_count) {
    scale_label <- switch(scale,
      response = "counts",
      link = "log-mean",
      exp = "incident rate ratios",
      probs = ,
      probability = "probabilities",
      NULL
    )
  }
  scale_label
}



# methods ----------------------------


#' @export
format.ggcomparisons <- function(x, ...) {
  ci <- attributes(x)$ci_level
  out <- insight::standardize_names(x)
  attr(out, "ci") <- ci
  insight::format_table(out, ...)
}


#' @export
print.ggcomparisons <- function(x, ...) {
  test_pairwise <- identical(attributes(x)$test, "pairwise")
  estimate_name <- attributes(x)$estimate_name
  rope_range <- attributes(x)$rope_range
  msg_intervals <- isTRUE(attributes(x)$msg_intervals)
  verbose <- isTRUE(attributes(x)$verbose)
  scale <- attributes(x)$scale
  scale_label <- attributes(x)$scale_label
  is_linear <- isTRUE(attributes(x)$linear_model)

  # get header and footer, then print table
  x <- format(x, ...)
  slopes <- vapply(x, function(i) all(i == "slope"), TRUE)
  if (!is.null(rope_range)) {
    caption <- c("# TOST-test for Practical Equivalence", "blue")
  } else if (any(slopes)) {
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
  newline <- ifelse(is.null(footer), "\n", "")

  # split tables by response levels?
  if ("Response_Level" %in% colnames(x)) {
    x$Response_Level <- factor(x$Response_Level, levels = unique(x$Response_Level))
    out <- split(x, x$Response_Level)
    for (response_table in seq_along(out)) {
      insight::print_color(paste0("\n# Response Level: ", names(out)[response_table], "\n\n"), "red")
      tab <- out[[response_table]]
      tab$Response_Level <- NULL
      if (response_table == 1) {
        cat(insight::export_table(tab, title = caption, footer = NULL, ...))
      } else if (response_table == length(out)) {
        cat(insight::export_table(tab, title = NULL, footer = footer, ...))
      } else {
        cat(insight::export_table(tab, title = NULL, footer = NULL, ...))
      }
    }
  } else {
    cat(insight::export_table(x, title = caption, footer = footer, ...))
  }

  # what type of estimates do we have?
  type <- switch(estimate_name,
    "Predicted" = "Predictions",
    "Contrast" = "Contrasts",
    "Slope" = "Slopes",
    "Estimates"
  )

  # tell user about scale of estimate type
  if (verbose && !(is_linear && identical(scale, "response"))) {
    if (is.null(scale_label)) {
      scale_label <- switch(scale,
        response = "response",
        probs = ,
        probability = "probability",
        exp = "exponentiated",
        log = "log",
        link = "link"
      )
      msg <- paste0(newline, type, " are presented on the ", scale_label, " scale.")
    } else {
      msg <- paste0(newline, type, " are presented as ", scale_label, ".")
    }
    insight::format_alert(msg)
  }

  # tell user about possible discrepancies between prediction intervals of
  # predictions and confidence intervals of contrasts/comparisons
  if (msg_intervals && verbose) {
    insight::format_alert(
      "\nIntervals used for contrasts and comparisons are regular confidence intervals, not prediction intervals.",
      "To obtain the same type of intervals for your predictions, use `ggpredict(..., interval = \"confidence\")`."
    )
  }
}


#' @export
plot.see_equivalence_test_ggeffects <- function(x,
                                                size_point = 0.7,
                                                rope_color = "#0171D3",
                                                rope_alpha = 0.2,
                                                show_intercept = FALSE,
                                                n_columns = 1,
                                                ...) {
  insight::check_if_installed("ggplot2")
  .data <- NULL

  .rope <- c(x$ROPE_low[1], x$ROPE_high[1])

  # check for user defined arguments

  fill.color <- c("#CD423F", "#018F77", "#FCDA3B")
  legend.title <- "Decision on H0"
  x.title <- NULL

  fill.color <- fill.color[sort(unique(match(x$ROPE_Equivalence, c("Accepted", "Rejected", "Undecided"))))]

  add.args <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)
  if ("colors" %in% names(add.args)) fill.color <- eval(add.args[["colors"]])
  if ("x.title" %in% names(add.args)) x.title <- eval(add.args[["x.title"]])
  if ("legend.title" %in% names(add.args)) legend.title <- eval(add.args[["legend.title"]])
  if ("labels" %in% names(add.args)) labels <- eval(add.args[["labels"]])

  rope.line.alpha <- 1.25 * rope_alpha
  if (rope.line.alpha > 1) rope.line.alpha <- 1

  # make sure we have standardized column names for parameters and estimates
  parameter_columns <- attributes(x)$parameter_columns
  estimate_columns <- which(colnames(x) %in% c("Estimate", "Slope", "Predicted", "Contrast"))
  colnames(x)[estimate_columns[1]] <- "Estimate"

  if (length(parameter_columns) > 1) {
    x$Parameter <- unname(apply(x[parameter_columns], MARGIN = 1, toString))
  } else {
    x$Parameter <- x[[parameter_columns]]
  }

  p <- ggplot2::ggplot(
    x,
    ggplot2::aes(
      y = .data[["Parameter"]],
      x = .data[["Estimate"]],
      xmin = .data[["CI_low"]],
      xmax = .data[["CI_high"]],
      colour = .data[["ROPE_Equivalence"]]
    )
  ) +
    ggplot2::annotate(
      "rect",
      xmin = .rope[1],
      xmax = .rope[2],
      ymin = 0,
      ymax = Inf,
      fill = rope_color,
      alpha = (rope_alpha / 3)
    ) +
    ggplot2::geom_vline(
      xintercept = .rope,
      linetype = "dashed",
      colour = rope_color,
      linewidth = 0.8,
      alpha = rope.line.alpha
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      colour = rope_color,
      linewidth = 0.8,
      alpha = rope.line.alpha
    ) +
    ggplot2::geom_pointrange(size = size_point) +
    ggplot2::scale_colour_manual(values = fill.color) +
    ggplot2::labs(y = x.title, x = NULL, colour = legend.title) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_y_discrete()

  p
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
