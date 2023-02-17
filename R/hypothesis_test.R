#' @title (Pairwise) comparisons between predictions
#' @name hypothesis_test
#'
#' @description Create...
#'
#' @param model A fitted model object, or an object of class `ggeffects`.
#' @param terms Character vector with the names of the focal terms from `model`,
#'   for which contrasts or comparisons should be displayed. At least one term
#'   is required, maximum length is three terms. One focal term may be numeric.
#'   In this case, contrasts or comparisons for the *slopes* of the numeric
#'   predictor are computed (possibly grouped by the levels of further categorical
#'   focal predictors).
#' @param test Hypothesis to test. By default, pairwise-comparisons are conducted.
#' @param ... Currently not used.
#'
#' @return A data frame containing...
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
hypothesis_test.default <- function(model, terms = NULL, test = "pairwise", ...) {
  insight::check_if_installed("marginaleffects")

  # only model objects are supported...
  if (!insight::is_model_supported(model)) {
    insight::format_error(
      paste0("Objects of class `", class(model)[1], "` are not yet supported.")
    )
  }

  # we want contrasts or comparisons for these focal predictors...
  focal <- .clean_terms(terms)

  grid <- insight::get_datagrid(model, focal)
  # grid <- expand.grid(c(attributes(x)$at.list, attributes(x)$constant.values))

  # comparisons only make sense if we have at least two predictors, or if
  # we have one categorical
  focal_numeric <- vapply(grid[focal], is.numeric, TRUE)
  focal_other <- !focal_numeric
  hypothesis_label <- NULL

  if (length(focal) > 1 && all(focal_numeric)) {
    insight::format_error(
      "Pairwise comparisons for multiple focal predictors can only be computed if at least one predictor is categorical."
    )
  }

  if (any(focal_numeric)) {

    # testing slopes =====

    # reorder grid, making numeric first
    grid[c(which(focal_numeric), which(focal_other))] <- grid[focal]
    colnames(grid)[c(which(focal_numeric), which(focal_other))] <- focal
    # reorder focal terms to match grid
    focal <- focal[c(which(focal_numeric), which(focal_other))]

    # just the "trend" (slope) of one focal predictor
    if (length(focal) == 1) {
      # argument "test" will be ignored for average slopes
      test <- NULL
      .comparisons <- marginaleffects::avg_slopes(model, variables = focal)
      out <- data.frame(x_ = "slope", stringsAsFactors = FALSE)
      colnames(out) <- focal

    } else {
      # "trends" (slopes) of numeric focal predictor by group levels
      # of other focal predictor
      .comparisons <- marginaleffects::slopes(
        model,
        variables = focal[1],
        by = focal[2:length(focal)],
        hypothesis = test
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
        grid_categorical <- as.data.frame(.comparisons[focal[2:length(focal)]])
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
            hypothesis = NULL
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

    .comparisons <- marginaleffects::predictions(
      model,
      newdata = grid,
      hypothesis = test
    )

    # pairwise comparisons - we now extract the group levels from the "term"
    # column and create separate columns for contrats of focal predictors
    if (!is.null(test) && all(test == "pairwise")) {

      ## pairwise comparisons of group levels -----

      contrast_terms <- data.frame(
        do.call(rbind, strsplit(.comparisons$term, " - ", fixed = TRUE)),
        stringsAsFactors = FALSE
      )
      contrast_terms[] <- lapply(contrast_terms, function(i) {
        insight::trim_ws(gsub("Row", "", i, fixed = TRUE))
      })

      out <- as.data.frame(lapply(focal, function(i) {
        unlist(lapply(seq_len(nrow(contrast_terms)), function(j) {
          .contrasts <- grid[[i]][as.numeric(unlist(contrast_terms[j, ]))]
          .contrasts_string <- paste(.contrasts, collapse = "-")
        }))
      }))
      colnames(out) <- focal

    } else if (is.null(test)) {

      ## contrasts of group levels -----

      # we have simple contrasts - we can just copy from the data frame
      # returned by "marginaleffects"
      out <- as.data.frame(.comparisons[focal])

    } else {

      ## hypothesis testing of group levels -----

      # if we have specific comparisons of estimates, like "b1 = b2", we
      # want to replace these shortcuts with the full related predictor names
      # and levels
      if (any(grepl("b[0-9]+", .comparisons$term))) {
        # re-compute comoparisons for all combinations, so we know which
        # estimate refers to which combination of predictor levels
        .full_comparisons <- marginaleffects::predictions(
          model,
          newdata = grid,
          hypothesis = NULL
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

  # further results
  out[[estimate_name]] <- .comparisons$estimate
  out$conf.low <- .comparisons$conf.low
  out$conf.high <- .comparisons$conf.high
  out$p.value <- .comparisons$p.value

  class(out) <- c("ggcomparisons", "data.frame")
  attr(out, "ci") <- 0.95
  attr(out, "test") <- test
  attr(out, "hypothesis_label") <- hypothesis_label
  out
}


#' @rdname hypothesis_test
#' @export
hypothesis_test.ggeffects <- function(model, test = "pairwise", ...) {
  # retrieve focal predictors
  focal <- attributes(model)$terms
  # retrieve relevant information and generate data grid for predictions
  model <- .get_model_object(model)
  hypothesis_test.default(model, terms = focal, test = test, ...)
}


# helper ------------------------

.extract_labels <- function(full_comparisons, focal, test, old_labels) {
  # now we have both names of predictors and their levels
  beta_rows <- full_comparisons[focal]
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
