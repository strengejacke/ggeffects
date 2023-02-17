#' @title (Pairwise) comparisons between predictions
#' @name ggcomparisons
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
#' data(efc)
#' efc$c172code <- as.factor(efc$c172code)
#' efc$c161sex <- as.factor(efc$c161sex)
#' levels(efc$c161sex) <- c("male", "female")
#' m <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
#'
#' # direct computation of comparisons
#' ggcomparisons(m, "c172code")
#'
#' # passing a `ggeffects` object
#' pred <- ggpredict(m, "c172code")
#' ggcomparisons(pred)
#'
#' # test for slope
#' ggcomparisons(m, "c12hour")
#'
#' # interaction - contrasts by groups
#' m <- lm(barthtot ~ c12hour + c161sex * c172code + neg_c_7, data = efc)
#' ggcomparisons(m, c("c161sex", "c172code"), test = NULL)
#'
#' # interaction - pairwise comparisons by groups
#' m <- lm(barthtot ~ c12hour + c161sex * c172code + neg_c_7, data = efc)
#' ggcomparisons(m, c("c161sex", "c172code"))
#'
#' # interaction - slope by groups
#' m <- lm(barthtot ~ c12hour + neg_c_7 * c172code + c161sex, data = efc)
#' ggcomparisons(m, c("neg_c_7", "c172code"))
#' @export
ggcomparisons <- function(model, ...) {
  UseMethod("ggcomparisons")
}

#' @rdname ggcomparisons
#' @export
ggcomparisons.default <- function(model, terms = NULL, test = "pairwise", ...) {
  insight::check_if_installed("marginaleffects")

  # only model objects are supported...
  if (!insight::is_model_supported(model)) {
    insight::format_error(
      paste0("Objects of class `", class(model)[1], "` are not yet supported.")
    )
  }

  focal <- .clean_terms(terms)
  grid <- insight::get_datagrid(model, focal)
  # grid <- expand.grid(c(attributes(x)$at.list, attributes(x)$constant.values))

  # comparisons only make sense if we have at least two predictors, or if
  # we have one categorical
  focal_numeric <- vapply(grid[focal], is.numeric, TRUE)
  focal_other <- !focal_numeric

  if (length(focal) > 1 && all(focal_numeric)) {
    insight::format_error(
      "Pairwise comparisons for multiple focal predictors can only be computed if at least one predictor is categorical."
    )
  }

  # testing slopes -----
  if (any(focal_numeric)) {
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

      } else {
        # if we have simple slopes without pairwise comparisons, we can
        # copy the information directly from the marginaleffects-object
        grid_categorical <- as.data.frame(.comparisons[focal[2:length(focal)]])
        out <- cbind(data.frame(x_ = "slope", stringsAsFactors = FALSE), grid_categorical)
      }
    }
    colnames(out) <- focal
    estimate_name <- ifelse(is.null(test), "Slope", "Contrast")

  } else {
    # testing groups (factors) -----
    .comparisons <- marginaleffects::predictions(
      model,
      newdata = grid,
      hypothesis = test
    )

    if (!is.null(test) && all(test == "pairwise")) {
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
    } else {
      out <- data.frame(Comparison = .comparisons$term, stringsAsFactors = FALSE)
    }
    estimate_name <- "Contrast"
  }

  # further results
  out[[estimate_name]] <- .comparisons$estimate
  out$conf.low <- .comparisons$conf.low
  out$conf.high <- .comparisons$conf.high
  out$p.value <- .comparisons$p.value

  class(out) <- c("ggcomparisons", "data.frame")
  attr(out, "ci") <- 0.95
  out
}


#' @rdname ggcomparisons
#' @export
ggcomparisons.ggeffects <- function(model, test = "pairwise", ...) {
  # retrieve focal predictors
  focal <- attributes(model)$terms
  # retrieve relevant information and generate data grid for predictions
  model <- .get_model_object(model)
  ggcomparisons.default(model, terms = focal, test = test, ...)
}


# methods ----------------------------

#' @export
format.ggcomparisons <- function(x, ...) {
  insight::format_table(insight::standardize_names(x), ...)
}

#' @export
print.ggcomparisons <- function(x, ...) {
  x <- format(x, ...)
  slopes <- vapply(x, function(i) all(i == "slope"), TRUE)
  if (any(slopes)) {
    x[slopes] <- NULL
    caption <- c(paste0("# Trend for ", names(slopes)[slopes]), "blue")
  } else {
    caption <- NULL
  }
  cat(insight::export_table(x, title = caption, ...))
}
