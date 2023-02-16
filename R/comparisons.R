#' @title (Pairwise) comparisons between predictions
#' @name comparisons
#'
#' @description Create...
#'
#' @param x A `ggeffects` object.
#' @param test Hypothesis to test. By default, pairwise-comparisons are conducted.
#'
#' @return A data frame containing...
#'
#' @examples
#' data(efc)
#' efc$c172code <- as.factor(efc$c172code)
#' m <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
#' pred <- ggpredict(m, "c172hour")
#' comparisons(pred)
#' @export
comparisons <- function(x, test = "pairwise") {
  insight::check_if_installed("marginaleffects")

  # retrieve focal predictors
  focal <- attributes(x)$terms
  # retrieve relevant information and generate data grid for predictions
  model <- .get_model_object(x)
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

      if (!is.null(test) && all(test == "pairwise")) {
      } else {
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

  class(out) <- c("ggeffects_comparisons", "data.frame")
  attr(out, "ci") <- 0.95
  out
}


#' @export
format.ggeffects_comparisons <- function(x, ...) {
  insight::format_table(insight::standardize_names(x), ...)
}

#' @export
print.ggeffects_comparisons <- function(x, ...) {
  x <- format(x, ...)
  cat(insight::export_table(x, ...))
}
