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

  # retrieve relevant information and generate data grid for predictions
  focal <- attributes(x)$terms
  model <- .get_model_object(x)
  grid <- insight::get_datagrid(model, focal)

  # grid <- expand.grid(c(attributes(x)$at.list, attributes(x)$constant.values))

  .comparisons <- marginaleffects::predictions(
    model,
    newdata = grid,
    hypothesis = test
  )

  if (any(startsWith(.comparisons$term, "Row"))) {
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
    names(out) <- focal
  } else {
    out <- data.frame(Comparison = .comparisons$term, stringsAsFactors = FALSE)
  }

  # further results
  out$contrast <- .comparisons$estimate
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
