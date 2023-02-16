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

}