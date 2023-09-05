#' @title Collapse raw data by random effect groups
#' @name collapse_by_group
#'
#' @description This function extracts the raw data points (i.e. the data
#'   that was used to fit the model) and "averages" (i.e. "collapses") the
#'   response variable over the levels of the grouping factor given in
#'   `collapse_by`. Only works with mixed models.
#'
#' @param collapse_by Name of the (random effects) grouping factor. Data is
#'   collapsed by the levels of this factor.
#' @param residuals Logical, if `TRUE`, collapsed partial residuals instead
#'   of raw data by the levels of the grouping factor.
#' @inheritParams residualize_over_grid
#'
#' @return A data frame with raw data points, averaged over the levels of
#'   the given grouping factor from the random effects. The group level of
#'   the random effect is saved in the column `"random"`.
#'
#' @examples
#' library(ggeffects)
#' if (require("lme4", quietly = TRUE)) {
#'   data(efc)
#'   efc$e15relat <- as.factor(efc$e15relat)
#'   efc$c161sex <- as.factor(efc$c161sex)
#'   levels(efc$c161sex) <- c("male", "female")
#'   model <- lmer(neg_c_7 ~ c161sex + (1 | e15relat), data = efc)
#'   me <- ggpredict(model, terms = "c161sex")
#'   head(attributes(me)$rawdata)
#'   collapse_by_group(me, model, "e15relat")
#' }
#' @export
collapse_by_group <- function(grid, model, collapse_by = NULL, residuals = FALSE, collapse.by = collapse_by) {

  ## TODO: handle deprecated argument
  if (!missing(collapse.by)) {
    collapse_by <- collapse.by
  }

  if (!insight::is_mixed_model(model)) {
    insight::format_error("This function only works with mixed effects models.")
  }

  data <- insight::get_data(model, source = "frame")

  if (is.null(collapse_by)) {
    collapse_by <- insight::find_random(model, flatten = TRUE)
  }

  if (length(collapse_by) > 1) {
    collapse_by <- collapse_by[1]
    insight::format_alert(
      "More than one random grouping variable found.",
      paste0("Using `", collapse_by, "`.")
    )
  }

  if (!collapse_by %in% colnames(data)) {
    insight::format_error("Could not find `", collapse_by, "` column.")
  }

  if (residuals) {
    rawdata <- residualize_over_grid(grid, model, protect_names = TRUE)
    y_name <- "predicted"
  } else {
    rawdata <- attr(grid, "rawdata", exact = TRUE)
    y_name <- "response"

    if (any(sapply(rawdata[-(1:2)], Negate(is.factor))) || attr(grid, "x.is.factor", exact = TRUE) == "0") {
      insight::format_alert("Collapsing usually not informative across a continuous variable.")
    }
  }

  if (is.factor(rawdata[[y_name]])) {
    rawdata[[y_name]] <- as.numeric(rawdata[[y_name]])
    if (insight::model_info(model)$is_binomial) {
      rawdata[[y_name]] <- rawdata[[y_name]] - 1
    } # else ordinal?
  }

  rawdata$random <- factor(data[[collapse_by]])

  agg_data <- stats::aggregate(rawdata[[y_name]],
                               by = rawdata[colnames(rawdata) != y_name],
                               FUN = mean)

  colnames(agg_data)[ncol(agg_data)] <- y_name
  colnames(agg_data)[colnames(agg_data) == "group"] <- "group_col"

  # sanity check, add dummy if not present
  if (is.null(agg_data$group_col)) {
    agg_data$group_col <- factor(1)
  }

  agg_data
}
