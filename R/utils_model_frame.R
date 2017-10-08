#' @importFrom stats model.frame getCall formula
#' @importFrom purrr map_lgl
#' @importFrom dplyr select bind_cols one_of
#' @importFrom prediction find_data
get_model_frame <- function(model, fe.only = TRUE) {
  if (inherits(model, c("merMod", "lmerMod", "glmerMod", "nlmerMod", "merModLmerTest")))
    fitfram <- stats::model.frame(model, fixed.only = fe.only)
  else if (inherits(model, "lme"))
    fitfram <- model$data
  else if (inherits(model, "vgam"))
    fitfram <- prediction::find_data(model)
  else
    fitfram <- stats::model.frame(model)

  # check if we have any matrix columns, e.g. from splines
  mc <- purrr::map_lgl(fitfram, is.matrix)

  # if we have any matrix columns, we remove them from original
  # model frame and convert them to regular data frames, give
  # proper column names and bind them back to the original model frame
  if (any(mc)) {
    fitfram <- dplyr::select(fitfram, -which(mc))
    spline.term <- get_cleaned_varnames(names(which(mc)))
    # try to get model data from environment
    md <- eval(stats::getCall(model)$data, environment(stats::formula(model)))
    # bind spline terms to model frame
    fitfram <- dplyr::bind_cols(fitfram, dplyr::select(md, dplyr::one_of(spline.term)))
  }

  # clean variable names
  colnames(fitfram) <- get_cleaned_varnames(colnames(fitfram))

  fitfram
}
