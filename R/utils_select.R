#' @keywords internal
string_contains <- function(pattern, x) {
  pattern <- paste0("\\Q", pattern, "\\E")
  grep(pattern, x, perl = TRUE)
}

#' @keywords internal
string_ends_with <- function(pattern, x) {
  pattern <- paste0("\\Q", pattern, "\\E$")
  grep(pattern, x, perl = TRUE)
}

#' @keywords internal
string_one_of <- function(pattern, x) {
  m <- unlist(lapply(pattern, function(.x) grep(.x, x, fixed = TRUE, useBytes = TRUE)))
  x[m]
}

#' @keywords internal
.rownames_as_column <- function(x, var = "rowname") {
  rn <- data.frame(rn = rownames(x), stringsAsFactors = FALSE)
  x <- cbind(rn, x)
  colnames(x)[1] <- var
  rownames(x) <- NULL
  x
}

#' @keywords internal
.obj_has_name <- function(x, name) {
  name %in% names(x)
}

#' @keywords internal
obj_has_rownames <- function(x) {
  !identical(as.character(seq_len(nrow(x))), rownames(x))
}
