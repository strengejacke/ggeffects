#' @importFrom sjmisc trim
.clean_terms <- function(x) {
  # get positions of variable names and see if we have
  # a suffix for certain values
  cleaned.pos <- regexpr(pattern = "(\\s|\\[)", x)

  # position "-1" means we only had variable name, no suffix
  replacers <- which(cleaned.pos == -1)
  # replace -1 with number of chars
  cleaned.pos[replacers] <- nchar(x)[replacers]

  # get variable names only
  x <- sjmisc::trim(substr(x, 0, cleaned.pos))

  # be sure to remove any brackets
  sub("[", "", x, fixed = TRUE)
}
