.clean_terms <- function(x) {
  if (is.list(x)) {
    return(names(x))
  }
  # get positions of variable names and see if we have
  # a suffix for certain values
  cleaned.pos <- regexpr(pattern = "[", x, fixed = TRUE)
  # cleaned.pos <- regexpr(pattern = "(\\s|\\[)", x)

  # position "-1" means we only had variable name, no suffix
  replacers <- which(cleaned.pos == -1)
  # replace -1 with number of chars
  cleaned.pos[replacers] <- nchar(x)[replacers]

  # get variable names only
  x <- trimws(substr(x, 0, cleaned.pos))

  # be sure to remove any brackets
  trimws(sub("[", "", x, fixed = TRUE))
}


.list_to_character_terms <- function(x) {
  if (is.list(x)) {
    x <- unlist(lapply(names(x), function(i) {
      paste0(i, " [", toString(x[[i]]), "]")
    }))
  }
  x
}
