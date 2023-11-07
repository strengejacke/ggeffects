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


# the "terms" argument can be a character vector, formula, list or data grid
# here we convert it to a character vector, so we have a unique format
.reconstruct_focal_terms <- function(terms = NULL, model = NULL) {
  if (!is.null(terms)) {
    if (inherits(terms, "formula")) {
      # check if terms are a formula
      terms <- all.vars(terms)
    } else if (is.data.frame(terms)) {
      # if "terms" is a data grid, convert it to character
      terms <- .list_to_character_terms(lapply(terms, unique))
    } else if (is.list(terms)) {
      # "terms" can also be a list, convert now
      terms <- .list_to_character_terms(terms)
    }
  }
  if (!is.null(model)) {
    terms <- .check_vars(terms, model)
  }
  terms
}