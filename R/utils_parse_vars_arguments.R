# This method extracts values for prediction and effect
# transformation, that are used inside "terms" argument.
# e.g. "age [50, 80]" should return the values 50 and 80.
# Note that this method separates specified levels at comma.
# use "get_xlevels_vector" to return a list, where numeric levels
# are still associated with the variable
#' @importFrom sjmisc is_empty trim
#' @importFrom stats setNames
get_xlevels_single <- function(x) {
  # get variable with suffix
  vars.pos <-
    which(as.vector(regexpr(
      pattern = " ([^\\]]*)\\]",
      text = x,
      perl = T
    )) != -1)

  # is empty?
  if (sjmisc::is_empty(vars.pos)) return(NULL)

  # get variable names. needed later to set as
  # names attributes
  vars.names <- get_clear_vars(x)[vars.pos]

  # check if we have variables, where more levels were separated with comma
  commas <- get_comma_count(x[vars.pos])

  # repeat variable names, so we have the correct
  # variable name for each level
  dummy <- c()
  for (i in seq_len(length(commas))) {
    dummy <- c(dummy, rep(vars.names[i], times = commas[i] + 1))
  }
  # copy back repeated variable names
  vars.names <- dummy

  # get levels inside brackets
  tmp <- unlist(regmatches(
    x,
    gregexpr(
      pattern = " ([^\\]]*)\\]",
      text = x,
      perl = T
    )
  ))

  # see if we have multiple values, split at comma
  tmp <- unlist(strsplit(tmp, ",", fixed = T))

  # remove brackets and whitespace
  tmp <- sjmisc::trim(gsub("(\\[*)(\\]*)", "", tmp))

  # return as numeric
  if (!anyNA(as.numeric(tmp))) tmp <- as.numeric(tmp)

  stats::setNames(tmp, vars.names)
}


# return levels, as list
# c("age", "edu [1,3]", "sex [2]") would return a list:
# $edu [1] 1 3; $sex [1] 2
#' @importFrom sjmisc is_empty trim str_contains
#' @importFrom purrr map
#' @importFrom stats setNames
#' @importFrom sjlabelled as_numeric
get_xlevels_vector <- function(x) {
  # get variable with suffix
  vars.pos <-
    which(as.vector(regexpr(
      pattern = " ([^\\]]*)\\]",
      text = x,
      perl = T
    )) != -1)

  # is empty?
  if (sjmisc::is_empty(vars.pos)) return(NULL)

  # get variable names. needed later to set as
  # names attributes
  vars.names <- get_clear_vars(x)[vars.pos]

  # get levels inside brackets
  tmp <- unlist(regmatches(
    x,
    gregexpr(
      pattern = " ([^\\]]*)\\]",
      text = x,
      perl = T
    )
  ))

  # remove brackets
  tmp <- gsub("(\\[*)(\\]*)", "", tmp)

  # see if we have multiple values, split at comma
  tmp <- sjmisc::trim(strsplit(tmp, ",", fixed = T))

  # now check for ranges
  tmp <-
    purrr::map(tmp, function(x) {
      if (sjmisc::str_contains(x, ":")) {
        s <- sjmisc::trim(strsplit(x, ":", fixed = T)) %>%
          unlist() %>%
          sjlabelled::as_numeric()
        x <- seq(from = s[1], to = s[2], by = 1)
      }

      x
    })

  # check if levels were numeric or not...
  suppressWarnings(
    if (!anyNA(unlist(lapply(tmp, as.numeric))))
      tmp <- lapply(tmp, as.numeric)
  )

  stats::setNames(tmp, vars.names)
}


get_comma_count <- function(x) {
  unlist(lapply(gregexpr(
    pattern = ",",
    text = x,
    fixed = T
  ), function(x)
    if (x[1] != -1)
      length(x)
  else
    0))
}


#' @importFrom sjmisc trim
get_clear_vars <- function(x) {
  # get positions of variable names and see if we have
  # a suffix for certain values
  cleaned.pos <- regexpr(pattern = "\\s", x)

  # position "-1" means we only had variable name, no suffix
  replacers <- which(cleaned.pos == -1)
  # replace -1 with number of chars
  cleaned.pos[replacers] <- nchar(x)[replacers]

  # get variable names only
  sjmisc::trim(substr(x, 0, cleaned.pos))
}
