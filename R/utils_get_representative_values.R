# return levels, as list
# c("age", "edu [1,3]", "sex [2]") would return a list:
# $edu [1] 1 3; $sex [1] 2
#' @importFrom sjmisc is_empty trim str_contains is_num_fac
#' @importFrom purrr possibly
#' @importFrom stats setNames sd
#' @importFrom sjlabelled as_numeric
.get_representative_values <- function(x, model_frame = NULL) {
  # get variable with suffix
  terms_with_suffix <- which(as.vector(regexpr(pattern = "([^\\]]*)\\]", text = x, perl = TRUE)) != -1)

  # is empty?
  if (sjmisc::is_empty(terms_with_suffix)) return(NULL)

  # get variable names. needed later to set as
  # names attributes
  at_terms <- .clean_terms(x)[terms_with_suffix]

  # get levels inside brackets
  at_levels <- unlist(regmatches(x, gregexpr(pattern = "\\[(.*)\\]", text = x, perl = TRUE)))

  # remove brackets
  at_levels <- gsub("(\\[*)(\\]*)", "", at_levels)

  # see if we have multiple values, split at comma
  at_levels <- sjmisc::trim(strsplit(at_levels, ",", fixed = T))

  # moderator pattern
  at_pattern <- c("minmax", "meansd", "zeromax", "quart2", "all", "quart")

  # now check for ranges
  at_levels <- purrr::map2(at_levels, at_terms, function(x, y) {

    # Here we may have a range of values. we then create the
    # sequence with all values from this range

    if (sjmisc::str_contains(x, ":")) {

      # values at sequence (from to) ------------------------------------------

      from_to_by <- s <- unlist(sjmisc::trim(strsplit(x, ":", fixed = T)))
      if (grepl("by", s[2], fixed = TRUE)) {
        from_to_by[2] <- sub("(.*)(\\s*)by(\\s*)=(.*)", "\\1", x = s[2])
        from_to_by[3] <- sub("(.*)(\\s*)by(\\s*)=(.*)", "\\4", x = s[2])
      } else {
        from_to_by[3] <- "1"
      }

      from_to_by <- as.numeric(from_to_by)
      x <- seq(from = from_to_by[1], to = from_to_by[2], by = from_to_by[3])

    } else if (length(x) == 1 && grepl("^n(\\s*)=", x)) {

      # values at pretty range -----------------------------------------------

      steps <- as.numeric(sjmisc::trim(substring(gsub(" ", "", x), first = 3)))
      x <- pretty_range(model_frame[[y]], n = steps)

    } else if (length(x) == 1 && grepl("^sample(\\s*)=", x)) {

      # values at random samples ---------------------------------------------

      size <- as.numeric(sjmisc::trim(substring(gsub(" ", "", x), first = 8)))
      lev <- stats::na.omit(unique(model_frame[[y]]))
      pos <- sample.int(n = length(lev), size = size, replace = FALSE)
      x <- lev[pos]

      if (is.factor(x)) {
        if (sjmisc::is_num_fac(x)) {
          x <- sjlabelled::as_numeric(
            droplevels(x),
            keep.labels = FALSE
          )
        } else {
          x <- as.character(x)
        }
      }

    } else if (length(x) == 1 && grepl("[[:alpha:]]", x)) {

      # values at function ---------------------------------------------

      # else, we also may have a character expression. This may
      # either be the name of a valid function. In this case, we
      # transform the values for predictions using this function.
      # Else, it also might be the name of a value labels, so no
      # valid function name. In this case, simply return the label.

      if (x == "pretty") {
        x <- pretty_range(model_frame[[y]])
      } else if (x %in% at_pattern) {
        x <- values_at(model_frame[[y]], values = x)
      } else {
        at_function <- purrr::possibly(match.fun, NULL)
        funtrans <- at_function(x)
        if (!is.null(funtrans) && !is.null(model_frame)) {
          x <- funtrans(sort(unique(model_frame[[y]])))
        }
      }
    }

    x
  })

  # check if levels were numeric or not...
  suppressWarnings(
    if (!anyNA(unlist(lapply(at_levels, as.numeric)))) {
      at_levels <- lapply(at_levels, as.numeric)
    }
  )

  stats::setNames(at_levels, at_terms)
}
