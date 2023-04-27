# return levels, as list
# c("age", "edu [1,3]", "sex [2]") would return a list:
# $edu [1] 1 3; $sex [1] 2
.get_representative_values <- function(x, model_frame = NULL) {
  # get variable with suffix
  terms_with_suffix <- which(as.vector(regexpr(pattern = "([^\\]]*)\\]", text = x, perl = TRUE)) != -1)

  # is empty?
  if (.is_empty(terms_with_suffix)) return(NULL)

  # get variable names. needed later to set as
  # names attributes
  at_terms <- .clean_terms(x)[terms_with_suffix]

  # get levels inside brackets
  at_levels <- unlist(regmatches(x, gregexpr(pattern = "\\[(.*)\\]", text = x, perl = TRUE)))

  # remove brackets
  at_levels <- gsub("(\\[*)(\\]*)", "", at_levels)

  # see if we have multiple values, split at comma
  at_levels <- lapply(strsplit(at_levels, ",", fixed = TRUE), trimws)

  # moderator pattern
  at_pattern <- c("minmax", "meansd", "zeromax", "quart2", "all", "quart",
                  "fivenum", "terciles", "terciles2", "quartiles", "quartiles2")

  # now check for ranges
  at_levels <- Map(function(x, y) {

    # Here we may have a range of values. we then create the
    # sequence with all values from this range

    if (any(grepl(":", x, fixed = TRUE))) {

      # values at sequence (from to) ------------------------------------------

      from_to_by <- s <- unlist(lapply(strsplit(x, ":", fixed = TRUE), trimws))
      if (grepl("by", s[2], fixed = TRUE)) {
        from_to_by[2] <- sub("(.*)(\\s*)by(\\s*)=(.*)", "\\1", x = s[2])
        from_to_by[3] <- sub("(.*)(\\s*)by(\\s*)=(.*)", "\\4", x = s[2])
      } else if (grepl("by", s[3], fixed = TRUE)) {
        from_to_by[3] <- sub("by(\\s*)=(.*)", "\\2", x = s[3])
      } else {
        from_to_by[3] <- "1"
      }

      from_to_by <- as.numeric(from_to_by)
      x <- seq(from = from_to_by[1], to = from_to_by[2], by = from_to_by[3])

    } else if (length(x) == 1 && grepl("^n(\\s*)=", x)) {

      # values at pretty range -----------------------------------------------

      steps <- as.numeric(trimws(substring(gsub(" ", "", x, fixed = TRUE), first = 3)))
      x <- pretty_range(model_frame[[y]], n = steps)

    } else if (length(x) == 1 && grepl("^sample(\\s*)=", x)) {

      # values at random samples ---------------------------------------------

      size <- as.numeric(trimws(substring(gsub(" ", "", x, fixed = TRUE), first = 8)))
      lev <- stats::na.omit(unique(model_frame[[y]]))
      pos <- sample.int(n = length(lev), size = size, replace = FALSE)
      x <- lev[pos]

      if (is.factor(x)) {
        if (.is_numeric_factor(x)) {
          x <- .factor_to_numeric(droplevels(x))
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
        # return pretty range
        x <- pretty_range(model_frame[[y]])
      } else if (x %in% at_pattern) {
        # return at specific values
        x <- values_at(model_frame[[y]], values = x)
      } else {

        # transform values by function
        funtrans <- try(match.fun(x), silent = TRUE)
        if (!inherits(funtrans, "try-error") && !is.null(model_frame)) {
          x <- funtrans(sort(unique(model_frame[[y]])))

          # is x a value from vector?
        } else if (x %in% unique(model_frame[[y]])) {
          x

          # return values of a vector
        } else {
          out <- .safe(get(x, envir = parent.frame()))
          if (is.null(out)) {
            out <- .safe(get(x, envir = globalenv()))
          }
          if (is.null(out)) {
            out <- .safe(dynGet(x, ifnotfound = NULL))
          }
          x <- out
        }
      }
    }

    x
  }, at_levels, at_terms)

  # check if levels were numeric or not...
  suppressWarnings(
    if (!anyNA(unlist(lapply(at_levels, as.numeric)))) {
      at_levels <- lapply(at_levels, as.numeric)
    }
  )

  stats::setNames(at_levels, at_terms)
}
