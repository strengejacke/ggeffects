# return levels, as list
# c("age", "edu [1,3]", "sex [2]") would return a list:
# $edu [1] 1 3; $sex [1] 2
#' @importFrom sjmisc is_empty trim str_contains is_num_fac
#' @importFrom purrr map possibly
#' @importFrom stats setNames sd
#' @importFrom sjlabelled as_numeric
.get_representative_values <- function(x, mf = NULL) {
  # get variable with suffix
  vars.pos <-
    which(as.vector(regexpr(
      pattern = "([^\\]]*)\\]",
      text = x,
      perl = T
    )) != -1)

  # is empty?
  if (sjmisc::is_empty(vars.pos)) return(NULL)

  # get variable names. needed later to set as
  # names attributes
  vars.names <- .get_cleaned_terms(x)[vars.pos]

  # get levels inside brackets
  tmp <- unlist(regmatches(
    x,
    gregexpr(
      # pattern = " ([^\\]]*)\\]",
      pattern = "\\[(.*)\\]",
      text = x,
      perl = T
    )
  ))

  # remove brackets
  tmp <- gsub("(\\[*)(\\]*)", "", tmp)

  # see if we have multiple values, split at comma
  tmp <- sjmisc::trim(strsplit(tmp, ",", fixed = T))

  # moderator pattern
  mp <- c("minmax", "meansd", "zeromax", "quart2", "all", "quart")

  # now check for ranges
  tmp <-
    purrr::map2(tmp, vars.names, function(x, y) {

      # Here we may have a range of values. we then create the
      # sequence with all values from this range

      if (sjmisc::str_contains(x, ":")) {

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

        steps <- as.numeric(sjmisc::trim(substring(gsub(" ", "", x), first = 3)))
        x <- pretty_range(mf[[y]], n = steps)

      } else if (length(x) == 1 && grepl("^sample(\\s*)=", x)) {

        size <- as.numeric(sjmisc::trim(substring(gsub(" ", "", x), first = 8)))
        lev <- stats::na.omit(unique(mf[[y]]))
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

        # else, we also may have a character expression. This may
        # either be the name of a valid function. In this case, we
        # transform the values for predictions using this function.
        # Else, it also might be the name of a value labels, so no
        # valid function name. In this case, simply return the label.

        if (x == "pretty") {
          x <- pretty_range(mf[[y]])
        } else if (x %in% mp) {
          x <- values_at(mf[[y]], values = x)
        } else {
          maf <- purrr::possibly(match.fun, NULL)
          funtrans <- maf(x)
          if (!is.null(funtrans) && !is.null(mf)) {
            x <- funtrans(sort(unique(mf[[y]])))
          }
        }
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
