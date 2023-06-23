#' @keywords internal
.gather <- function(x, names_to = "key", values_to = "value", columns = colnames(x)) {
  if (is.numeric(columns)) columns <- colnames(x)[columns]
  dat <- stats::reshape(
    x,
    idvar = "id",
    ids = row.names(x),
    times = columns,
    timevar = names_to,
    v.names = values_to,
    varying = list(columns),
    direction = "long"
  )

  if (is.factor(dat[[values_to]])) {
    dat[[values_to]] <- as.character(dat[[values_to]])
  }

  dat[, 1:(ncol(dat) - 1), drop = FALSE]
}



#' @keywords internal
.multiple_gather <- function(x,
                             names_to = "key",
                             values_to = "value",
                             columns = colnames(x),
                             numeric_timvar = FALSE,
                             id = "id") {

  ## TODO make timevar numeric?

  variable_attr <- lapply(x, attributes)

  if (is.numeric(columns)) columns <- colnames(x)[columns]
  if (!is.list(columns)) columns <- list(columns)

  dat <- stats::reshape(
    x,
    idvar = id,
    times = columns[[1]],
    timevar = names_to,
    v.names = values_to,
    varying = columns,
    direction = "long"
  )

  if (numeric_timvar) {
    f <- as.factor(dat[[names_to]])
    levels(f) <- 1:nlevels(f)
    dat[[names_to]] <- as.numeric(as.character(f))
  }

  for (i in colnames(dat)) {
    attributes(dat[[i]]) <- variable_attr[[i]]
  }

  dat[[id]] <- NULL
  rownames(dat) <- NULL

  dat
}


.var_rename <- function(x, ...) {
  .dots <- unlist(match.call(expand.dots = FALSE)$...)
  old_names <- names(.dots)
  new_names <- unname(.dots)

  non.match <- which(!(old_names %in% colnames(x)))
  if (length(non.match)) {
    # remove invalid names
    old_names <- old_names[-non.match]
    new_names <- new_names[-non.match]
  }

  name_pos <- match(old_names, colnames(x))
  colnames(x)[name_pos] <- new_names
  x
}


.round_numeric <- function(x, digits = 2) {
  x[] <- lapply(x, function(.i) {
    if (is.numeric(.i)) round(.i, digits = digits) else .i
  })
  x
}
