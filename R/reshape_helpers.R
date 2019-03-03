#' @keywords internal
.nest <- function(x, cn = "data") {
  if (!inherits(x, "grouped_df"))
    return(x)

  # get group indices and group keys from grouped df
  g <- .group_indices(x)
  k <- .group_keys(x)

  # create a factor with group indices, for "split()"
  f <- vector(mode = "numeric", length = nrow(x))
  for (i in 1:length(g)) {
    f[g[[i]]] <- i
  }

  # remove grouping variables (keys) from data frame
  # because these should not be nested
  data_to_group <- x[, setdiff(colnames(x), colnames(k)), drop = FALSE]

  # split data, and add create a data frame with list-variable
  l <- split(data_to_group, f)
  dat <- data.frame(data = I(l))
  colnames(dat) <- cn

  # bind keys and nested data frames
  nested_df <- cbind(k, dat)

  attr(nested_df, "groups") <- f
  attr(nested_df, "indices") <- unlist(g)

  nested_df
}


#' @keywords internal
.unnest <- function(x, cn = NULL, more_list_cols = NULL) {
  # get name of data column
  if (is.null(cn))
    cn <- colnames(x)[ncol(x)]

  # iterate all rows, i.e. all nested data frames
  # and add values from key-variables as variables,
  # so the key variables are also present in the final,
  # unnested data frame
  keys <- x[, setdiff(colnames(x), c(cn, more_list_cols)), drop = FALSE]
  for (i in 1:nrow(x)) {
    for (j in 1:length(keys)) {
      x[[cn]][[i]][[colnames(keys)[j]]] <- keys[i, j]
    }
    if (!is.null(more_list_cols))
      x[[cn]][[i]][[more_list_cols]] <- x[[more_list_cols]][[i]]
  }

  # bind all data frames, and restore original order
  unnested_df <- do.call(rbind, x[[cn]])

  rows <- attr(x, "indices", exact = TRUE)
  if (is.null(rows)) rows <- 1:nrow(unnested_df)

  unnested_df[order(rows), c(colnames(keys), setdiff(colnames(unnested_df), colnames(keys)))]
}


#' @keywords internal
.group_indices <- function(x) {
  # dplyr >= 0.8.0 returns attribute "indices"
  grps <- attr(x, "groups", exact = TRUE)

  # dplyr < 0.8.0?
  if (is.null(grps)) {
    grps <- attr(x, "indices", exact = TRUE)
  } else {
    grps <- grps[[".rows"]]
  }

  grps
}


#' @keywords internal
.group_keys <- function(x) {
  # dplyr >= 0.8.0 returns attribute "indices"
  grps <- attr(x, "groups", exact = TRUE)

  # dplyr < 0.8.0?
  if (is.null(grps)) {
    ## TODO fix for dplyr < 0.8
    keys <- attr(x, "indices", exact = TRUE)
  } else {
    keys <- grps[, setdiff(colnames(grps), ".rows")]
  }

  keys
}
