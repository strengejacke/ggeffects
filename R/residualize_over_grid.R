residualize_over_grid <- function(grid, model, ...) {
  UseMethod("residualize_over_grid")

}


#' @importFrom insight get_predictors link_function link_inverse
#' @importFrom stats residuals
residualize_over_grid.data.frame <- function(grid, model, pred_name, ...) {
  old_d <- insight::get_predictors(model)
  fun_link <- insight::link_function(model)
  inv_fun <- insight::link_inverse(model)
  predicted <- grid[[pred_name]]
  grid[[pred_name]] <- NULL

  is_fixed <- sapply(grid, function(x) length(unique(x))) == 1
  grid <- grid[,!is_fixed, drop = FALSE]
  old_d <- old_d[, colnames(grid)[colnames(grid) %in% colnames(old_d)], drop = FALSE]

  stopifnot("Grid must be a fully crossed grid." = .is_grid(grid))

  # for each var
  best_match <- NULL

  for (p in colnames(old_d)) {
    if (is.factor(old_d[[p]]) || is.logical(old_d[[p]]) || is.character(old_d[[p]])) {
      grid[[p]] <- as.character(grid[[p]])
      old_d[[p]] <- as.character(old_d[[p]])
    } else {
      grid[[p]] <- .validate_num(grid[[p]])
    }

    # if factor / logical / char in old data, find where it is equal
    # if numeric in old data, find where it is closest
    best_match <- .closest(old_d[[p]], grid[[p]], best_match = best_match)
  }

  idx <- apply(best_match, 2, which)

  res <- tryCatch(
    {
      stats::residuals(model, ...)
    },
    error = function(e) { NULL }
  )

  if (is.null(res)) {
    warning("Could not extract residuals.", call. = FALSE)
    return(NULL)
  }

  points <- grid[idx, , drop = FALSE]
  points[[pred_name]] <- inv_fun(fun_link(predicted[idx]) + res) # add errors

  points
}


residualize_over_grid.ggeffects <- function(grid, model, protect_gge_names = TRUE, ...) {
  new_d <- as.data.frame(grid)
  new_d <- new_d[colnames(new_d) %in% c("x", "group", "facet","panel", "predicted")]

  colnames(new_d)[colnames(new_d) %in% c("x", "group", "facet","panel")] <- attr(grid,"terms")

  points <- residualize_over_grid(new_d, model, pred_name = "predicted", ...)

  if (protect_gge_names) {
    colnames_gge <- c("x", "group", "facet","panel")
    colnames_orig <- attr(grid,"terms")
    for (i in seq_along(colnames_orig)) {
      colnames(points)[colnames(points) == colnames_orig[i]] <- colnames_gge[i]
    }
  }

  points
}



.is_grid <- function(df) {
  unq <- lapply(df, unique)

  if (prod(sapply(unq, length)) != nrow(df)) {
    return(FALSE)
  }

  df2 <- do.call(expand.grid, args = unq)
  df2$..1 <- 1

  res <- merge(df,df2, by = colnames(df), all = TRUE)

  return(sum(res$..1) == sum(df2$..1))
}



.closest <- function(x, target, best_match) {
  if (is.numeric(x)) {
    AD <- outer(x, target, FUN = function(x, y) abs(x - y))
    idx <- apply(AD, 1, function(x) x == min(x))
  } else {
    idx <- t(outer(x, target, FUN = `==`))
  }

  if (is.matrix(best_match)) {
    idx <- idx & best_match
  }

  idx
}



.validate_num <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(as.character(x))
  }
  x
}
