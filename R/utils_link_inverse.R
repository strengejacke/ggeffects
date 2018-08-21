#' @importFrom stats family
get_link_fun <- function(model) {
  tryCatch(
    {
      # get model family
      ff <- stats::family(model)

      # return link function, if exists
      if ("linkfun" %in% names(ff)) return(ff$linkfun)

      # else, create link function from link-string
      if ("link" %in% names(ff)) return(match.fun(ff$link))
    },
    error = function(x) { NULL },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )
}


#' @importFrom stats terms weights
get_model_weights <- function(model) {
  w <- NULL
  tryCatch(
    {
      w <- stats::weights(model)
    },
    error = function(x) { NULL },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )

  w
}
