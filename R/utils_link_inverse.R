#' @importFrom stats family
get_link_fun <- function(model) {
  # get model family
  ff <- stats::family(model)

  # return link function, if exists
  if ("linkfun" %in% names(ff)) return(ff$linkfun)

  # else, create link function from link-string
  if ("link" %in% names(ff)) return(match.fun(ff$link))

  NULL
}
