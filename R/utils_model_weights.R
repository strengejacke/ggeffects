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
