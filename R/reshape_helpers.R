#' @importFrom stats reshape
#' @keywords internal
.gather <- function(x, key = "key", value = "value", columns = colnames(x)) {
  if (is.numeric(columns)) columns <- colnames(x)[columns]
  dat <- stats::reshape(
    x,
    idvar = "id",
    ids = row.names(x),
    times = columns,
    timevar = key,
    v.names = value,
    varying = list(columns),
    direction = "long"
  )

  if (is.factor(dat[[value]]))
    dat[[value]] <- as.character(dat[[value]])

  dat[, 1:(ncol(dat) - 1), drop = FALSE]
}
