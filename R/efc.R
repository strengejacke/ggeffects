#' @docType data
#' @title Sample dataset from the EUROFAMCARE project
#' @name efc
#' @keywords data
#'
#' @description A SPSS sample data set, imported with the \code{\link[sjmisc]{read_spss}} function.
#'
#' @examples
#' # Attach EFC-data
#' data(efc)
#'
#' # Show structure
#' str(efc)
#'
#' # show first rows
#' head(efc)
#'
#' # show variables
#' \dontrun{
#' library(sjmisc)
#' library(sjPlot)
#' view_df(efc)
#'
#' # show variable labels
#' get_label(efc)
#'
#' # plot efc-data frame summary
#' sjt.df(efc, alternateRowColor = TRUE)}
#'
NULL

