.has_splines <- function(model) {
  form <- .get_pasted_formula(model)
  if (is.null(form)) return(FALSE)

  any(
    grepl("s\\(([^,)]*)", form) | grepl("bs\\(([^,)]*)", form) |
      grepl("ns\\(([^,)]*)", form) | grepl("pspline\\(([^,)]*)", form) |
      grepl("poly\\(([^,)]*)", form)
  )
}



.has_poly <- function(model) {
  form <- .get_pasted_formula(model)
  if (is.null(form)) return(FALSE)
  any(grepl("I\\(.*?\\^.*?\\)", form) | grepl("poly\\(([^,)]*)", form))
}



.has_log <- function(model) {
  any(.get_log_terms(model))
}



.get_log_terms <- function(model) {
  form <- .get_pasted_formula(model)
  if (is.null(form)) return(FALSE)
  grepl("log\\(([^,)]*).*", form)
}


#' @importFrom insight find_terms
.get_pasted_formula <- function(model) {
  tryCatch(
    {
      unlist(.compact_list(insight::find_terms(model)[c("conditional", "random", "instruments")]))
    },
    error = function(x) { NULL }
  )
}



.has_poly_term <- function(x) {
  any(grepl("poly\\(([^,)]*)", x))
}



.uses_all_tag <- function(terms) {
  tags <- unlist(regmatches(
    terms,
    gregexpr(
      pattern = "\\[(.*)\\]",
      text = terms,
      perl = T
    )
  ))

  "[all]" %in% tags
}
