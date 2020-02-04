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
      model_terms <- unlist(.compact_list(insight::find_terms(model)[c("conditional", "random", "instruments")]))
      if (model_terms[1] %in% c("0", "1")) {
        model_terms <- model_terms[-1]
      }
      model_terms
    },
    error = function(x) { NULL }
  )
}



.which_log_terms <- function(model) {
  form <- .get_pasted_formula(model)
  if (is.null(form)) return(NULL)
  log_terms <- form[grepl("log\\(([^,)]*).*", form)]
  if (length(log_terms) > 0) {
    log_terms <- insight::clean_names(log_terms)
  } else {
    log_terms <- NULL
  }
  log_terms
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
