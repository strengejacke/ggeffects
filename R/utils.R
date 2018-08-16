#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


# get color palette
#' @importFrom scales brewer_pal grey_pal
get_colors <- function(geom.colors, collen) {
  # check for corrct color argument
  if (!is.null(geom.colors)) {
    # check for color brewer palette
    if (is.brewer.pal(geom.colors[1]) && collen == 1) {
      geom.colors <- "black"
    } else if (is.brewer.pal(geom.colors[1])) {
      geom.colors <- scales::brewer_pal(palette = geom.colors[1])(collen)
    } else if (geom.colors[1] == "gs") {
      geom.colors <- scales::grey_pal()(collen)
      # do we have correct amount of colours?
    } else if (geom.colors[1] == "bw") {
      geom.colors <- rep("black", times = collen)
      # do we have correct amount of colours?
    } else if (length(geom.colors) > collen) {
      # shorten palette
      geom.colors <- geom.colors[1:collen]
    } else if (length(geom.colors) < collen) {
      # warn user abount wrong color palette
      warning(sprintf("Insufficient length of color palette provided. %i color values needed.", collen), call. = F)
      # set default palette
      geom.colors <- scales::brewer_pal(palette = "Set1")(collen)
    }
  } else {
    geom.colors <- scales::brewer_pal(palette = "Set1")(collen)
  }

  geom.colors
}


# check whether a color value is indicating
# a color brewer palette
is.brewer.pal <- function(pal) {
  bp.seq <- c("BuGn", "BuPu", "GnBu", "OrRd", "PuBu", "PuBuGn", "PuRd", "RdPu",
              "YlGn", "YlGnBu", "YlOrBr", "YlOrRd", "Blues", "Greens", "Greys",
              "Oranges", "Purples", "Reds")

  bp.div <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu",
              "RdYlGn", "Spectral")

  bp.qul <- c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
              "Set2", "Set3")

  bp <- c(bp.seq, bp.div, bp.qul)

  any(bp == pal)
}


check_vars <- function(terms) {
  if (missing(terms) || is.null(terms)) {
    stop("`terms` needs to be a character vector with at least one predictor names: one term used for the x-axis, more optional terms as grouping factors.", call. = F)
  }

  # check for correct length of vector
  if (length(terms) > 3) {
    message("`terms` must have not more than three values. Using first three values now.")
    terms <- terms[1:3]
  }

  terms
}


#' @importFrom sjstats resp_val resp_var
#' @importFrom dplyr filter
#' @importFrom stats complete.cases
#' @importFrom sjlabelled as_label as_numeric
get_raw_data <- function(model, mf, terms) {
  # for matrix variables, don't return raw data
  if (any(purrr::map_lgl(mf, is.matrix)))
    return(NULL)

  # remove missings from model frame
  mf <- dplyr::filter(mf, stats::complete.cases(mf))

  if (!all(sjstats::resp_var(model) %in% colnames(mf)))
    return(NULL)

  # get response and x-value
  response <- sjstats::resp_val(model)
  x <- sjlabelled::as_numeric(mf[[terms[1]]])

  # for cox-models, modify response
  if (inherits(model, "coxph")) {
    lr <- length(response)
    response <- response[((lr / 2) + 1):lr]
  }

  # add optional grouping variable
  if (length(terms) > 1) {
    group <-
      sjlabelled::as_label(
        mf[[terms[2]]],
        prefix = FALSE,
        drop.na = TRUE,
        drop.levels = !is.numeric(mf[[terms[2]]])
      )
  } else {
    group <- sjmisc::to_factor(1)
  }

  # return all as data.frame
  tryCatch(
    {
      data.frame(response = response, x = x, group = group, stringsAsFactors = FALSE)
    },
    error = function(x) { NULL },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )
}


#' @importFrom purrr map
#' @importFrom dplyr n_distinct
prettify_data <- function(xl.remain, fitfram, terms) {
  purrr::map(xl.remain, function(.x) {
    pr <- fitfram[[terms[.x]]]
    if (is.numeric(pr)) {
      if (.x > 1 && dplyr::n_distinct(pr, na.rm = TRUE) >= 10)
        rprs_values(pr)
      else if (dplyr::n_distinct(pr, na.rm = TRUE) < 9)
        na.omit(unique(pr))
      else
        pretty_range(pr)
    } else if (is.factor(pr))
      levels(pr)
    else
      na.omit(unique(pr))
  })
}
