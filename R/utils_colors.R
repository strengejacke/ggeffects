# get color palette
#' @importFrom scales brewer_pal grey_pal
.get_colors <- function(geom.colors, collen) {
  # check for corrct color argument
  if (!is.null(geom.colors)) {
    # check for color brewer palette
    if (is.brewer.pal(geom.colors[1]) && collen == 1) {
      geom.colors <- "black"
    } else if (is.brewer.pal(geom.colors[1])) {
      geom.colors <- scales::brewer_pal(palette = geom.colors[1])(collen)
    } else if (geom.colors[1] %in% names(ggeffects_colors)) {
      geom.colors <- ggeffects_pal(palette = geom.colors[1], n = collen)
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
