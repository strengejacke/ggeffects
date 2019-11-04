# get color palette
.get_colors <- function(geom.colors, collen) {
  # check for corrct color argument
  if (!is.null(geom.colors)) {
    # check for color brewer palette
    if (geom.colors[1] %in% names(ggeffects_colors)) {
      geom.colors <- ggeffects_pal(palette = geom.colors[1], n = collen)
    } else if (geom.colors[1] == "bw") {
      geom.colors <- rep("black", times = collen)
    } else if (geom.colors[1] == "gs") {
      geom.colors <- ggeffects_pal(palette = "greyscale", n = collen)
      # do we have correct amount of colours?
    } else if (length(geom.colors) > collen) {
      # shorten palette
      geom.colors <- geom.colors[1:collen]
    } else if (length(geom.colors) < collen) {
      # warn user abount wrong color palette
      warning(sprintf("Insufficient length of color palette provided. %i color values needed.", collen), call. = F)
      # set default palette
      geom.colors <- ggeffects_pal(palette = "Set1", n = collen)
    }
  } else {
    geom.colors <- ggeffects_pal(palette = "Set1", n = collen)
  }

  geom.colors
}
