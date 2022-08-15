# get color palette
.get_colors <- function(geom.colors, collen, continuous) {
  # check for corrct color argument
  if (!is.null(geom.colors)) {
    geom.colors <- tolower(geom.colors)
    # check for color brewer palette
    if (geom.colors[1] %in% names(ggeffects_colors)) {
      geom.colors <- ggeffects_pal(palette = geom.colors[1], n = collen)
    } else if (geom.colors[1] == "bw") {
      geom.colors <- rep("black", times = collen)
    } else if (geom.colors[1] == "gs") {
      geom.colors <- ggeffects_pal(palette = "greyscale", n = collen)
      # do we have correct amount of colours?
    } else if (length(geom.colors) > 1 && continuous) {
      # preserve colors as is for latter use in gradient scale
      return(geom.colors)
    } else if (length(geom.colors) > collen) {
      # shorten palette
      geom.colors <- geom.colors[1:collen]
    } else if (length(geom.colors) < collen) {
      # warn user abount wrong color palette
      warning(sprintf("Insufficient length of color palette provided. %i color values needed.", collen), call. = FALSE)
      # set default palette
      geom.colors <- ggeffects_pal(palette = "set1", n = collen)
    }
  } else {
    geom.colors <- ggeffects_pal(palette = "set1", n = collen)
  }

  geom.colors
}
