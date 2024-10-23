#' @rdname plot
#' @export
theme_ggeffects <- function(base_size = 11, base_family = "") {
  insight::check_if_installed("ggplot2")

  (ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
     ggplot2::theme(
       axis.line.x      = ggplot2::element_line(colour = "grey80"),
       axis.line.y      = ggplot2::element_line(colour = "grey80"),
       axis.text        = ggplot2::element_text(colour = "grey50"),
       axis.title       = ggplot2::element_text(colour = "grey30"),
       strip.background = ggplot2::element_rect(colour = "grey70", fill = "grey90"),
       strip.text       = ggplot2::element_text(colour = "grey30"),
       legend.title     = ggplot2::element_text(colour = "grey30"),
       legend.text      = ggplot2::element_text(colour = "grey30")
     ))
}


#' @rdname plot
#' @export
ggeffects_palette <- function(palette = "metro", n = NULL) {
  if (is.null(palette)) {
    return(ggeffects_colors)
  }

  pl <- ggeffects_colors[[palette]]

  if (!is.null(n) && n <= length(pl)) {
    if (.is_continuous_palette(palette)) {
      pl <- pl[stats::quantile(seq_along(pl), probs = seq(0, 1, length.out = n))]
    } else {
      pl <- pl[1:n]
    }
  }

  pl
}


#' @rdname plot
#' @export
show_palettes <- function() {
  insight::check_if_installed("ggplot2")
  .data <- NULL

  longest.pal <- max(lengths(ggeffects_colors))

  color_pal <- lapply(ggeffects_colors, function(.x) {
    if (length(.x) == longest.pal) {
      .x
    } else {
      c(.x, rep("#ffffff", times = longest.pal - length(.x)))
    }
  })

  x <- as.data.frame(color_pal, check.names = FALSE)
  x <- .gather(x[rev(seq_len(nrow(x))), , drop = FALSE])
  x <- x[order(x$key), , drop = FALSE]

  x$y <- rep_len(1:longest.pal, nrow(x))
  x$cols <- as.factor(seq_len(nrow(x)))

  x$key <- factor(x$key, levels = rev(unique(x$key)))

  x$group <- "Other Palettes"
  x$group[.is_continuous_palette(x$key)] <- "Continuous Palettes"
  x$group[.is_rgb_palette(x$key)] <- "Red-Blue-Green Palettes"

  ggplot2::ggplot(x, ggplot2::aes(x = .data[["key"]], fill = .data[["cols"]])) +
    ggplot2::geom_bar(width = 0.7) +
    ggplot2::scale_fill_manual(values = x$value) +
    ggplot2::scale_y_continuous(breaks = NULL, labels = NULL) +
    ggplot2::guides(fill = "none") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::facet_wrap(~group, ncol = 1, scales = "free")
}


# color palettes ------------------------

ggeffects_colors <- list(
  aqua = c("#BAF5F3", "#46A9BE", "#8B7B88", "#BD7688", "#F2C29E"),
  warm = c("#072835", "#664458", "#C45B46", "#F1B749", "#F8EB85"),
  dust = c("#232126", "#7B5756", "#F7B98B", "#F8F7CF", "#AAAE9D"),
  blambus = c("#E02E1F", "#5D8191", "#BD772D", "#494949", "#F2DD26"),
  simply = c("#CD423F", "#0171D3", "#018F77", "#FCDA3B", "#F5C6AC"),
  us = c("#004D80", "#376C8E", "#37848E", "#9BC2B6", "#B5D2C0"),
  reefs = c("#43a9b6", "#218282", "#dbdcd1", "#44515c", "#517784"),
  `breakfast club` = c("#b6411a", "#4182dd", "#2d6328", "#eec3d8", "#ecf0c8"), # nolint
  metro = c("#d11141", "#00aedb", "#00b159", "#f37735", "#8c8c8c", "#ffc425", "#cccccc"), # nolint
  viridis = c("#440154", "#46337E", "#365C8D", "#277F8E", "#1FA187", "#4AC16D", "#9FDA3A", "#FDE725"), # nolint
  ipsum = c("#3f2d54", "#75b8d1", "#2d543d", "#d18975", "#8fd175", "#d175b8", "#758bd1", "#d1ab75", "#c9d175"), # nolint
  quadro = c("#ff0000", "#1f3c88", "#23a393", "#f79f24", "#625757"),
  eight = c("#003f5c", "#2f4b7c", "#665191", "#a05195", "#d45087", "#f95d6a", "#ff7c43", "#ffa600"), # nolint
  circus = c("#C1241E", "#0664C9", "#EBD90A", "#6F130D", "#111A79"),
  system = c("#0F2838", "#F96207", "#0DB0F3", "#04EC04", "#FCC44C"),
  hero = c("#D2292B", "#165E88", "#E0BD1C", "#D57028", "#A5CB39", "#8D8F70"), # nolint
  flat = c("#c0392b", "#2980b9", "#16a085", "#f39c12", "#8e44ad", "#7f8c8d", "#d35400"), # nolint
  social = c("#b92b27", "#0077B5", "#00b489", "#f57d00", "#410093", "#21759b", "#ff3300"), # nolint
  set1 = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"), # nolint
  greyscale = c("#333333", "#5A5A5A", "#737373", "#878787", "#989898", "#A7A7A7", "#B4B4B4", "#C1C1C1", "#CCCCCC"), # nolint
  `okabe-ito` = c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#999999", "#000000", "#0072B2", "#D55E00") # nolint
)

# palettes with a continuous color scale
.is_continuous_palette <- function(p) {
  p %in% c("aqua", "dust", "eight", "greyscale", "us", "viridis", "warm")
}

.is_rgb_palette <- function(p) {
  p %in% c("breakfast club", "flat", "metro", "quadro", "set1", "simply", "social")
}
