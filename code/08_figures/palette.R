# =============================================================================
# Shared figure palette and theme (adopted 2026-10-02). Sourced by figure scripts.
# Forest classes: canopy green -> gold (regrowth) -> slate (dead wood).
# Components: the colour of the thing; lightness alternates so stacked
# segments separate in greyscale. Both palettes checked for deuteranopia,
# protanopia and tritanopia (min CIEDE2000 >= 10 between classes; components
# lowest pair 11, never adjacent).
# =============================================================================
pal_class <- c(intact = "#1E6B4E", regenerating = "#D9A441", ghost = "#6E6A86")
pal_class_data <- c(healthy = "#1E6B4E", regenerating = "#D9A441", ghost = "#6E6A86")   # dataset labels
pal_comp <- c(water = "#2C7BB6", soil = "#6B4226", `prop root` = "#E07B39", stem = "#EAD7A6",
              `downed wood` = "#A7A9AC", leaf = "#2D6A4F")
pal_comp_data <- c(water = "#2C7BB6", soil = "#6B4226", root = "#E07B39", stem = "#EAD7A6",
                   cwd = "#A7A9AC", leaves = "#2D6A4F")                                 # dataset labels
col_ink <- "#222222"; col_waterline <- "#DCEAF5"; col_missing <- "#BDBDBD"
class_labels <- c(healthy = "intact", regenerating = "regenerating", ghost = "ghost")

theme_fig <- function(base_size = 8) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(colour = "grey92", linewidth = 0.3),
                   axis.line = ggplot2::element_line(colour = "grey40", linewidth = 0.3),
                   axis.ticks = ggplot2::element_line(colour = "grey40", linewidth = 0.3),
                   axis.text = ggplot2::element_text(colour = "grey25"),
                   strip.text = ggplot2::element_text(face = "bold", hjust = 0),
                   legend.position = "bottom", legend.key.size = ggplot2::unit(8, "pt"),
                   plot.tag = ggplot2::element_text(face = "bold", size = base_size + 3))
}
# asinh flux axis shared by all flux panels
asinh_axis <- function(breaks = c(-10, -1, 0, 1, 10, 100, 1000), ...)
  ggplot2::scale_x_continuous(breaks = asinh(breaks), labels = breaks, ...)
