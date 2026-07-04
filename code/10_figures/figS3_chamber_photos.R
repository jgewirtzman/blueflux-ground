#!/usr/bin/env Rscript
# =============================================================================
# Figure S3: Chamber type photographs
# Output: pub_SI_chamber_photos
# =============================================================================
# To interactively adjust crop regions, run crop_photos_interactive() in RStudio.
# It will display each photo and let you click two corners to define the crop.
# Crop offsets are saved and reused on subsequent runs.

library(cowplot)
library(magick)
library(ggplot2)

photo_dir <- "output/figures/other/photos"
crop_file <- "output/figures/other/photos/crop_offsets.csv"

photos <- list(
  list(file = "soil_chamber.jpg",                     label = "(a) Soil cylinder"),
  list(file = "soil_chamber_with_pneumatophores.jpg", label = "(b) Soil w/ pneumatophores"),
  list(file = "soil_collar_chamber.jpg",              label = "(c) Soil collar + dome"),
  list(file = "floating_chamber.jpg",                 label = "(d) Floating water"),
  list(file = "root_chamber.jpg",                     label = "(e) Prop root"),
  list(file = "stem_chamber.jpg",                     label = "(f) Stem"),
  list(file = "cwd_chamber.jpg",                      label = "(g) Coarse woody debris"),
  list(file = "leaf_chamber.jpg",                     label = "(h) Leaf")
)

target_ratio <- 4 / 3

# ---- Interactive crop tool (run in RStudio) ----------------------------------

crop_photos_interactive <- function() {
  offsets <- data.frame(file = character(), x_off = numeric(), y_off = numeric(),
                        stringsAsFactors = FALSE)

  for (p in photos) {
    img <- image_read(file.path(photo_dir, p$file))
    info <- image_info(img)

    # Show photo in plot window
    rast <- as.raster(img)
    plot(0, 0, type = "n", xlim = c(0, info$width), ylim = c(info$height, 0),
         asp = 1, xlab = "", ylab = "", main = paste(p$label, "-", p$file))
    rasterImage(rast, 0, info$height, info$width, 0)

    # Draw target crop rectangle at current center
    cr <- info$width / info$height
    if (cr > target_ratio) {
      crop_w <- round(info$height * target_ratio)
      crop_h <- info$height
    } else {
      crop_w <- info$width
      crop_h <- round(info$width / target_ratio)
    }

    # Default center crop
    def_x <- round((info$width - crop_w) / 2)
    def_y <- round((info$height - crop_h) / 2)
    rect(def_x, def_y, def_x + crop_w, def_y + crop_h, border = "red", lwd = 2)

    cat("Click the CENTER of where you want the crop (or right-click/Esc to keep default)\n")
    pt <- locator(1)

    if (is.null(pt)) {
      x_off <- def_x
      y_off <- def_y
    } else {
      x_off <- max(0, min(round(pt$x - crop_w / 2), info$width - crop_w))
      y_off <- max(0, min(round(pt$y - crop_h / 2), info$height - crop_h))
    }

    offsets <- rbind(offsets, data.frame(file = p$file, x_off = x_off, y_off = y_off))
    cat(sprintf("  %s: offset x=%d y=%d\n", p$file, x_off, y_off))
  }

  write.csv(offsets, crop_file, row.names = FALSE)
  cat("Saved crop offsets to:", crop_file, "\n")
  offsets
}

# ---- Build figure ------------------------------------------------------------

# Load saved crop offsets if available, otherwise use center crop
if (file.exists(crop_file)) {
  offsets <- read.csv(crop_file, stringsAsFactors = FALSE)
  cat("Using saved crop offsets from:", crop_file, "\n")
} else {
  offsets <- NULL
  cat("No saved crop offsets — using center crop. Run crop_photos_interactive() to adjust.\n")
}

make_photo <- function(p) {
  img <- image_read(file.path(photo_dir, p$file))
  info <- image_info(img)

  cr <- info$width / info$height
  if (cr > target_ratio) {
    crop_w <- round(info$height * target_ratio)
    crop_h <- info$height
  } else {
    crop_w <- info$width
    crop_h <- round(info$width / target_ratio)
  }

  # Get offset
  if (!is.null(offsets) && p$file %in% offsets$file) {
    row <- offsets[offsets$file == p$file, ]
    x_off <- row$x_off
    y_off <- row$y_off
  } else {
    x_off <- round((info$width - crop_w) / 2)
    y_off <- round((info$height - crop_h) / 2)
  }

  img <- image_crop(img, paste0(crop_w, "x", crop_h, "+", x_off, "+", y_off))
  ggdraw() + draw_image(img)
}

make_cap <- function(label) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = label, size = 3.5, fontface = "bold") +
    theme_void()
}

# Build rows: photo row then caption row
rows <- list()
for (i in seq(1, length(photos), by = 2)) {
  photo_row <- plot_grid(make_photo(photos[[i]]), make_photo(photos[[i + 1]]), ncol = 2)
  cap_row <- plot_grid(make_cap(photos[[i]]$label), make_cap(photos[[i + 1]]$label), ncol = 2)
  rows <- c(rows, list(photo_row), list(cap_row))
}

combined <- plot_grid(plotlist = rows, ncol = 1,
                      rel_heights = rep(c(1, 0.08), length(photos) / 2))

save_pub <- function(plot, name, width, height, units = "mm") {
  ggsave(paste0("output/figures/other/pub_", name, ".pdf"), plot,
         width = width, height = height, units = units)
  ggsave(paste0("output/figures/other/pub_", name, ".png"), plot,
         width = width, height = height, units = units, dpi = 300)
  cat(sprintf("Saved: pub_%s.pdf/.png\n", name))
}

save_pub(combined, "SI_chamber_photos", width = 190, height = 270)

# Run figure cleanup if available
cleanup_script <- "code/10_figures/figure_cleanup.R"
if (file.exists(cleanup_script)) source(cleanup_script)
