# =============================================================================
# Root-crown height of Rhizophora mangle, for chamber heights measured from the
# root crown. Used by 05_dataset/01_compile_datasets.R.
#
# In March and October 2022, R. mangle stems were chambered at nominal 0, 50 and
# 100 cm, and prop roots at -25 or -50 cm, from the root crown (where the prop
# roots leave the trunk): a trunk cannot be chambered at 0 cm above the
# sediment, and roots were measured down from the same zero. Height above the
# sediment = crown height + recorded height.
#
# Crown height per site (cm), HEIGHT_CROWN =
#   field (default): median over R. mangle trees of the lowest trunk chamber in
#     March 2023, when heights were measured from the sediment and the lowest
#     chamber sat just above the prop roots. Trees are consecutive closures of
#     increasing height on one date. FLM30 (no live R. mangle in March 2023):
#     pooled SRS5 + SRS6 trees.
#   tls: mean height of the TLS prop-root zone at SRS5 and SRS6, from the root
#     surface area by 0.5 m bin as a survival curve (share of root area at or
#     above each bin = share of trees whose crown is higher); BL60 and FLM30
#     as for field.
#   none: recorded heights taken at face value (as labelled).
# =============================================================================
crown_heights <- function(d, project_dir = ".") {
  mode <- Sys.getenv("HEIGHT_CROWN", "field")
  x <- d[d$component == "stem" & d$species %in% "RHMA" & d$month_year == "2023-03" & !is.na(d$height) &
           d$above %in% "sediment", ]
  x <- x[order(x$plot, x$date, x$start_time), ]
  new_tree <- c(TRUE, diff(x$height) <= 0 | x$plot[-1] != x$plot[-nrow(x)] | x$date[-1] != x$date[-nrow(x)])
  x$tree <- cumsum(new_tree)
  lo <- aggregate(height ~ plot + tree, x, min)
  field <- tapply(lo$height, lo$plot, median)
  pooled <- median(lo$height[lo$plot %in% c("SRS5", "SRS6")])
  out <- c(SRS5 = field[["SRS5"]], SRS6 = field[["SRS6"]], BL60 = field[["BL60"]], FLM30 = pooled)
  if (mode == "tls") {
    for (s in c("SRS5", "SRS6")) {
      r <- read.csv(file.path(project_dir, "data", "tls", paste0(s, "_plot_component_summary.csv")))
      r <- r[r$segment_class == "root", ]; r <- r[order(r$height_bin_num), ]
      out[[s]] <- 100 * sum(0.5 * r$Total_surface_area_m2 / r$Total_surface_area_m2[1])
    }
    out[["FLM30"]] <- mean(out[c("SRS5", "SRS6")])
  }
  if (mode == "none") out[] <- NA
  attr(out, "mode") <- mode
  out
}
