# =============================================================================
# Root-crown height of Rhizophora mangle, for chamber heights measured from the
# root crown. Used by 05_dataset/01_compile_datasets.R.
#
# In March and October 2022, R. mangle stems were chambered at nominal 0, 50 and
# 100 cm, and prop roots at -25 or -50 cm, from the root crown (where the prop
# roots leave the trunk). Height above the sediment = crown height + recorded
# height. Crown height per site (cm): median over R. mangle trees of the lowest
# trunk chamber in March 2023, when heights were measured from the sediment and
# the lowest chamber sat just above the prop roots (trees = consecutive
# closures of increasing height on one date). FLM30 (no live R. mangle in March
# 2023): pooled SRS5 + SRS6 trees. The TLS prop-root profile gives similar
# values (SRS5 75 cm, SRS6 86 cm).
# =============================================================================
crown_heights <- function(d) {
  x <- d[d$component == "stem" & d$species %in% "RHMA" & d$month_year == "2023-03" & !is.na(d$height) &
           d$above %in% "sediment", ]
  x <- x[order(x$plot, x$date, x$start_time), ]
  new_tree <- c(TRUE, diff(x$height) <= 0 | x$plot[-1] != x$plot[-nrow(x)] | x$date[-1] != x$date[-nrow(x)])
  x$tree <- cumsum(new_tree)
  lo <- aggregate(height ~ plot + tree, x, min)
  field <- tapply(lo$height, lo$plot, median)
  c(SRS5 = field[["SRS5"]], SRS6 = field[["SRS6"]], BL60 = field[["BL60"]],
    FLM30 = median(lo$height[lo$plot %in% c("SRS5", "SRS6")]))
}
