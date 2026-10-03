# =============================================================================
# QA: raw CH4 / CO2 traces of the BL60 October 2022 Laguncularia stem closures
# (high base fluxes under review). Needs data/analyzer. Writes
# output/qa/review_BL60_LARA_{ch4,co2}.png.
# =============================================================================
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
suppressMessages({library(dplyr);library(readr);library(ggplot2);library(tidyr)})
source("code/00_lib/lib_raw.R")
T <- function(x) as.POSIXct(x, tz="UTC")
win <- read_csv("output/flux/02_windows/windows.csv", show_col_types=FALSE) %>% mutate(ws=T(start), we=T(end))
ids <- c("Oct_22_257_BL60_stem","Oct_22_258_BL60_stem","Oct_22_259_BL60_stem","Oct_22_260_BL60_stem","Oct_22_261_BL60_stem","Oct_22_262_BL60_stem",
         "Oct_22_254_BL60_stem","Oct_22_255_BL60_stem","Oct_22_228_BL60_stem","Oct_22_229_BL60_stem","Oct_22_230_BL60_stem","Oct_22_231_BL60_stem","Oct_22_232_BL60_stem","Oct_22_233_BL60_stem")
w <- win %>% filter(UniqueID %in% ids); print(w %>% select(UniqueID, analyzer, start, end, window_source))
d <- read.csv("output/data_products/combined_gas_flux_dataset.csv")
out <- list()
for (i in seq_len(nrow(w))) { r <- read_raw(w$analyzer[i], w$ws[i]-120, w$we[i]+120)
  out[[i]] <- r %>% transmute(t=as.numeric(POSIX.time-w$ws[i]), CH4=CH4dry_ppb/1000, CO2=CO2dry_ppm, H2O=H2O_ppm/1000, id=w$UniqueID[i]) }
r <- bind_rows(out) %>% left_join(d %>% transmute(id=flux_id, lab=sprintf("%s  h=%gcm  CH4=%.1f", sub("_BL60_stem","",flux_id), height, CH4_best.flux)), by="id")
r <- r %>% left_join(w %>% transmute(id=UniqueID, len=as.numeric(we-ws)), by="id")
r$lab <- factor(r$lab, unique(r$lab[order(match(r$id, ids))]))
pc <- ggplot(r, aes(t, CH4)) + geom_rect(data=distinct(r, lab, len), aes(xmin=0, xmax=len, ymin=-Inf, ymax=Inf), fill="grey85", inherit.aes=FALSE) +
  geom_line(colour="#d7301f", linewidth=.4) + facet_wrap(~lab, scales="free_y", ncol=4) + theme_bw(base_size=9) +
  labs(x="seconds from fit-window start (grey = fit window)", y="CH4 (ppm)")
ggsave("output/qa/review_BL60_LARA_ch4.png", pc, width=13, height=9, dpi=110)
pco <- ggplot(r, aes(t, CO2)) + geom_rect(data=distinct(r, lab, len), aes(xmin=0, xmax=len, ymin=-Inf, ymax=Inf), fill="grey85", inherit.aes=FALSE) + geom_line(colour="#1a9850", linewidth=.4) + facet_wrap(~lab, scales="free_y", ncol=4) + theme_bw(base_size=9) + labs(x="seconds from fit-window start (grey = fit window)", y="CO2 (ppm)")
ggsave("output/qa/review_BL60_LARA_co2.png", pco, width=13, height=9, dpi=110)
