# =============================================================================
# QA / decision support (stage 04): diffusive window for floating-chamber
# closures with bubbles. Six manually confirmed bubble traces
# (data/flux_metadata/ebullition_confirmed_traces.csv) are fitted with
#   A  released goFlux 0.4.0 goAquaFlux(), defaults (pre-bubble window,
#      bubble.window.size 30)
#   B  goFlux fork, branch feat/aqua-diffusive-deebulliated, de-ebulliated
#      window (diffusion.window = "deebulliated"; bubble.window.size 15)
#   C  same fork, pre-bubble window (isolates the bubble.window.size change)
# and compared with the legacy hand-built partitioning
# (output/ebullition/partitioned_fluxes.csv).
#
# The fork is installed into a private library given by GOFLUX_FORK_LIB
# (R CMD INSTALL -l <lib> of `git archive feat/aqua-diffusive-deebulliated`);
# each variant runs in its own R process (callr).
# Writes output/qa/ebullition_window_comparison.csv and .png.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(ggplot2); library(tidyr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/lib_raw.R")
fork_lib <- Sys.getenv("GOFLUX_FORK_LIB"); stopifnot(nzchar(fork_lib), dir.exists(fork_lib))

conf <- read_csv("data/flux_metadata/ebullition_confirmed_traces.csv", show_col_types = FALSE)
win  <- read_csv("output/flux/02_windows/windows.csv", show_col_types = FALSE)
aux  <- read_csv("output/flux/01_metadata/auxfile.csv", show_col_types = FALSE)
legacy <- read_csv("output/ebullition/partitioned_fluxes.csv", show_col_types = FALSE)

# Placement times in the legacy tables are analyzer time + a hard-coded offset
# (detect_ebullition.R: Picarro -25220 s; LGR per-day LGR_OFFSETS). Undo exactly
# that offset so every variant fits the same samples the legacy partitioning used.
# NB the legacy LGR2 2022-10-23 offset (-1091 s) is wrong: the five CP40 water
# closures of that day sit at offset ~0 in the raw record (saved windows: -15 s),
# so the legacy placements of that day are matched to the wrong closures.
legacy_offsets <- tibble(analyzer = c("LGR2", "LGR2", "LGR3", "LGR3", "LGR3", "LGR2", "LGR1", "LGR3"),
                         date = as.Date(c("2022-10-23", "2023-03-11", "2023-03-12", "2023-03-15", "2023-03-16",
                                          "2023-03-17", "2023-03-18", "2023-03-22")),
                         legacy_offset = c(-1091, -28, -24, -13, -24, -28, -14, -24))
day_off <- conf %>% mutate(date = as.Date(date)) %>% distinct(analyzer, date) %>%
  left_join(legacy_offsets, by = c("analyzer", "date")) %>%
  mutate(offset_s = case_when(analyzer == "Picarro" ~ 25220, !is.na(legacy_offset) ~ -legacy_offset, TRUE ~ 0)) %>%
  select(analyzer, date, offset_s)
# floating chamber geometry and the day's tower air temperature / pressure
geo <- aux %>% filter(component == "water", !excluded) %>% group_by(analyzer, date, plot) %>%
  summarise(Area = first(Area), Vtot = first(Vtot), Vcham = first(Vcham) / 1000, Tcham = mean(Tcham),
            Pcham = mean(Pcham), .groups = "drop")

traces <- conf %>% mutate(date = as.Date(date)) %>%
  left_join(day_off, by = c("analyzer", "date")) %>%
  left_join(geo, by = c("analyzer", "date", "site" = "plot")) %>%
  rowwise() %>% group_split() %>%
  lapply(function(p) {
    s <- as.POSIXct(p$start, tz = "UTC") + p$offset_s; e <- as.POSIXct(p$end, tz = "UTC") + p$offset_s
    r <- read_raw(p$analyzer, s, e)
    inst <- if (grepl("^LGR", p$analyzer)) c(0.35, 0.9) else c(0.025, 0.1)
    r %>% mutate(UniqueID = p$placement_id, Etime = as.numeric(POSIX.time - s, units = "secs"), flag = 1,
                 H2O_ppm = 0, CO2_prec = inst[1], CH4_prec = inst[2], H2O_prec = 0,
                 Area = p$Area, offset = 0, Vtot = p$Vtot, Vcham = p$Vcham, Tcham = p$Tcham, Pcham = p$Pcham) %>%
      select(-source_file) %>% as.data.frame()
  }) %>% bind_rows()
cat("Traces:", length(unique(traces$UniqueID)), "| rows:", nrow(traces), "\n")

run_variant <- function(lib, args) callr::r(function(d, args, lib) {
  if (!is.null(lib)) .libPaths(c(lib, .libPaths()))
  suppressMessages(library(goFlux))
  one <- function(x) tryCatch({
    res <- suppressWarnings(do.call(goFlux::goAquaFlux, c(list(dataframe = x, gastype = "CH4dry_ppb"), args)))
    as.data.frame(if (!is.null(res$flux_summary)) res$flux_summary else res[[1]])
  }, error = function(e) data.frame(UniqueID = x$UniqueID[1], error = conditionMessage(e)))
  s <- do.call(dplyr::bind_rows, lapply(split(d, d$UniqueID), one))
  list(version = as.character(utils::packageVersion("goFlux")), path = find.package("goFlux"), summary = s)
}, args = list(d = traces, args = args, lib = lib))

A <- run_variant(NULL, list())
B <- run_variant(fork_lib, list(diffusion.window = "deebulliated", bubble.window.size = 15))
C <- run_variant(fork_lib, list(diffusion.window = "pre_bubble", bubble.window.size = 15))
# Picarro logs every ~5 s: a 15-30 observation detection window spans 75-150 s,
# longer than its 2-min placements. Scaled to ~30 s (6 obs) for the Picarro traces:
pic <- unique(traces$UniqueID[grepl("^Picarro", traces$UniqueID)])
traces_pic <- traces[traces$UniqueID %in% pic, ]
run_pic <- function(lib, args) { old <- traces; traces <<- traces_pic; on.exit(traces <<- old); run_variant(lib, args) }
D <- run_pic(NULL, list(bubble.window.size = 6, diffusion.minimum_window = 6))
E <- run_pic(fork_lib, list(diffusion.window = "deebulliated", bubble.window.size = 6, diffusion.minimum_window = 6))
cat("A:", A$path, "\nB/C:", B$path, "\n")

pick <- function(x, tag) {
  s <- x$summary; nm <- names(s)
  col <- function(pat) { k <- grep(pat, nm, value = TRUE); if (length(k)) s[[k[1]]] else NA_real_ }
  tibble(placement_id = s$UniqueID, variant = tag,
         diffusive = col("^flux_diffusive$|diffusive_flux$|^flux_diffusive"),
         ebullitive = col("^flux_ebullitive$|ebullition_flux|^flux_ebull"),
         total = col("^flux_total$|total_flux|^flux_total"),
         n_bubbles = col("n_bubble|n_events|bubble_count"),
         window = if ("diffusive_window" %in% nm) s$diffusive_window else NA_character_,
         error = if ("error" %in% nm) s$error else NA_character_)
}
cmp <- bind_rows(pick(A, "A released 0.4.0 (pre-bubble, bws 30)"),
                 pick(B, "B fork de-ebulliated (bws 15)"),
                 pick(C, "C fork pre-bubble (bws 15)"),
                 pick(D, "D released, Picarro bws 6"),
                 pick(E, "E fork de-ebulliated, Picarro bws 6"),
                 legacy %>% filter(placement_id %in% conf$placement_id) %>%
                   transmute(placement_id, variant = "L legacy hand-built", diffusive = diffusive_flux_nmol,
                             ebullitive = ebull_flux_nmol, total = total_flux_nmol, n_bubbles = n_jumps,
                             window = "legacy"))
write_csv(cmp, "output/qa/ebullition_window_comparison.csv")
print(as.data.frame(cmp %>% mutate(across(c(diffusive, ebullitive, total), ~ round(.x, 2))) %>% arrange(placement_id, variant)),
      row.names = FALSE)
cat("\nfork flux_summary columns:", paste(names(B$summary), collapse = ", "), "\n")

long <- cmp %>% pivot_longer(c(diffusive, ebullitive, total), names_to = "component", values_to = "flux")
p <- ggplot(long, aes(placement_id, flux, fill = variant)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  facet_wrap(~ component, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("#9aa5b1", "#2a6f97", "#7fb3d5", "#5b8c5a", "#a3c99a", "#c98b2a")) +
  labs(x = NULL, y = expression(CH[4]~flux~(nmol~m^-2~s^-1)), fill = NULL,
       title = "Confirmed bubble traces: diffusive window choice") +
  theme_minimal(base_size = 10) + theme(legend.position = "top", axis.text.x = element_text(angle = 20, hjust = 1),
                                        panel.grid.major.x = element_blank())
ggsave("output/qa/ebullition_window_comparison.png", p, width = 10, height = 8, dpi = 150, bg = "white")
