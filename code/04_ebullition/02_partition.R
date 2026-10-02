# =============================================================================
# Diffusive / ebullitive CH4 partition per floating-chamber placement
# (pipeline stage 04, step 2).
#
# goAquaFlux from the goFlux fork (code/00_lib/goflux_fork.R, commit 2ed7224),
# run in its own R process with the fork's library first on the path:
#   diffusion.window = "deebulliated", bubble.window.size = 15
#   (Jon, 2026-10-01: LGR uses the de-ebulliated window). The Picarro logs every
#   ~5 s, so its detection window is 6 observations (~30 s; bubble.window.size
#   and diffusion.minimum_window = 6, Jon 2026-10-01): 15 observations (~75 s)
#   missed most of the hand-confirmed Picarro bubbles. Placements with < 30
#   observations get no bubble detection (flagged).
#   The October 2022 Picarro logs every ~2.6 s but updates CH4 only every
#   ~5.3 s, and its bubbles appear as smooth ramps over ~1 min, not steps
#   (hand-confirmed bubbles in BL60 water 82): goAquaFlux cannot separate them.
#   Picarro CH4 traces keep the fresh readings only (lib_raw.R fresh_ch4), so
#   a 6-observation window spans ~30 s (Oct 2022) to ~60 s (Mar 2022); the CO2
#   fit uses the fresh CO2 rows (lib_raw.R fresh_co2).
#   For the Picarro, over the placement start to the end of the diffusive window:
#     total      = two-point (endpoint) flux, (C_end - C_start) / t x flux term,
#                  C from the mean of the first / last ENDPOINT_S (model-free, the
#                  same estimate goAquaFlux uses as its consistency check);
#     diffusive  = goAquaFlux de-ebulliated diffusive flux;
#     ebullitive = total - diffusive (>= 0), flagged as a residual.
# For each placement (01_placements.R):
#   diffusive  = flux_diffusive of the call on the diffusive window
#                (first 10 min of a long placement, else the stage-02 window);
#   ebullitive = flux_ebullition of the call on the whole placement (summed
#                bubble steps / placement time), with the number of bubbles;
#   total      = diffusive + ebullitive.
# CO2 (no ebullition) = goAquaFlux flux_diffusive on the diffusive window.
# Geometry, Tcham and Pcham come from the closure named in geometry_from
# (stage-01 auxfile); no water-vapour correction (H2O set to 0, as in stage 03).
#
# Writes output/flux/04_ebullition/partition.csv, settings.json, bubbles.csv
# (detected bubbles over each placement, Etime s from the placement start, CH4
# ppb) and traces.csv.gz (placement CH4, de-ebulliated CH4, diffusive window;
# read by the Fig S1 script, which runs without the raw records).
# =============================================================================
suppressMessages({library(dplyr); library(readr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/lib_raw.R")
source("code/00_lib/goflux_fork.R")
fork_lib <- goflux_fork_lib()

AQUA_ARGS <- list(LGR = list(diffusion.window = "deebulliated", bubble.window.size = 15),
                  Picarro = list(diffusion.window = "deebulliated", bubble.window.size = 6, diffusion.minimum_window = 6))
PREC <- list(LGR = c(CO2 = 0.35, CH4 = 0.9), Picarro = c(CO2 = 0.025, CH4 = 0.1))   # as in stage 03
MIN_CH4_PPB <- 1500; MIN_CO2_PPM <- 300                                              # start-up readings
ENDPOINT_S <- 15                                                                     # Picarro two-point means
utc <- function(x) as.POSIXct(x, tz = "UTC")

pl  <- read_csv("output/flux/04_ebullition/placements.csv", show_col_types = FALSE,
                col_types = cols(.default = col_character())) %>%
  mutate(across(c(placement_start, placement_end, diffusive_start, diffusive_end), utc))
aux <- read_csv("output/flux/01_metadata/auxfile.csv", show_col_types = FALSE)

trace_of <- function(p, s, e, id, ch4_fresh = TRUE) {
  g <- aux %>% filter(UniqueID == p$geometry_from)
  inst <- if (grepl("^LGR", p$analyzer)) "LGR" else "Picarro"
  r <- read_raw(p$analyzer, s, e)
  r <- r[if (ch4_fresh) fresh_ch4(r, p$analyzer) else fresh_co2(r, p$analyzer), ]   # Picarro: the gas's fresh rows
  r %>% filter(CH4dry_ppb >= MIN_CH4_PPB, CO2dry_ppm >= MIN_CO2_PPM) %>%
    mutate(UniqueID = id, Etime = as.numeric(POSIX.time - min(POSIX.time), units = "secs"), flag = 1,
           H2O_ppm = 0, CO2_prec = PREC[[inst]][["CO2"]], CH4_prec = PREC[[inst]][["CH4"]], H2O_prec = 0,
           Area = g$Area, offset = 0, Vtot = g$Vtot, Vcham = g$Vcham / 1000, Tcham = g$Tcham, Pcham = g$Pcham) %>%
    select(-any_of("source_file")) %>% as.data.frame()
}
diff_tr <- bind_rows(lapply(seq_len(nrow(pl)), function(i) trace_of(pl[i, ], pl$diffusive_start[i], pl$diffusive_end[i], pl$placement_id[i])))
full_tr <- bind_rows(lapply(seq_len(nrow(pl)), function(i) trace_of(pl[i, ], pl$placement_start[i], pl$placement_end[i], pl$placement_id[i])))

aqua <- function(d, gas, args) callr::r(function(d, gas, args, lib) {
  .libPaths(c(lib, .libPaths())); suppressMessages(library(goFlux))
  one <- function(x) {
    w <- character(0)
    res <- withCallingHandlers(
      tryCatch(do.call(goFlux::goAquaFlux, c(list(dataframe = x, gastype = gas), args)), error = function(e) e),
      warning = function(wn) { w <<- c(w, conditionMessage(wn)); invokeRestart("muffleWarning") })
    if (inherits(res, "error")) return(data.frame(UniqueID = x$UniqueID[1], error = conditionMessage(res)))
    s <- as.data.frame(res$flux_summary)
    b <- res$bubbles; if (!is.null(b)) b <- b[b$UniqueID == x$UniqueID[1], , drop = FALSE]
    s$n_bubbles <- if (!is.null(b)) nrow(b) else 0L
    s$warnings <- paste(unique(w), collapse = " | ")
    attr(s, "bubbles") <- if (!is.null(b) && nrow(b)) data.frame(UniqueID = b$UniqueID, start = b$start, end = b$end, magnitude = b$magnitude) else NULL
    attr(s, "deeb") <- if (!is.null(res$deebulliated)) data.frame(UniqueID = x$UniqueID[1], Etime = res$deebulliated$Etime,
                                                                   CH4_deebulliated = res$deebulliated[[gas]]) else NULL
    s
  }
  parts <- lapply(split(d, d$UniqueID), one)
  out <- do.call(dplyr::bind_rows, parts)
  attr(out, "bubbles") <- do.call(rbind, lapply(parts, attr, "bubbles"))
  attr(out, "deeb") <- do.call(rbind, lapply(parts, attr, "deeb"))
  attr(out, "goFlux") <- c(version = as.character(utils::packageVersion("goFlux")), path = find.package("goFlux"))
  out
}, args = list(d = d, gas = gas, args = args, lib = fork_lib))

pic_ids <- pl$placement_id[pl$analyzer == "Picarro"]
aqua2 <- function(tr, gas) {
  a <- aqua(tr[!tr$UniqueID %in% pic_ids, ], gas, AQUA_ARGS$LGR)
  b <- aqua(tr[tr$UniqueID %in% pic_ids, ], gas, AQUA_ARGS$Picarro)
  out <- dplyr::bind_rows(a, b); attr(out, "goFlux") <- attr(a, "goFlux")
  attr(out, "bubbles") <- rbind(attr(a, "bubbles"), attr(b, "bubbles")); attr(out, "deeb") <- rbind(attr(a, "deeb"), attr(b, "deeb"))
  out
}
pic_tr <- bind_rows(lapply(which(pl$analyzer == "Picarro"), function(i)
  trace_of(pl[i, ], pl$placement_start[i], pl$diffusive_end[i], pl$placement_id[i])))
ch4_pic <- aqua(pic_tr, "CH4dry_ppb", AQUA_ARGS$Picarro)
two_point <- pic_tr %>% group_by(placement_id = UniqueID) %>%
  summarise(C0 = mean(CH4dry_ppb[Etime <= ENDPOINT_S]), Cf = mean(CH4dry_ppb[Etime >= max(Etime) - ENDPOINT_S]),
            t_s = max(Etime) - ENDPOINT_S,
            term = first(Vtot) * first(Pcham) / (8.314 * (first(Tcham) + 273.15) * first(Area) / 1e4), .groups = "drop") %>%
  transmute(placement_id, pic_two_point = (Cf - C0) / t_s * term)
ch4_diff <- aqua2(diff_tr, "CH4dry_ppb")
ch4_full <- aqua2(full_tr, "CH4dry_ppb")
co2_tr <- bind_rows(lapply(seq_len(nrow(pl)), function(i)
  trace_of(pl[i, ], pl$diffusive_start[i], pl$diffusive_end[i], pl$placement_id[i], ch4_fresh = FALSE)))
co2_diff <- aqua2(co2_tr, "CO2dry_ppm")
stopifnot(grepl("goflux-aqua", attr(ch4_full, "goFlux")[["path"]]))
col <- function(x, nm) if (nm %in% names(x)) x[[nm]] else rep(NA, nrow(x))

nobs <- function(tr) tr %>% count(placement_id = UniqueID, name = "n")
res <- pl %>%
  left_join(tibble(placement_id = ch4_diff$UniqueID, CH4_diffusive = col(ch4_diff, "flux_diffusive"),
                   CH4_diffusive_SE = col(ch4_diff, "SE_diffusive"), CH4_diffusive_window = col(ch4_diff, "diffusive_window"),
                   n_obs_diffusion = col(ch4_diff, "n_obs.diffusion"), diff_warnings = col(ch4_diff, "warnings"),
                   diff_error = col(ch4_diff, "error")), by = "placement_id") %>%
  left_join(tibble(placement_id = ch4_full$UniqueID, CH4_ebullitive = col(ch4_full, "flux_ebullition"),
                   CH4_ebullitive_SE = col(ch4_full, "SE_ebullition"), n_bubbles = col(ch4_full, "n_bubbles"),
                   CH4_total_whole_placement = col(ch4_full, "flux_total"), full_warnings = col(ch4_full, "warnings"),
                   full_error = col(ch4_full, "error")), by = "placement_id") %>%
  left_join(tibble(placement_id = co2_diff$UniqueID, CO2_flux = col(co2_diff, "flux_diffusive"),
                   CO2_flux_SE = col(co2_diff, "SE_diffusive")), by = "placement_id") %>%
  left_join(nobs(diff_tr) %>% rename(n_obs_diffusive_window = n), by = "placement_id") %>%
  left_join(nobs(full_tr) %>% rename(n_obs_placement = n), by = "placement_id") %>%
  left_join(tibble(placement_id = ch4_pic$UniqueID, pic_total = col(ch4_pic, "flux_total"),
                   pic_diff = col(ch4_pic, "flux_diffusive"), pic_ebul = col(ch4_pic, "flux_ebullition"),
                   pic_nb = col(ch4_pic, "n_bubbles")), by = "placement_id") %>%
  left_join(two_point, by = "placement_id") %>%
  mutate(pic = analyzer == "Picarro",
         CH4_diffusive = if_else(pic, pic_diff, CH4_diffusive),
         CH4_ebullitive = if_else(pic, pmax(pic_two_point - pic_diff, 0), CH4_ebullitive),
         n_bubbles = if_else(pic, pic_nb, n_bubbles),
         CH4_ebullitive = coalesce(CH4_ebullitive, 0),
         CH4_total = if_else(pic, CH4_diffusive + CH4_ebullitive, CH4_diffusive + CH4_ebullitive),
         CH4_ebullitive_fraction = if_else(!is.na(CH4_total) & CH4_total > 0, CH4_ebullitive / CH4_total, NA_real_),
         ebullition_flag = case_when(
           grepl("bubble detection skipped", full_warnings) ~ "bubble detection skipped (< 30 observations)",
           analyzer == "Picarro" ~ "Picarro: bubbles not separable at ~5 s CH4 updates; total = two-point from placement start, ebullitive = total - de-ebulliated diffusive",
           TRUE ~ NA_character_))

f <- function(x) format(x, "%Y-%m-%d %H:%M:%S")
out <- res %>% transmute(placement_id, logged = as.logical(logged), analyzer, plot, date, geometry_from,
                         placement_start = f(placement_start), placement_end = f(placement_end), duration_s = as.numeric(duration_s),
                         end_by, long = as.logical(long), diffusive_start = f(diffusive_start), diffusive_end = f(diffusive_end),
                         diffusive_rule, n_obs_diffusive_window, n_obs_placement,
                         CH4_diffusive, CH4_diffusive_SE, CH4_diffusive_window, n_obs_diffusion,
                         CH4_ebullitive, CH4_ebullitive_SE, n_bubbles, CH4_total, CH4_ebullitive_fraction,
                         CH4_total_whole_placement, CO2_flux, CO2_flux_SE, ebullition_flag,
                         diff_error, full_error, diff_warnings, full_warnings)
write_csv(out, "output/flux/04_ebullition/partition.csv")

# traces for the display items (stage 08 runs without the raw records): every
# placement's CH4 (fresh rows), the de-ebulliated series and the diffusive window
tr_out <- full_tr %>% select(placement_id = UniqueID, Etime, POSIX.time, CH4_ppb = CH4dry_ppb) %>%
  left_join(as.data.frame(attr(ch4_full, "deeb")) %>% rename(placement_id = UniqueID), by = c("placement_id", "Etime")) %>%
  left_join(pl %>% select(placement_id, diffusive_start, diffusive_end), by = "placement_id") %>%
  mutate(in_diffusive_window = POSIX.time >= diffusive_start & POSIX.time <= diffusive_end,
         CH4_ppb = round(CH4_ppb, 2), CH4_deebulliated = round(CH4_deebulliated, 2), Etime = round(Etime, 1)) %>%
  select(placement_id, Etime, CH4_ppb, CH4_deebulliated, in_diffusive_window)
write_csv(tr_out, "output/flux/04_ebullition/traces.csv.gz")
bub <- attr(ch4_full, "bubbles")
write_csv(if (is.null(bub)) tibble(placement_id = character(), start = numeric(), end = numeric(), magnitude = numeric())
          else as_tibble(bub) %>% rename(placement_id = UniqueID), "output/flux/04_ebullition/bubbles.csv")
jsonlite::write_json(list(goFlux = as.list(attr(ch4_full, "goFlux")), tarball = GOFLUX_FORK_TARBALL,
                          goAquaFlux_args = AQUA_ARGS, prec = PREC, min_ch4_ppb = MIN_CH4_PPB, min_co2_ppm = MIN_CO2_PPM),
                     "output/flux/04_ebullition/settings.json", auto_unbox = TRUE, pretty = TRUE)

cat("Placements:", nrow(out), "| CH4 diffusive NA:", sum(is.na(out$CH4_diffusive)), "| with bubbles:", sum(out$n_bubbles > 0, na.rm = TRUE), "\n")
print(as.data.frame(out %>% transmute(placement_id, min = round(duration_s / 60, 1), diff = round(CH4_diffusive, 2),
                                      ebul = round(CH4_ebullitive, 2), nb = n_bubbles, total = round(CH4_total, 2),
                                      CO2 = round(CO2_flux, 3), flag = substr(coalesce(ebullition_flag, diff_error, full_error, ""), 1, 30))),
      row.names = FALSE)
