# =============================================================================
# QA: the Picarro G4301 measures one gas per logged row, alternating: CH4 on
# every other row, carried forward almost unchanged (|dCH4| < HELD_PPB) on the
# rows in between, where CO2 is the fresh reading. Half the CH4 first
# differences are therefore ~0. What that does to the stage-03 quantities,
# all rows vs fresh CH4 rows only:
#   sigma_emp (MAD of first differences / sqrt 2), MDF_emp, LM slope and SE,
#   number of points (HM >= 30 rule).
# Writes output/qa/picarro_update_cadence.png and
# output/qa/picarro_update_cadence_closures.csv.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(ggplot2); library(tidyr); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/lib_raw.R")
HELD_PPB <- 0.3
T <- function(x) as.POSIXct(x, tz = "UTC")
fresh <- function(r) { d <- c(Inf, abs(diff(r$CH4dry_ppb))); r[d >= HELD_PPB, ] }

# 1. what the rows look like (BL60 water 82 rise, 2022-10-25)
r <- read_raw("Picarro", T("2022-10-25 18:11:40"), T("2022-10-25 18:12:40")) %>%
  mutate(dCH4 = c(NA, diff(CH4dry_ppb)), held = !is.na(dCH4) & abs(dCH4) < HELD_PPB)
g1 <- ggplot(r, aes(POSIX.time, CH4dry_ppb)) + geom_step(colour = "grey60") +
  geom_point(aes(colour = held), size = 2) +
  scale_colour_manual(values = c(`FALSE` = "#2a6f97", `TRUE` = "#c0392b"), labels = c("fresh CH4 reading", "carried forward (|dCH4| < 0.3 ppb)"), name = NULL) +
  labs(title = "Logged rows, BL60 water 82 (2022-10-25): CH4 is new on every other row", x = NULL, y = "CH4 (ppb)") +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom")

# 2. distribution of |dCH4| over every Picarro water / soil / stem window
win <- read_csv("output/flux/02_windows/windows.csv", show_col_types = FALSE) %>% filter(analyzer == "Picarro") %>%
  mutate(ws = T(start), we = T(end))
fit <- read_csv("output/flux/03_fit/CH4/fluxes.csv", show_col_types = FALSE)
aux <- read_csv("output/flux/01_metadata/auxfile.csv", show_col_types = FALSE)
tr <- bind_rows(lapply(seq_len(nrow(win)), function(i) read_raw("Picarro", win$ws[i], win$we[i]) %>%
  mutate(UniqueID = win$UniqueID[i], campaign = format(win$ws[i], "%Y-%m"))))
dd <- tr %>% group_by(UniqueID) %>% mutate(d = c(NA, abs(diff(CH4dry_ppb)))) %>% ungroup() %>% filter(!is.na(d))
g2 <- ggplot(dd, aes(pmax(d, 1e-3))) + geom_histogram(bins = 80, fill = "grey40") + scale_x_log10() +
  geom_vline(xintercept = HELD_PPB, colour = "#c0392b", linetype = 2) + facet_wrap(~ campaign, scales = "free_y") +
  labs(title = "|dCH4| between consecutive rows, all Picarro closure windows (log scale)", x = "|dCH4| (ppb)", y = "rows") +
  theme_minimal(base_size = 10)

# 3. stage-03 quantities, all rows vs fresh rows
sig <- function(x) stats::mad(diff(x)) / sqrt(2)
camp <- bind_rows(tr %>% group_by(campaign, UniqueID) %>% summarise(s = sig(CH4dry_ppb), .groups = "drop") %>% mutate(rows = "all rows"),
                  tr %>% group_by(campaign, UniqueID) %>% group_modify(~ fresh(.x)) %>% summarise(s = sig(CH4dry_ppb), .groups = "drop") %>%
                    mutate(rows = "fresh CH4 rows")) %>%
  group_by(campaign, rows) %>% summarise(sigma_ppb = median(s, na.rm = TRUE), .groups = "drop")
lmfit <- function(x) { t <- as.numeric(x$POSIX.time) - as.numeric(min(x$POSIX.time)); m <- summary(lm(x$CH4dry_ppb ~ t))
  tibble(n = nrow(x), slope = m$coefficients[2, 1], se = m$coefficients[2, 2], dur = max(t)) }
cl <- tr %>% group_by(UniqueID, campaign) %>% group_modify(~ bind_cols(lmfit(.x) %>% rename_with(~ paste0(.x, "_all")),
                                                                   lmfit(fresh(.x)) %>% rename_with(~ paste0(.x, "_fresh")))) %>%
  ungroup() %>% left_join(aux %>% select(UniqueID, component), by = "UniqueID") %>%
  left_join(fit %>% select(UniqueID, model, MDF_emp, best.flux, flux.term), by = "UniqueID") %>%
  left_join(camp %>% select(-rows) %>% group_by(campaign) %>% summarise(sig_all = first(sigma_ppb), sig_fresh = last(sigma_ppb)), by = "campaign") %>%
  mutate(MDF_all = 1.96 * sig_all / dur_all * flux.term, MDF_fresh = 1.96 * sig_fresh / dur_fresh * flux.term,
         flux_all = slope_all * flux.term, flux_fresh = slope_fresh * flux.term,
         SE_ratio = se_fresh / se_all, below_all = abs(best.flux) <= MDF_all, below_fresh = abs(best.flux) <= MDF_fresh)
write_csv(cl, "output/qa/picarro_update_cadence_closures.csv")

g3 <- ggplot(cl, aes(flux_all, flux_fresh, colour = component)) + geom_abline(linetype = 2) + geom_point(size = 2) +
  scale_x_continuous(trans = "pseudo_log") + scale_y_continuous(trans = "pseudo_log") +
  labs(title = "LM CH4 flux: all rows vs fresh rows", x = "all rows (nmol m-2 s-1)", y = "fresh rows only") + theme_minimal(base_size = 10)
g4 <- ggplot(cl, aes(component, SE_ratio)) + geom_boxplot(outlier.size = 1) + geom_hline(yintercept = 1, linetype = 2) +
  labs(title = "LM slope SE, fresh / all rows", x = NULL, y = "SE ratio") + theme_minimal(base_size = 10)
ggsave("output/qa/picarro_update_cadence.png", (g1 | g2) / (g3 | g4), width = 14, height = 10, dpi = 110, bg = "white")

cat("Empirical CH4 precision sigma_emp (median per closure, ppb):\n"); print(as.data.frame(camp %>% mutate(sigma_ppb = round(sigma_ppb, 2))), row.names = FALSE)
cat("\nClosures:", nrow(cl), "| points all/fresh (median):", median(cl$n_all), "/", median(cl$n_fresh),
    "| HM-eligible (>= 30 points) all / fresh:", sum(cl$n_all >= 30), "/", sum(cl$n_fresh >= 30), "\n")
cat("LM flux fresh/all, median ratio:", round(median(cl$flux_fresh / cl$flux_all, na.rm = TRUE), 3),
    "| SE fresh/all, median:", round(median(cl$SE_ratio, na.rm = TRUE), 2), "\n")
cat("Below MDF: all-rows sigma", sum(cl$below_all, na.rm = TRUE), "| fresh-rows sigma", sum(cl$below_fresh, na.rm = TRUE), "of", nrow(cl), "\n")
print(as.data.frame(cl %>% count(campaign, component)), row.names = FALSE)
