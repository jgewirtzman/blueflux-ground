# =============================================================================
# Site greenness from MODIS (context for Table S4 and the ghost-forest GPP note).
#   MOD13Q1 v061 (250 m, 16-day): NDVI, EVI, pixel reliability
#   MOD15A2H v061 (500 m, 8-day): LAI, FPAR, FparLai_QC
# Single pixel at each site's coordinates (data/sites/site_metadata.csv), 2015-16 and 2022-23,
# from the ORNL DAAC MODIS/VIIRS subset web service (public; no login). Raw JSON is
# cached in data/environmental/satellite/modis/ and only downloaded when absent.
# Summaries: pre-Irma (2015-2016) and 2022-2023 medians of good-quality composites,
# and the campaign-month composites (Oct 2022, Mar 2023).
# Writes output/analysis/si/site_greenness_modis.csv.
# =============================================================================
suppressMessages({library(dplyr); library(jsonlite)})
options(timeout = 180)
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
API <- "https://modis.ornl.gov/rst/api/v1"
get_json <- function(u) { for (try in 1:6) { r <- tryCatch(fromJSON(u), error = function(e) NULL); if (!is.null(r)) return(r); Sys.sleep(15 * try) }
  stop("MODIS service unavailable: ", u) }
cache <- "data/environmental/satellite/modis"; dir.create(cache, recursive = TRUE, showWarnings = FALSE)
meta <- read.csv("data/sites/site_metadata.csv")
prods <- list(MOD13Q1 = c("250m_16_days_NDVI", "250m_16_days_EVI", "250m_16_days_pixel_reliability"),
              MOD15A2H = c("Lai_500m", "Fpar_500m", "FparLai_QC"))

fetch <- function(prod, band, site, lat, lon) {
  f <- file.path(cache, sprintf("%s_%s_%s.json", site, prod, band))
  if (!file.exists(f)) {
    d <- get_json(sprintf("%s/%s/dates?latitude=%f&longitude=%f", API, prod, lat, lon))$dates
    d <- d[substr(d$calendar_date, 1, 4) %in% c("2015", "2016", "2022", "2023"), ]
    # service limit: 10 dates per request, counted between start and end, so chunk within each year
    chunks <- unlist(lapply(split(d$modis_date, substr(d$calendar_date, 1, 4)),
                            function(x) split(x, ceiling(seq_along(x) / 10))), recursive = FALSE)
    out <- bind_rows(lapply(chunks, function(ch) {
      u <- sprintf("%s/%s/subset?latitude=%f&longitude=%f&band=%s&startDate=%s&endDate=%s&kmAboveBelow=0&kmLeftRight=0",
                   API, prod, lat, lon, band, ch[1], ch[length(ch)])
      r <- get_json(u); s <- r$subset
      data.frame(date = s$calendar_date, value = sapply(s$data, `[`, 1), scale = as.numeric(r$scale))
    }))
    write_json(out, f, digits = NA)
  }
  fromJSON(f) %>% mutate(site = site, prod = prod, band = band)
}
raw <- bind_rows(lapply(seq_len(nrow(meta)), function(i) bind_rows(lapply(names(prods), function(p)
  bind_rows(lapply(prods[[p]], function(b) fetch(p, b, meta$site_id[i], meta$latitude[i], meta$longitude[i])))))))

v13 <- raw %>% filter(prod == "MOD13Q1") %>% select(site, date, band, value, scale) %>%
  mutate(value = ifelse(band == "250m_16_days_pixel_reliability", value, value * ifelse(scale > 0, scale, 1))) %>%
  select(-scale) %>% tidyr::pivot_wider(names_from = band, values_from = value) %>%
  transmute(site, date = as.Date(date), NDVI = `250m_16_days_NDVI`, EVI = `250m_16_days_EVI`,
            good = `250m_16_days_pixel_reliability` %in% c(0, 1))
v15 <- raw %>% filter(prod == "MOD15A2H") %>% select(site, date, band, value, scale) %>%
  tidyr::pivot_wider(names_from = band, values_from = c(value, scale)) %>%
  transmute(site, date = as.Date(date),
            LAI = ifelse(value_Lai_500m <= 100, value_Lai_500m * scale_Lai_500m, NA),
            good = bitwAnd(as.integer(value_FparLai_QC), 1L) == 0L)       # bit 0: main algorithm, good quality
per <- function(d) ifelse(format(d, "%Y") %in% c("2015", "2016"), "pre-Irma 2015-16",
                   ifelse(format(d, "%Y") %in% c("2022", "2023"), "2022-23", NA))
camp <- function(d) ifelse(format(d, "%Y-%m") == "2022-10", "Oct 2022", ifelse(format(d, "%Y-%m") == "2023-03", "Mar 2023", NA))
s13 <- v13 %>% filter(good) %>% mutate(period = per(date)) %>% filter(!is.na(period)) %>%
  group_by(site, period) %>% summarise(NDVI = median(NDVI), EVI = median(EVI), n13 = n(), .groups = "drop")
s15 <- v15 %>% filter(good, !is.na(LAI)) %>% mutate(period = per(date)) %>% filter(!is.na(period)) %>%
  group_by(site, period) %>% summarise(LAI = median(LAI), n15 = n(), .groups = "drop")
c13 <- v13 %>% mutate(period = camp(date)) %>% filter(!is.na(period)) %>%
  group_by(site, period) %>% summarise(NDVI = median(NDVI), EVI = median(EVI), n13 = n(), .groups = "drop")
c15 <- v15 %>% mutate(period = camp(date)) %>% filter(!is.na(period), !is.na(LAI)) %>%
  group_by(site, period) %>% summarise(LAI = median(LAI), n15 = n(), .groups = "drop")
out <- bind_rows(full_join(s13, s15, by = c("site", "period")), full_join(c13, c15, by = c("site", "period"))) %>%
  arrange(site, period)
dir.create("output/analysis/si", showWarnings = FALSE, recursive = TRUE)
write.csv(out, "output/analysis/si/site_greenness_modis.csv", row.names = FALSE)
print(as.data.frame(out %>% mutate(across(c(NDVI, EVI, LAI), ~ round(.x, 2)))))
