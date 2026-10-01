library(data.table)
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

set.seed(20260420)

infile <- "data/tower/AMF_US-Skr_BASE_HH_2-5.csv"
outfile <- "output/gpp/US-Skr_GPP_halfhourly_Mar2022_Oct2022_Mar2023.csv"
diagfile <- "output/gpp/US-Skr_GPP_model_diagnostics.csv"

target_months <- data.table(
  year = c(2022L, 2022L, 2023L),
  month = c(3L, 10L, 3L)
)

boot_n <- 200L
night_sw_threshold <- 10
day_sw_threshold <- 10

needed <- c("TIMESTAMP_START", "TIMESTAMP_END", "NEE_PI", "FC", "SC", "TA_1_1_1", "SW_IN", "USTAR")
dt <- fread(infile, select = needed)

for (nm in names(dt)) {
  if (is.numeric(dt[[nm]])) {
    set(dt, which(dt[[nm]] <= -9990), nm, NA_real_)
  }
}

parse_ts <- function(x) {
  as.POSIXct(as.character(x), format = "%Y%m%d%H%M", tz = "UTC")
}

dt[, timestamp_start := parse_ts(TIMESTAMP_START)]
dt[, timestamp_end := parse_ts(TIMESTAMP_END)]
dt[, timestamp_mid := timestamp_start + as.numeric(difftime(timestamp_end, timestamp_start, units = "secs")) / 2]
dt[, `:=`(
  year = as.integer(format(timestamp_mid, "%Y")),
  month = as.integer(format(timestamp_mid, "%m")),
  doy = as.integer(format(timestamp_mid, "%j")),
  hour = as.integer(format(timestamp_mid, "%H")),
  minute = as.integer(format(timestamp_mid, "%M"))
)]

fill_linear <- function(x) {
  idx <- seq_along(x)
  ok <- is.finite(x)
  if (sum(ok) == 0L) return(x)
  if (sum(ok) == 1L) return(rep(x[ok][1], length(x)))
  as.numeric(approx(idx[ok], x[ok], xout = idx, rule = 2)$y)
}

dt[, TA_MODEL := fill_linear(TA_1_1_1)]
dt[, SW_MODEL := pmax(fill_linear(SW_IN), 0)]
dt[, driver_gapfill_flag := !is.finite(TA_1_1_1) | !is.finite(SW_IN)]

season_name <- function(month) {
  fifelse(month %in% c(12L, 1L, 2L), "DJF",
    fifelse(month %in% 3L:5L, "MAM",
      fifelse(month %in% 6L:8L, "JJA", "SON")
    )
  )
}

dt[, season := season_name(month)]
dt[, season_year := fifelse(month == 12L, year + 1L, year)]
dt[, period := sprintf("%s_%d", season, season_year)]

dt[, NEE_OBS := fifelse(is.finite(NEE_PI), NEE_PI,
  fifelse(is.finite(FC) & is.finite(SC), FC + SC, FC)
)]
dt[, NEE_source := fifelse(is.finite(NEE_PI), "NEE_PI",
  fifelse(is.finite(FC) & is.finite(SC), "FC_plus_SC",
    fifelse(is.finite(FC), "FC_only", "gap")
  )
)]

target_months[, season := season_name(month)]
target_months[, season_year := fifelse(month == 12L, year + 1L, year)]
target_months[, period := sprintf("%s_%d", season, season_year)]

fit_temp <- function(train) {
  night <- train[SW_MODEL <= night_sw_threshold & is.finite(NEE_OBS) &
                   is.finite(TA_MODEL) & NEE_OBS > 0]
  if (nrow(night) < 30L) {
    stop("Not enough nighttime positive NEE records for temperature response.")
  }
  fit <- lm(log(NEE_OBS) ~ TA_MODEL, data = night)
  pred <- exp(predict(fit, newdata = night))
  sigma <- sqrt(mean((night$NEE_OBS - pred)^2, na.rm = TRUE))
  list(fit = fit, data = night, sigma = sigma)
}

gpp_curve <- function(sw, alpha, pmax) {
  out <- (alpha * sw * pmax) / (alpha * sw + pmax)
  out[!is.finite(out) | sw <= day_sw_threshold] <- 0
  out
}

fit_light <- function(train, temp_coef) {
  day <- train[SW_MODEL > day_sw_threshold & is.finite(NEE_OBS) &
                 is.finite(TA_MODEL) & is.finite(SW_MODEL)]
  if (nrow(day) < 30L) {
    stop("Not enough daytime NEE records for light response.")
  }
  reco <- exp(temp_coef[1] + temp_coef[2] * day$TA_MODEL)
  day[, gpp_obs := pmax(reco - NEE_OBS, 0)]
  start_pmax <- max(stats::quantile(day$gpp_obs, 0.95, na.rm = TRUE), 1)
  obj <- function(par) {
    alpha <- exp(par[1])
    pmax <- exp(par[2])
    pred <- gpp_curve(day$SW_MODEL, alpha, pmax)
    mean((day$gpp_obs - pred)^2, na.rm = TRUE)
  }
  opt <- optim(log(c(alpha = 0.02, pmax = start_pmax)), obj, method = "Nelder-Mead",
               control = list(maxit = 2000))
  pars <- exp(opt$par)
  names(pars) <- c("alpha", "pmax")
  pred <- gpp_curve(day$SW_MODEL, pars[["alpha"]], pars[["pmax"]])
  sigma <- sqrt(mean((day$gpp_obs - pred)^2, na.rm = TRUE))
  list(par = pars, data = day, sigma = sigma)
}

bootstrap_models <- function(temp_fit, light_fit, n_boot) {
  temp_coefs <- matrix(NA_real_, nrow = n_boot, ncol = 2)
  light_pars <- matrix(NA_real_, nrow = n_boot, ncol = 2)
  colnames(temp_coefs) <- c("intercept", "ta_slope")
  colnames(light_pars) <- c("alpha", "pmax")

  night <- temp_fit$data
  day <- light_fit$data

  for (i in seq_len(n_boot)) {
    ns <- night[sample.int(nrow(night), nrow(night), replace = TRUE)]
    tf <- try(lm(log(NEE_OBS) ~ TA_MODEL, data = ns), silent = TRUE)
    if (inherits(tf, "try-error")) next
    tc <- coef(tf)
    temp_coefs[i, ] <- c(tc[[1]], tc[[2]])

    ds <- day[sample.int(nrow(day), nrow(day), replace = TRUE)]
    reco <- exp(tc[[1]] + tc[[2]] * ds$TA_MODEL)
    ds[, gpp_obs_boot := pmax(reco - NEE_OBS, 0)]
    start_pmax <- max(stats::quantile(ds$gpp_obs_boot, 0.95, na.rm = TRUE), 1)
    obj <- function(par) {
      alpha <- exp(par[1])
      pmax <- exp(par[2])
      pred <- gpp_curve(ds$SW_MODEL, alpha, pmax)
      mean((ds$gpp_obs_boot - pred)^2, na.rm = TRUE)
    }
    lf <- try(optim(log(c(alpha = 0.02, pmax = start_pmax)), obj,
                    method = "Nelder-Mead", control = list(maxit = 1000)),
              silent = TRUE)
    if (inherits(lf, "try-error") || lf$convergence > 1) next
    light_pars[i, ] <- exp(lf$par)
  }

  keep <- complete.cases(temp_coefs) & complete.cases(light_pars)
  list(temp = temp_coefs[keep, , drop = FALSE],
       light = light_pars[keep, , drop = FALSE])
}

predict_partition <- function(target, temp_fit, light_fit, boots) {
  temp_coef <- coef(temp_fit$fit)
  reco <- exp(temp_coef[[1]] + temp_coef[[2]] * target$TA_MODEL)
  gpp_model <- gpp_curve(target$SW_MODEL, light_fit$par[["alpha"]], light_fit$par[["pmax"]])
  nee_model <- reco - gpp_model

  nee_gapfilled <- fifelse(is.finite(target$NEE_OBS), target$NEE_OBS, nee_model)
  gapfilled_flag <- !is.finite(target$NEE_OBS)
  gpp <- pmax(reco - nee_gapfilled, 0)

  nb <- nrow(boots$temp)
  reco_boot <- matrix(NA_real_, nrow = nrow(target), ncol = nb)
  gpp_model_boot <- matrix(NA_real_, nrow = nrow(target), ncol = nb)
  nee_model_boot <- matrix(NA_real_, nrow = nrow(target), ncol = nb)
  gpp_boot <- matrix(NA_real_, nrow = nrow(target), ncol = nb)

  for (i in seq_len(nb)) {
    reco_i <- exp(boots$temp[i, "intercept"] + boots$temp[i, "ta_slope"] * target$TA_MODEL)
    gpp_i <- gpp_curve(target$SW_MODEL, boots$light[i, "alpha"], boots$light[i, "pmax"])
    nee_i <- reco_i - gpp_i
    nee_gap_i <- ifelse(is.finite(target$NEE_OBS), target$NEE_OBS, nee_i)
    reco_boot[, i] <- reco_i
    gpp_model_boot[, i] <- gpp_i
    nee_model_boot[, i] <- nee_i
    gpp_boot[, i] <- pmax(reco_i - nee_gap_i, 0)
  }

  qfun <- function(x, p) as.numeric(stats::quantile(x, p, na.rm = TRUE, names = FALSE))
  target[, `:=`(
    NEE_gapfilled = nee_gapfilled,
    NEE_gapfill_flag = gapfilled_flag,
    NEE_model = nee_model,
    Reco = reco,
    GPP = gpp,
    GPP_model = gpp_model,
    NEE_model_sd = apply(nee_model_boot, 1, sd, na.rm = TRUE),
    NEE_model_q025 = apply(nee_model_boot, 1, qfun, p = 0.025),
    NEE_model_q975 = apply(nee_model_boot, 1, qfun, p = 0.975),
    Reco_sd = apply(reco_boot, 1, sd, na.rm = TRUE),
    Reco_q025 = apply(reco_boot, 1, qfun, p = 0.025),
    Reco_q975 = apply(reco_boot, 1, qfun, p = 0.975),
    GPP_sd = apply(gpp_boot, 1, sd, na.rm = TRUE),
    GPP_q025 = apply(gpp_boot, 1, qfun, p = 0.025),
    GPP_q975 = apply(gpp_boot, 1, qfun, p = 0.975),
    n_boot = nb
  )]
  target
}

outputs <- list()
diagnostics <- list()

for (p in unique(target_months$period)) {
  train <- dt[period == p]
  target_periods <- target_months[period == p, .(year, month)]
  target <- dt[target_periods, on = .(year, month), nomatch = 0]

  tf <- fit_temp(train)
  lf <- fit_light(train, coef(tf$fit))
  boots <- bootstrap_models(tf, lf, boot_n)
  pred <- predict_partition(copy(target), tf, lf, boots)
  outputs[[p]] <- pred

  tc <- coef(tf$fit)
  diagnostics[[p]] <- data.table(
    period = p,
    train_start = as.character(min(train$timestamp_start, na.rm = TRUE)),
    train_end = as.character(max(train$timestamp_end, na.rm = TRUE)),
    n_train = nrow(train),
    n_night_temp = nrow(tf$data),
    n_day_light = nrow(lf$data),
    temp_intercept = tc[[1]],
    temp_ta_slope = tc[[2]],
    temp_rmse = tf$sigma,
    light_alpha = lf$par[["alpha"]],
    light_pmax = lf$par[["pmax"]],
    light_rmse = lf$sigma,
    n_boot_success = nrow(boots$temp)
  )
}

out <- rbindlist(outputs, use.names = TRUE, fill = TRUE)
setorder(out, timestamp_start)

out_export <- out[, .(
  site_id = "US-Skr",
  timestamp_start = format(timestamp_start, "%Y-%m-%d %H:%M:%S"),
  timestamp_end = format(timestamp_end, "%Y-%m-%d %H:%M:%S"),
  timestamp_mid = format(timestamp_mid, "%Y-%m-%d %H:%M:%S"),
  year, month, doy, hour, minute, season, season_year,
  NEE_observed = NEE_OBS,
  NEE_source,
  NEE_PI,
  FC,
  SC,
  NEE_gapfilled,
  NEE_gapfill_flag,
  NEE_model,
  NEE_model_sd,
  NEE_model_q025,
  NEE_model_q975,
  Reco,
  Reco_sd,
  Reco_q025,
  Reco_q975,
  GPP,
  GPP_sd,
  GPP_q025,
  GPP_q975,
  GPP_model,
  TA = TA_1_1_1,
  TA_model = TA_MODEL,
  SW_IN,
  SW_IN_model = SW_MODEL,
  driver_gapfill_flag,
  USTAR,
  n_boot,
  time_basis = "AmeriFlux timestamp parsed as site local standard clock time; no daylight-saving adjustment applied",
  method = "season-year nighttime exponential NEE-temperature respiration; daytime rectangular-hyperbola light response using SW_IN; bootstrap parameter uncertainty",
  ameriflux_source = "AmeriFlux BASE US-Skr Shark River Slough (Tower SRS-6) Everglades, Ver. 2-5, DOI 10.17190/AMF/1246105"
)]

fwrite(out_export, outfile)
fwrite(rbindlist(diagnostics), diagfile)

cat("Wrote", nrow(out_export), "half-hourly records to", outfile, "\n")
cat("Wrote model diagnostics to", diagfile, "\n")
