# DIC from pH + total alkalinity (seacarb::carb, flag = 8), as used for the
# porewater figures. Probe pH is treated as total scale; alkalinity umol/L is
# taken as umol/kg. Returns DIC in umol/L (NA where inputs are missing).
add_dic <- function(df) {
  df$DIC_uM <- NA_real_
  if (!requireNamespace("seacarb", quietly = TRUE)) return(df)
  ok <- !is.na(df$pH) & !is.na(df$Alkalinity_uM) & !is.na(df$PSU) & !is.na(df$TempC)
  if (any(ok)) {
    cb <- seacarb::carb(flag = 8, var1 = df$pH[ok], var2 = df$Alkalinity_uM[ok] / 1e6,
                        S = df$PSU[ok], T = df$TempC[ok], pHscale = "T", warn = "n")
    df$DIC_uM[ok] <- cb$DIC * 1e6
  }
  df
}
