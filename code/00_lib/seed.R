# Deterministic, order-independent random-number seeds.
# seed_for(key) maps a text key (e.g. "SRS5 Oct 2022") to a fixed integer, so a
# Monte Carlo or bootstrap block gives the same draws however many other blocks
# ran before it or in what order. Bootstrap helpers reset the seed per call.
seed_for <- function(key, base = 42L) {
  v <- utf8ToInt(paste(key, collapse = "|"))
  as.integer((base + sum(v * seq_along(v))) %% .Machine$integer.max)
}
