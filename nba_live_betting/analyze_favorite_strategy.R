# Single-leg "buy the favorite" backtest: instead of a multi-leg strip, just
# buy YES on legs priced in the favorite range, one contract each, real
# quoted ask, real 7% fee on profit. Checks whether the edge found in
# calibration (70-90c underpriced) survives real spread + fee costs, and
# reports the actual observed spread on these specific legs (which may
# differ from the quarter-longshot legs tested earlier).
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) setwd(args[1])
library(tidyverse)

all_series <- c("KXNBASPREAD","KXNBA1QSPREAD","KXNBA2QSPREAD","KXNBA3QSPREAD",
                 "KXNBA4QSPREAD","KXNBA1HSPREAD","KXNBA2HSPREAD")

PRICE_LO <- 0.60
PRICE_HI <- 0.92  # exclude the very top bin (n too thin, 90c+) for stability

run_favorite <- function(series) {
  df <- readRDS(sprintf("calibration_v2_%s.rds", series)) %>%
    filter(!is.na(snapshot_price), !is.na(result),
           snapshot_price >= PRICE_LO, snapshot_price <= PRICE_HI) %>%
    mutate(
      yes_bid = 2 * snapshot_mid - snapshot_price,
      spread = snapshot_price - yes_bid,
      won = as.integer(result == "yes"),
      leg_pnl = ifelse(won == 1, 0.93 * (1 - snapshot_price), -snapshot_price)
    )

  if (nrow(df) < 5) return(tibble(series = series, n = nrow(df)))

  bt <- binom.test(sum(df$won), nrow(df), p = mean(df$snapshot_price))
  tt <- t.test(df$leg_pnl)

  tibble(
    series = series,
    n = nrow(df),
    mean_price = mean(df$snapshot_price),
    realized_rate = mean(df$won),
    edge_pp = (mean(df$won) - mean(df$snapshot_price)) * 100,
    binom_p = bt$p.value,
    mean_spread = mean(df$spread),
    total_pnl = sum(df$leg_pnl),
    avg_pnl_per_leg = mean(df$leg_pnl),
    return_pct = sum(df$leg_pnl) / sum(df$snapshot_price) * 100,
    pnl_p_value = tt$p.value
  )
}

results <- map_dfr(all_series, run_favorite)
cat(sprintf("=== Buy-the-favorite backtest, price in [%.2f, %.2f] ===\n\n", PRICE_LO, PRICE_HI))
print(as.data.frame(results), row.names = FALSE, digits = 3)

cat("\n=== Pooled across all 7 series ===\n")
pooled <- map_dfr(all_series, function(s) {
  readRDS(sprintf("calibration_v2_%s.rds", s)) %>%
    filter(!is.na(snapshot_price), !is.na(result),
           snapshot_price >= PRICE_LO, snapshot_price <= PRICE_HI) %>%
    mutate(yes_bid = 2*snapshot_mid - snapshot_price, spread = snapshot_price - yes_bid,
           won = as.integer(result == "yes"),
           leg_pnl = ifelse(won==1, 0.93*(1-snapshot_price), -snapshot_price), series = s)
})
cat(sprintf("n=%d games-worth of legs, mean_price=%.3f, realized=%.3f, edge=%.2fpp\n",
            nrow(pooled), mean(pooled$snapshot_price), mean(pooled$won),
            (mean(pooled$won)-mean(pooled$snapshot_price))*100))
cat(sprintf("mean spread=%.3f, total_pnl=$%.2f, return=%.1f%%\n",
            mean(pooled$spread), sum(pooled$leg_pnl), sum(pooled$leg_pnl)/sum(pooled$snapshot_price)*100))
bt_p <- binom.test(sum(pooled$won), nrow(pooled), p = mean(pooled$snapshot_price))
tt_p <- t.test(pooled$leg_pnl)
cat(sprintf("binom p=%.4f, t-test p=%.4f\n", bt_p$p.value, tt_p$p.value))
