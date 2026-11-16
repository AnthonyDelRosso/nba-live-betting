# Short-convexity backtest: buy NO on the same wing legs instead of YES.
# NO isn't independently quoted on Kalshi — no_ask = 1 - yes_bid exactly
# (verified directly against live market data) — so this is computed from
# the same already-collected snapshot_price (yes ask) / snapshot_mid data,
# no new pull needed. yes_bid is recovered as 2*mid - price.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) setwd(args[1])
library(tidyverse)

KALSHI_FEE <- 0.07
INNER_CUTOFF <- 10

run_strip <- function(series, side = c("yes", "no")) {
  side <- match.arg(side)
  df <- readRDS(sprintf("calibration_v2_%s.rds", series)) %>%
    filter(!is.na(snapshot_price), !is.na(result), threshold >= INNER_CUTOFF) %>%
    mutate(
      yes_bid = 2 * snapshot_mid - snapshot_price,
      yes_won = as.integer(result == "yes")
    )

  if (side == "yes") {
    df <- df %>% mutate(ask = snapshot_price, won = yes_won)
  } else {
    df <- df %>% mutate(ask = 1 - yes_bid, won = 1L - yes_won)
  }

  df <- df %>% mutate(leg_pnl = ifelse(won == 1, 0.93 * (1 - ask), -ask))

  by_game <- df %>%
    group_by(event_ticker) %>%
    summarise(n_legs = n(), cost = sum(ask), pnl = sum(leg_pnl), .groups = "drop")

  tt <- tryCatch(t.test(by_game$pnl), error = function(e) NULL)

  tibble(
    series = series, side = side,
    n_games = nrow(by_game),
    avg_cost_per_game = mean(by_game$cost),
    total_pnl = sum(by_game$pnl),
    avg_pnl_per_game = mean(by_game$pnl),
    pct_profitable = mean(by_game$pnl > 0) * 100,
    return_pct = sum(by_game$pnl) / sum(by_game$cost) * 100,
    p_value = if (!is.null(tt)) tt$p.value else NA
  )
}

all_series <- c("KXNBASPREAD","KXNBA1QSPREAD","KXNBA2QSPREAD","KXNBA3QSPREAD",
                 "KXNBA4QSPREAD","KXNBA1HSPREAD","KXNBA2HSPREAD")

results <- bind_rows(
  map_dfr(all_series, run_strip, side = "yes"),
  map_dfr(all_series, run_strip, side = "no")
) %>% arrange(series, side)

cat(sprintf("=== Long (buy YES wings) vs Short (buy NO wings) convexity, threshold >= %d pts ===\n\n", INNER_CUTOFF))
print(as.data.frame(results), row.names = FALSE, digits = 3)

cat("\n=== Sanity check: YES + NO legs on the same strike should be a risk-free wash minus 2x spread ===\n")
check <- readRDS("calibration_v2_KXNBA1QSPREAD.rds") %>%
  filter(!is.na(snapshot_price), threshold >= INNER_CUTOFF) %>%
  mutate(yes_bid = 2*snapshot_mid - snapshot_price, no_ask = 1 - yes_bid,
         combined_cost = snapshot_price + no_ask)
cat(sprintf("Mean combined cost of buying YES ask + NO ask on the same strike: $%.3f\n", mean(check$combined_cost)))
cat("(Should be > $1.00 by exactly the round-trip spread cost — one of YES/NO always pays $1, the other $0)\n")

cat("\nDone.\n")
