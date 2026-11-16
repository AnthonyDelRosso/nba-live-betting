# Backtest the long-convexity strip (buy both tails: "team wins by over k"
# on both sides, k beyond some inner cutoff) using real quoted asks and real
# outcomes from the already-collected pregame-fixed snapshot data. One
# contract per leg, Kalshi's 7% fee applied per-leg on profit only (no fee
# on losing legs), matching real settlement mechanics.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) setwd(args[1])
library(tidyverse)

KALSHI_FEE <- 0.07
INNER_CUTOFF <- 10  # exclude legs within this many points of even — the
                     # "near the money" zone our calibration found roughly
                     # fairly priced; include everything beyond it (the
                     # zone where we found significant overpricing)

run_strip <- function(series) {
  df <- readRDS(sprintf("calibration_v2_%s.rds", series)) %>%
    filter(!is.na(snapshot_price), !is.na(result), threshold >= INNER_CUTOFF) %>%
    mutate(won = as.integer(result == "yes"),
           leg_pnl = ifelse(won == 1, 0.93 * (1 - snapshot_price), -snapshot_price))

  by_game <- df %>%
    group_by(event_ticker) %>%
    summarise(n_legs = n(), cost = sum(snapshot_price), payout_legs = sum(won),
              pnl = sum(leg_pnl), .groups = "drop")

  tibble(
    series = series,
    n_games = nrow(by_game),
    avg_legs_per_game = mean(by_game$n_legs),
    avg_cost_per_game = mean(by_game$cost),
    total_pnl = sum(by_game$pnl),
    avg_pnl_per_game = mean(by_game$pnl),
    pct_games_profitable = mean(by_game$pnl > 0) * 100,
    pnl_sd = sd(by_game$pnl),
    t_stat = t.test(by_game$pnl)$statistic,
    p_value = t.test(by_game$pnl)$p.value
  )
}

all_series <- c("KXNBASPREAD","KXNBA1QSPREAD","KXNBA2QSPREAD","KXNBA3QSPREAD",
                 "KXNBA4QSPREAD","KXNBA1HSPREAD","KXNBA2HSPREAD")

results <- map_dfr(all_series, run_strip)
cat(sprintf("=== Long-convexity strip backtest (legs with threshold >= %d pts) ===\n\n", INNER_CUTOFF))
print(as.data.frame(results), row.names = FALSE, digits = 3)

cat("\n=== Full-game spread detail: per-game P&L distribution ===\n")
fg <- readRDS("calibration_v2_KXNBASPREAD.rds") %>%
  filter(!is.na(snapshot_price), !is.na(result), threshold >= INNER_CUTOFF) %>%
  mutate(won = as.integer(result == "yes"),
         leg_pnl = ifelse(won == 1, 0.93 * (1 - snapshot_price), -snapshot_price))
by_game_fg <- fg %>% group_by(event_ticker) %>%
  summarise(n_legs = n(), cost = sum(snapshot_price), pnl = sum(leg_pnl), .groups = "drop")
cat(sprintf("n games: %d\n", nrow(by_game_fg)))
print(summary(by_game_fg$pnl))
cat(sprintf("\nTotal P&L across all games (1 contract/leg): $%.2f\n", sum(by_game_fg$pnl)))
cat(sprintf("Total cost staked across all games: $%.2f\n", sum(by_game_fg$cost)))
cat(sprintf("Return on capital staked: %.1f%%\n", sum(by_game_fg$pnl)/sum(by_game_fg$cost)*100))

cat("\n=== Same full-game strip, GROSS of fees (fee-free comparison) ===\n")
fg_gross <- readRDS("calibration_v2_KXNBASPREAD.rds") %>%
  filter(!is.na(snapshot_price), !is.na(result), threshold >= INNER_CUTOFF) %>%
  mutate(won = as.integer(result == "yes"),
         leg_pnl_gross = ifelse(won == 1, 1 - snapshot_price, -snapshot_price))
by_game_gross <- fg_gross %>% group_by(event_ticker) %>%
  summarise(pnl_gross = sum(leg_pnl_gross), .groups = "drop")
cat(sprintf("Total P&L gross of fees: $%.2f (vs net-of-fee: $%.2f)\n",
            sum(by_game_gross$pnl_gross), sum(by_game_fg$pnl)))
cat(sprintf("Fee drag alone: $%.2f\n", sum(by_game_fg$pnl) - sum(by_game_gross$pnl_gross)))
