args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) setwd(args[1])
library(tidyverse)

df <- readRDS("calibration_data.rds") %>%
  filter(!is.na(trade_price), !is.na(result)) %>%
  mutate(won = as.integer(result == "yes"))

cat(sprintf("Usable observations: %d\n", nrow(df)))
cat(sprintf("Threshold range in sample: %.1f to %.1f pts\n", min(df$threshold), max(df$threshold)))
cat(sprintf("Overall realized win rate: %.3f | Overall mean first-trade price: %.3f\n",
            mean(df$won), mean(df$trade_price)))
cat(sprintf("Median minutes-to-first-trade: %.1f\n\n", median(df$minutes_to_trade, na.rm = TRUE)))

sanity <- df %>% summarise(n_sub_02 = sum(trade_price < 0.02), n_above_90 = sum(trade_price > 0.90))
cat(sprintf("Sanity: %d obs with price<0.02, %d obs with price>0.90 (should be a real spread, not all clustered)\n\n",
            sanity$n_sub_02, sanity$n_above_90))

# --- Core calibration table ---
bin_edges <- c(0, 0.02, 0.05, 0.10, 0.15, 0.20, 0.30, 0.40, 0.50, 0.65, 0.80, 1.00)
df <- df %>% mutate(price_bin = cut(trade_price, breaks = bin_edges, include.lowest = TRUE))

calib <- df %>%
  group_by(price_bin) %>%
  summarise(n = n(), mean_price = mean(trade_price), realized_rate = mean(won), .groups = "drop") %>%
  filter(n > 0) %>%
  rowwise() %>%
  mutate(
    binom_p = binom.test(round(realized_rate * n), n, p = mean_price)$p.value,
    edge = realized_rate - mean_price
  ) %>%
  ungroup()

cat("=== Calibration: realized win rate vs. first-trade price, by price bin (all strikes) ===\n\n")
print(as.data.frame(calib), row.names = FALSE, digits = 3)

cat("\n=== Same, restricted to threshold >= 15 pts (the actual straddle legs) ===\n")
calib_tail <- df %>%
  filter(threshold >= 15) %>%
  group_by(price_bin) %>%
  summarise(n = n(), mean_price = mean(trade_price), realized_rate = mean(won), .groups = "drop") %>%
  filter(n > 0) %>%
  rowwise() %>%
  mutate(
    binom_p = binom.test(round(realized_rate * n), n, p = mean_price)$p.value,
    edge = realized_rate - mean_price
  ) %>%
  ungroup()
print(as.data.frame(calib_tail), row.names = FALSE, digits = 3)

# --- Net-of-fee P&L per $1 staked at the traded price (approximates buying at the ask) ---
cat("\n=== Net P&L per $1 staked, net of 7% fee on profit (threshold >= 15 pts) ===\n")
KALSHI_FEE <- 0.07
pnl_tail <- df %>%
  filter(threshold >= 15) %>%
  group_by(price_bin) %>%
  summarise(
    n = n(), mean_price = mean(trade_price), realized_rate = mean(won),
    avg_pnl_per_dollar = mean(ifelse(won == 1,
      (1 / trade_price) * (1 - KALSHI_FEE * (1 - trade_price)) - 1, -1)),
    .groups = "drop"
  ) %>% filter(n > 0)
print(as.data.frame(pnl_tail), row.names = FALSE, digits = 3)

cat("\nDone.\n")
