args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) setwd(args[1])
library(tidyverse)

df <- readRDS("calibration_data.rds") %>%
  filter(!is.na(trade_price), !is.na(result)) %>%
  mutate(won = as.integer(result == "yes"))

# Finer bins, especially at the cheap end where the thesis lives.
bin_edges <- c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.07, 0.10, 0.15, 0.20,
               0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)

by_price <- df %>%
  mutate(price_bin = cut(trade_price, breaks = bin_edges, include.lowest = TRUE)) %>%
  group_by(price_bin) %>%
  summarise(
    n = n(),
    mean_price = mean(trade_price),
    realized_rate = mean(won),
    mean_threshold = mean(threshold),
    median_minutes_to_trade = median(minutes_to_trade, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n > 0) %>%
  rowwise() %>%
  mutate(
    binom_p = binom.test(round(realized_rate * n), n, p = mean_price)$p.value,
    edge = realized_rate - mean_price
  ) %>%
  ungroup()

cat("=== Stratified by first-trade PRICE (fine bins), with composition diagnostics ===\n")
cat("mean_threshold and median_minutes_to_trade are shown so a price bin's\n")
cat("composition (e.g. 'low threshold, late in a close game' vs 'high\n")
cat("threshold, blowout already locked in') can be read directly.\n\n")
print(as.data.frame(by_price %>%
  select(price_bin, n, mean_price, realized_rate, edge, binom_p,
         mean_threshold, median_minutes_to_trade)),
  row.names = FALSE, digits = 3)
