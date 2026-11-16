args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) setwd(args[1])
library(tidyverse)

df <- readRDS("calibration_v2_KXNBASPREAD.rds") %>%
  filter(!is.na(snapshot_price), !is.na(result)) %>%
  mutate(won = as.integer(result == "yes"))

cat(sprintf("Usable observations: %d\n", nrow(df)))
cat(sprintf("Distinct games: %d\n", n_distinct(df$event_ticker)))
cat(sprintf("Threshold range: %.1f to %.1f\n", min(df$threshold), max(df$threshold)))
cat(sprintf("Overall realized win rate: %.3f | Overall mean snapshot price: %.3f\n",
            mean(df$won), mean(df$snapshot_price)))
cat("\nBy snapshot method:\n")
print(df %>% group_by(snapshot_method) %>%
  summarise(n = n(), mean_price = mean(snapshot_price), realized_rate = mean(won), .groups = "drop"))

bin_edges <- c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.07, 0.10, 0.15, 0.20,
               0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)

calib <- df %>%
  mutate(price_bin = cut(snapshot_price, breaks = bin_edges, include.lowest = TRUE)) %>%
  group_by(price_bin) %>%
  summarise(n = n(), mean_price = mean(snapshot_price), realized_rate = mean(won),
            mean_threshold = mean(threshold), .groups = "drop") %>%
  filter(n > 0) %>%
  rowwise() %>%
  mutate(binom_p = binom.test(round(realized_rate * n), n, p = mean_price)$p.value,
         edge = realized_rate - mean_price) %>%
  ungroup()

cat("\n=== Price-stratified calibration (quote-based, preseason excluded) ===\n")
print(as.data.frame(calib), row.names = FALSE, digits = 3)

cat("\n=== Same, restricted to pregame_fixed snapshots only ===\n")
calib_pre <- df %>% filter(snapshot_method == "pregame_fixed") %>%
  mutate(price_bin = cut(snapshot_price, breaks = bin_edges, include.lowest = TRUE)) %>%
  group_by(price_bin) %>%
  summarise(n = n(), mean_price = mean(snapshot_price), realized_rate = mean(won), .groups = "drop") %>%
  filter(n > 0) %>%
  rowwise() %>%
  mutate(binom_p = binom.test(round(realized_rate * n), n, p = mean_price)$p.value,
         edge = realized_rate - mean_price) %>% ungroup()
print(as.data.frame(calib_pre), row.names = FALSE, digits = 3)

cat("\n=== Same, restricted to first_stable_quote (dynamically-created tail brackets) ===\n")
calib_tail <- df %>% filter(snapshot_method == "first_stable_quote") %>%
  mutate(price_bin = cut(snapshot_price, breaks = bin_edges, include.lowest = TRUE)) %>%
  group_by(price_bin) %>%
  summarise(n = n(), mean_price = mean(snapshot_price), realized_rate = mean(won), .groups = "drop") %>%
  filter(n > 0) %>%
  rowwise() %>%
  mutate(binom_p = binom.test(round(realized_rate * n), n, p = mean_price)$p.value,
         edge = realized_rate - mean_price) %>% ungroup()
print(as.data.frame(calib_tail), row.names = FALSE, digits = 3)

cat("\nDone.\n")
