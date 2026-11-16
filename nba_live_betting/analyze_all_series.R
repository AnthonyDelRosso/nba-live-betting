args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) setwd(args[1])
library(tidyverse)

all_series <- c("KXNBASPREAD","KXNBA1QSPREAD","KXNBA2QSPREAD","KXNBA3QSPREAD",
                 "KXNBA4QSPREAD","KXNBA1HSPREAD","KXNBA2HSPREAD")

all_df <- map_dfr(all_series, function(s) {
  readRDS(sprintf("calibration_v2_%s.rds", s)) %>%
    filter(!is.na(snapshot_price), !is.na(result)) %>%
    mutate(won = as.integer(result == "yes"), segment = s)
})

cat(sprintf("Total usable observations across all 7 series: %d\n", nrow(all_df)))
cat(sprintf("Distinct games: %d\n\n", n_distinct(all_df$event_ticker)))

cat("=== Per-series overall calibration ===\n")
print(all_df %>% group_by(segment) %>%
  summarise(n = n(), mean_price = mean(snapshot_price), realized_rate = mean(won),
            edge = realized_rate - mean_price, .groups = "drop"))

bin_edges <- c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.07, 0.10, 0.15, 0.20,
               0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)

cat("\n=== POOLED price-stratified calibration (full-game + all segments) ===\n")
pooled <- all_df %>%
  mutate(price_bin = cut(snapshot_price, breaks = bin_edges, include.lowest = TRUE)) %>%
  group_by(price_bin) %>%
  summarise(n = n(), mean_price = mean(snapshot_price), realized_rate = mean(won), .groups = "drop") %>%
  filter(n > 0) %>%
  rowwise() %>%
  mutate(binom_p = binom.test(round(realized_rate * n), n, p = mean_price)$p.value,
         edge = realized_rate - mean_price) %>%
  ungroup()
print(as.data.frame(pooled), row.names = FALSE, digits = 3)

cat("\n=== Full-game spread only (KXNBASPREAD), for direct comparison to earlier result ===\n")
fg <- all_df %>% filter(segment == "KXNBASPREAD") %>%
  mutate(price_bin = cut(snapshot_price, breaks = bin_edges, include.lowest = TRUE)) %>%
  group_by(price_bin) %>%
  summarise(n = n(), mean_price = mean(snapshot_price), realized_rate = mean(won), .groups = "drop") %>%
  filter(n > 0) %>%
  rowwise() %>%
  mutate(binom_p = binom.test(round(realized_rate * n), n, p = mean_price)$p.value,
         edge = realized_rate - mean_price) %>% ungroup()
print(as.data.frame(fg), row.names = FALSE, digits = 3)

cat("\nDone.\n")
