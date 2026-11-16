args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) setwd(args[1])
library(tidyverse)

df <- readRDS("calibration_data.rds") %>%
  filter(!is.na(trade_price), !is.na(result)) %>%
  mutate(won = as.integer(result == "yes"))

cat("=== Calibration cut by THRESHOLD band (not price band) ===\n")
cat("This conditions on how extreme the point-margin threshold actually is,\n")
cat("instead of mixing a near-even-money strike that became likely late in a\n")
cat("close game with a 35+ blowout strike that only existed because the game\n")
cat("was already a rout. Same test (mean price vs realized rate), different axis.\n\n")

by_thresh <- df %>%
  mutate(thresh_bucket = cut(threshold, c(0, 5, 10, 15, 20, 25, 30, 40, 80))) %>%
  group_by(thresh_bucket) %>%
  summarise(n = n(), mean_price = mean(trade_price), realized_rate = mean(won),
            median_minutes_to_trade = median(minutes_to_trade, na.rm = TRUE), .groups = "drop") %>%
  filter(n > 0) %>%
  rowwise() %>%
  mutate(
    binom_p = binom.test(round(realized_rate * n), n, p = mean_price)$p.value,
    edge = realized_rate - mean_price
  ) %>%
  ungroup()
print(as.data.frame(by_thresh), row.names = FALSE, digits = 3)

cat("\n=== Within the 30+ point threshold band specifically, cut by price too ===\n")
extreme <- df %>% filter(threshold >= 30)
cat(sprintf("n = %d, mean_price = %.3f, realized_rate = %.3f, median minutes_to_trade = %.1f\n",
            nrow(extreme), mean(extreme$trade_price), mean(extreme$won), median(extreme$minutes_to_trade, na.rm=TRUE)))
bt <- binom.test(sum(extreme$won), nrow(extreme), p = mean(extreme$trade_price))
cat(sprintf("binom p-value (realized vs mean price): %.4f\n", bt$p.value))
