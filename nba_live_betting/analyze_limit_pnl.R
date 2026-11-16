args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) setwd(args[1])
library(tidyverse)

df <- readRDS("limit_fills_KXNBA1QSPREAD.rds")
cat(sprintf("n legs: %d\n\n", nrow(df)))

# target=yes_bid is equivalent to buying NO at today's no-ask (no improvement
# — the market-order baseline we already backtested). target=yes_ask is the
# best passive price (full spread captured). The three in between trade off
# fill probability against price improvement.
levels <- tribble(
  ~label,            ~fill_col,            ~frac,
  "at_bid (= market order baseline)", "fill_at_bid",        0.00,
  "quarter_in",                        "fill_quarter_in",    0.25,
  "at_mid",                            "fill_at_mid",        0.50,
  "three_quarter_in",                  "fill_three_quarter", 0.75,
  "at_ask (full spread captured)",     "fill_at_ask",        1.00
)

results <- map_dfr(seq_len(nrow(levels)), function(i) {
  lvl <- levels[i, ]
  filled <- df[df[[lvl$fill_col]], ]
  if (nrow(filled) == 0) {
    return(tibble(label = lvl$label, n_filled = 0, fill_rate = 0, total_pnl = NA,
                  avg_pnl_per_leg = NA, return_pct = NA))
  }
  target <- filled$yes_bid + lvl$frac * filled$spread
  no_cost <- 1 - target
  no_won <- as.integer(filled$result == "no")
  leg_pnl <- ifelse(no_won == 1, 0.93 * target, target - 1)
  tibble(
    label = lvl$label,
    n_filled = nrow(filled),
    fill_rate = nrow(filled) / nrow(df) * 100,
    total_pnl = sum(leg_pnl),
    avg_pnl_per_leg = mean(leg_pnl),
    return_pct = sum(leg_pnl) / sum(no_cost) * 100
  )
})

cat("=== Limit-order short-convexity backtest (buy NO, 1Q spread, threshold>=10) ===\n\n")
print(as.data.frame(results), row.names = FALSE, digits = 3)

cat("\nFor comparison, market-order baseline (same legs, crossing the spread):\n")
mo <- df %>% mutate(no_cost = 1 - yes_bid, no_won = as.integer(result == "no"),
                     leg_pnl = ifelse(no_won == 1, 0.93 * (1 - no_cost), -no_cost))
cat(sprintf("n=%d, total_pnl=$%.2f, return=%.1f%%\n",
            nrow(mo), sum(mo$leg_pnl), sum(mo$leg_pnl)/sum(mo$no_cost)*100))
