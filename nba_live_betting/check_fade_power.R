library(tidyverse)
state <- readRDS("data/trading_state.rds")
fade <- state$positions %>% filter(strategy == "fade", status %in% c("settled_win","settled_loss"))
cat("n fade trades:", nrow(fade), "\n")
cat("wins:", sum(fade$status == "settled_win"), "\n")
cat("win rate:", mean(fade$status == "settled_win"), "\n")
cat("total pnl: $", sum(fade$pnl), "\n\n")

n <- nrow(fade); wins <- sum(fade$status == "settled_win")
bt <- binom.test(wins, n, p = 0.53)
cat("binom.test vs p=0.53: p-value =", bt$p.value, "\n")
cat("95% CI on true win rate:", bt$conf.int[1], "-", bt$conf.int[2], "\n\n")

# power check: if true rate IS 53%, how often would n=8 trials produce
# a win rate this low or lower just by chance?
cat("P(X <=", wins, "| n=8, p=0.53):", pbinom(wins, n, 0.53), "\n")

# how many trades would be needed to reliably distinguish 53% from, say,
# the documented alternative of a losing strategy (~40%)?
cat("\nSample size needed for 80% power to distinguish 53% vs 40% (two-sided, alpha=0.05):\n")
print(power.prop.test(p1 = 0.53, p2 = 0.40, power = 0.8))
