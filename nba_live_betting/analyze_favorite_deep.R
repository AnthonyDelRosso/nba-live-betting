# Deep dive on the full-game (KXNBASPREAD) favorite edge found in
# analyze_favorite_strategy.R (+6.3pp, p=0.013 at leg level).
#
# Concern: legs are NOT independent — one game contributes up to 11 nested
# strikes on the same team ("wins by >1.5", ">3.5", ...), so the leg-level
# t-test overstates significance. Everything here is evaluated at the GAME
# level (cluster), plus:
#   1. Game-clustered P&L inference + cluster bootstrap
#   2. Market-implied median margin vs realized margin (one number per game)
#   3. Decomposition: price bin, threshold, favorite size, home/away
#   4. Stability over time + a chronological out-of-sample split
#   5. Executable one-leg-per-game rules
#   6. Fee-model sensitivity (repo's 7%-of-profit vs Kalshi's
#      ceil(0.07*C*P*(1-P)) per-trade schedule)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) setwd(args[1])
suppressMessages(library(tidyverse))
set.seed(42)

PRICE_LO <- 0.60; PRICE_HI <- 0.92

raw <- readRDS("calibration_v2_KXNBASPREAD.rds") %>%
  filter(game_date >= "2026-10-20", !is.na(result)) %>%
  mutate(team = str_extract(ticker, "(?<=-)[A-Z]+(?=\\d+$)"),
         won = as.integer(result == "yes"),
         is_home = team == home_team)

stopifnot(!any(is.na(raw$team)))

# ---------- Realized margin per game, bracketed from strike results ----------
# For team T's ladder: result yes  <=> T margin > k.
margin <- raw %>%
  group_by(event_ticker, team) %>%
  summarise(lo = suppressWarnings(max(threshold[won == 1])),
            hi = suppressWarnings(min(threshold[won == 0])), .groups = "drop") %>%
  filter(is.finite(lo)) %>%               # this team won by > lo
  mutate(margin_est = ifelse(is.finite(hi), (lo + hi) / 2, lo + 1)) %>%
  select(event_ticker, winner = team, margin_est)

# ---------- Favorite legs (pregame_fixed only: tradable 60 min pre-tip) ----------
fav <- raw %>%
  filter(snapshot_method == "pregame_fixed", !is.na(snapshot_price),
         snapshot_price >= PRICE_LO, snapshot_price <= PRICE_HI) %>%
  mutate(yes_bid = 2 * snapshot_mid - snapshot_price,
         spread  = snapshot_price - yes_bid,
         pnl_repo   = ifelse(won == 1, 0.93 * (1 - snapshot_price), -snapshot_price),
         fee_kalshi = ceiling(0.07 * snapshot_price * (1 - snapshot_price) * 100) / 100,
         pnl_kalshi = won - snapshot_price - fee_kalshi,
         pnl_gross  = won - snapshot_price)

cat(sprintf("Favorite legs (pregame_fixed, %.2f-%.2f): %d legs across %d games\n",
            PRICE_LO, PRICE_HI, nrow(fav), n_distinct(fav$event_ticker)))
cat(sprintf("Teams per game with favorite legs: max %d (should be 1)\n",
            max(count(distinct(fav, event_ticker, team), event_ticker)$n)))

# ---------- 1. Leg-level vs game-clustered inference ----------
game_pnl <- fav %>% group_by(event_ticker, game_date) %>%
  summarise(n_legs = n(), cost = sum(snapshot_price),
            pnl_repo = sum(pnl_repo), pnl_kalshi = sum(pnl_kalshi),
            pnl_gross = sum(pnl_gross), wins = sum(won),
            exp_wins = sum(snapshot_price), .groups = "drop")

cat("\n=== 1. Inference: leg-level (naive) vs game-clustered ===\n")
cat(sprintf("Leg-level t-test p (naive):          %.4f\n", t.test(fav$pnl_repo)$p.value))
cat(sprintf("Game-level t-test on per-game P&L p:  %.4f  (n=%d games)\n",
            t.test(game_pnl$pnl_repo)$p.value, nrow(game_pnl)))
boot <- replicate(10000, {
  g <- game_pnl[sample(nrow(game_pnl), replace = TRUE), ]
  c(ret = sum(g$pnl_repo) / sum(g$cost), edge = (sum(g$wins) - sum(g$exp_wins)) / sum(g$n_legs))
})
cat(sprintf("Cluster bootstrap return: %.1f%%  95%% CI [%.1f%%, %.1f%%]  P(ret<=0)=%.3f\n",
            100 * sum(game_pnl$pnl_repo) / sum(game_pnl$cost),
            100 * quantile(boot["ret", ], .025), 100 * quantile(boot["ret", ], .975),
            mean(boot["ret", ] <= 0)))
cat(sprintf("Cluster bootstrap edge:   %.2fpp 95%% CI [%.2f, %.2f]\n",
            100 * (sum(game_pnl$wins) - sum(game_pnl$exp_wins)) / sum(game_pnl$n_legs),
            100 * quantile(boot["edge", ], .025), 100 * quantile(boot["edge", ], .975)))
cat(sprintf("Games profitable: %.1f%%\n", 100 * mean(game_pnl$pnl_repo > 0)))

# ---------- 2. Market-implied median margin vs realized ----------
# Build the favorite's survival curve P(fav margin > k) from pregame mids on
# the favorite's own ladder; the implied median is where it crosses 0.5.
ladder <- raw %>%
  filter(snapshot_method == "pregame_fixed", !is.na(snapshot_mid)) %>%
  semi_join(distinct(fav, event_ticker, team), by = c("event_ticker", "team")) %>%
  arrange(event_ticker, threshold)

implied <- ladder %>% group_by(event_ticker, team, home_team) %>%
  summarise(implied_median = {
              p <- cummin(snapshot_mid); k <- threshold   # enforce monotone
              if (max(p) < 0.5 || min(p) > 0.5) NA_real_
              else approx(p, k, xout = 0.5, ties = mean)$y },
            .groups = "drop") %>%
  left_join(margin, by = "event_ticker") %>%
  # No strike resolved yes on either side => |margin| <= 1 (incl. OT-free ties
  # impossible, so effectively a 1-pt game): treat as 0 rather than dropping.
  mutate(fav_margin = case_when(is.na(winner) ~ 0,
                                winner == team ~ margin_est, TRUE ~ -margin_est),
         beat = fav_margin - implied_median) %>%
  filter(!is.na(implied_median), !is.na(fav_margin))

cat("\n=== 2. Market-implied favorite margin vs realized (one obs per game) ===\n")
cat(sprintf("n=%d games | mean implied fav margin %.2f | mean realized %.2f\n",
            nrow(implied), mean(implied$implied_median), mean(implied$fav_margin)))
tt <- t.test(implied$beat)
cat(sprintf("Favorite beat implied median by %.2f pts on average (95%% CI %.2f to %.2f, p=%.4f)\n",
            mean(implied$beat), tt$conf.int[1], tt$conf.int[2], tt$p.value))
cat(sprintf("Covered implied median: %.1f%% of games (binom p=%.4f vs 50%%)\n",
            100 * mean(implied$beat > 0),
            binom.test(sum(implied$beat > 0), sum(implied$beat != 0))$p.value))
cat(sprintf("SD of realized minus implied: %.2f pts\n", sd(implied$beat)))

# ---------- 3. Decomposition (game-clustered where it matters) ----------
dec <- function(d, var) {
  d %>% group_by({{ var }}) %>%
    summarise(legs = n(), games = n_distinct(event_ticker),
              mean_price = mean(snapshot_price), realized = mean(won),
              edge_pp = 100 * (realized - mean_price),
              ret_pct = 100 * sum(pnl_repo) / sum(snapshot_price), .groups = "drop")
}
fav <- fav %>% left_join(select(implied, event_ticker, implied_median), by = "event_ticker") %>%
  mutate(price_bin = cut(snapshot_price, c(.6, .7, .8, .92), include.lowest = TRUE),
         thr_bin = cut(threshold, c(0, 4, 8, 12, 60)),
         fav_size = cut(implied_median, c(-Inf, 4, 8, Inf), labels = c("small(<4)", "mid(4-8)", "big(8+)")),
         venue = ifelse(is_home, "home fav", "road fav"),
         week = paste0("wk", as.integer(floor((as.Date(game_date) - as.Date("2026-10-20")) / 7)) + 1))

cat("\n=== 3a. By price bin ===\n");    print(as.data.frame(dec(fav, price_bin)), row.names = FALSE, digits = 3)
cat("\n=== 3b. By strike threshold ===\n"); print(as.data.frame(dec(fav, thr_bin)), row.names = FALSE, digits = 3)
cat("\n=== 3c. By favorite size (implied median margin) ===\n"); print(as.data.frame(dec(fav, fav_size)), row.names = FALSE, digits = 3)
cat("\n=== 3d. Home vs road favorite ===\n"); print(as.data.frame(dec(fav, venue)), row.names = FALSE, digits = 3)
cat("\nImplied-margin miss by venue (game level):\n")
print(implied %>% mutate(venue = ifelse(team == home_team, "home fav", "road fav")) %>%
        group_by(venue) %>% summarise(games = n(), mean_beat = mean(beat),
                                      p = t.test(beat)$p.value, .groups = "drop"))

# ---------- 4. Stability ----------
cat("\n=== 4a. By week ===\n"); print(as.data.frame(dec(fav, week)), row.names = FALSE, digits = 3)
dates <- sort(unique(game_pnl$game_date)); cut_date <- dates[ceiling(length(dates) / 2)]
cat(sprintf("\n=== 4b. Chronological split at %s ===\n", cut_date))
for (half in c("first", "second")) {
  g <- if (half == "first") filter(game_pnl, game_date <= cut_date) else filter(game_pnl, game_date > cut_date)
  im <- implied %>% semi_join(g, by = "event_ticker")
  cat(sprintf("%-6s half: %3d games | return %6.1f%% | game-level p=%.3f | fav beat implied by %.2f pts (p=%.3f)\n",
              half, nrow(g), 100 * sum(g$pnl_repo) / sum(g$cost), t.test(g$pnl_repo)$p.value,
              mean(im$beat), t.test(im$beat)$p.value))
}

# ---------- 5. One leg per game rules ----------
cat("\n=== 5. One leg per game (removes within-game stacking) ===\n")
rule <- function(d, label, chooser) {
  one <- d %>% group_by(event_ticker) %>% slice(chooser(dplyr::pick(everything()))) %>% ungroup()
  tibble(rule = label, games = nrow(one), mean_price = mean(one$snapshot_price),
         realized = mean(one$won), edge_pp = 100 * (mean(one$won) - mean(one$snapshot_price)),
         ret_repo = 100 * sum(one$pnl_repo) / sum(one$snapshot_price),
         ret_kalshi = 100 * sum(one$pnl_kalshi) / sum(one$snapshot_price),
         p = t.test(one$pnl_repo)$p.value)
}
rules <- bind_rows(
  rule(fav, "closest to 70c",       function(x) which.min(abs(x$snapshot_price - 0.70))),
  rule(fav, "closest to 75c",       function(x) which.min(abs(x$snapshot_price - 0.75))),
  rule(fav, "lowest strike (safest)", function(x) which.min(x$threshold)),
  rule(fav, "highest strike in band", function(x) which.max(x$threshold))
)
print(as.data.frame(rules), row.names = FALSE, digits = 3)

cat("\n=== 5b. Big-favorite subset (implied margin 8+), game-level ===\n")
big_g <- fav %>% filter(fav_size == "big(8+)") %>% group_by(event_ticker) %>%
  summarise(pnl = sum(pnl_repo), cost = sum(snapshot_price), .groups = "drop")
cat(sprintf("%d games | return %.1f%% | game-level p=%.4f\n", nrow(big_g),
            100 * sum(big_g$pnl) / sum(big_g$cost), t.test(big_g$pnl)$p.value))
print(as.data.frame(rule(filter(fav, fav_size == "big(8+)"), "big fav, closest to 70c",
                         function(x) which.min(abs(x$snapshot_price - 0.70)))), row.names = FALSE, digits = 3)
im_big <- implied %>% filter(implied_median >= 8)
cat(sprintf("Big favs beat implied by %.2f pts (p=%.3f, n=%d); mid favs (4-8): %.2f pts (p=%.3f, n=%d)\n",
            mean(im_big$beat), t.test(im_big$beat)$p.value, nrow(im_big),
            mean(filter(implied, implied_median < 8)$beat), t.test(filter(implied, implied_median < 8)$beat)$p.value,
            nrow(filter(implied, implied_median < 8))))
cat("Big-fav split by chronological half:\n")
print(fav %>% filter(fav_size == "big(8+)") %>% mutate(half = ifelse(game_date <= cut_date, "first", "second")) %>%
        group_by(half) %>% summarise(games = n_distinct(event_ticker), legs = n(),
                                     ret_pct = 100 * sum(pnl_repo) / sum(snapshot_price), .groups = "drop"))

# ---------- 6. Fee sensitivity + costs ----------
cat("\n=== 6. Fee model sensitivity (all favorite legs) ===\n")
cat(sprintf("Gross (no fee):                       %+.2f  (%.1f%%)\n", sum(fav$pnl_gross),  100 * sum(fav$pnl_gross)  / sum(fav$snapshot_price)))
cat(sprintf("Repo model (7%% of profit on wins):    %+.2f  (%.1f%%)\n", sum(fav$pnl_repo),   100 * sum(fav$pnl_repo)   / sum(fav$snapshot_price)))
cat(sprintf("Kalshi ceil(.07*P*(1-P)), 1 contract: %+.2f  (%.1f%%)\n", sum(fav$pnl_kalshi), 100 * sum(fav$pnl_kalshi) / sum(fav$snapshot_price)))
fav <- fav %>% mutate(pnl_k100 = won - snapshot_price - 0.07 * snapshot_price * (1 - snapshot_price))
cat(sprintf("Kalshi fee at size (rounding amortized): %+.2f  (%.1f%%)\n", sum(fav$pnl_k100), 100 * sum(fav$pnl_k100) / sum(fav$snapshot_price)))
cat(sprintf("Mean quoted spread on these legs: %.3f | buying at mid instead of ask would add %.1f%% return\n",
            mean(fav$spread), 100 * sum(fav$snapshot_price - fav$snapshot_mid) / sum(fav$snapshot_price)))
cat(sprintf("Median market total volume on these legs: %.0f contracts\n", median(fav$volume_fp)))

saveRDS(list(fav = fav, game_pnl = game_pnl, implied = implied), "favorite_deep_results.rds")
cat("\nDone.\n")
