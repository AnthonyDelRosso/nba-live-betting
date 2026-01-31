# Fade Re-entry Caps Backtest
# Tests different limits on number of fade positions per game

library(tidyverse)
library(hoopR)

message("\n=== FADE RE-ENTRY CAPS BACKTEST ===\n")

# ------------------------------------------------------------------------------
# Actual Blown Lead Rates
# ------------------------------------------------------------------------------

get_actual_blown_rate <- function(lead, period) {
  if (period == 1) {
    case_when(
      lead >= 25 ~ 0.017, lead >= 21 ~ 0.095, lead >= 18 ~ 0.153,
      lead >= 15 ~ 0.183, lead >= 12 ~ 0.253, lead >= 10 ~ 0.288,
      lead >= 7 ~ 0.340, lead >= 5 ~ 0.389, TRUE ~ 0.443
    )
  } else if (period == 2) {
    case_when(
      lead >= 25 ~ 0.024, lead >= 21 ~ 0.056, lead >= 18 ~ 0.103,
      lead >= 15 ~ 0.145, lead >= 12 ~ 0.195, lead >= 10 ~ 0.253,
      lead >= 7 ~ 0.303, lead >= 5 ~ 0.362, TRUE ~ 0.433
    )
  } else if (period == 3) {
    case_when(
      lead >= 25 ~ 0.003, lead >= 21 ~ 0.024, lead >= 18 ~ 0.045,
      lead >= 15 ~ 0.076, lead >= 12 ~ 0.126, lead >= 10 ~ 0.191,
      lead >= 7 ~ 0.255, lead >= 5 ~ 0.335, TRUE ~ 0.416
    )
  } else {
    case_when(
      lead >= 25 ~ 0.000, lead >= 21 ~ 0.001, lead >= 18 ~ 0.004,
      lead >= 15 ~ 0.013, lead >= 12 ~ 0.033, lead >= 10 ~ 0.058,
      lead >= 7 ~ 0.114, lead >= 5 ~ 0.172, TRUE ~ 0.297
    )
  }
}

calc_breakeven_blown <- function(trailer_price) {
  trailer_price / (trailer_price + (1 - trailer_price) * 0.93)
}

# ------------------------------------------------------------------------------
# Load Data
# ------------------------------------------------------------------------------

message("Loading data...")

kalshi_data <- readRDS("kalshi_nba_backtest_data.rds")
schedule <- load_nba_schedule(seasons = 2026) %>%
  filter(status_type_completed == TRUE) %>%
  select(game_id, game_date, home_abbrev = home_abbreviation,
         away_abbrev = away_abbreviation, home_winner) %>%
  mutate(game_date = as.Date(game_date))

pbp <- load_nba_pbp(seasons = 2026)

KALSHI_TO_HOOPR <- c(
 "MIN" = "MIN", "DAL" = "DAL", "GSW" = "GS", "UTA" = "UTAH",
 "SAS" = "SA", "HOU" = "HOU", "CHA" = "CHA", "MEM" = "MEM",
 "NYK" = "NY", "TOR" = "TOR", "ATL" = "ATL", "BOS" = "BOS",
 "LAL" = "LAL", "CLE" = "CLE", "ORL" = "ORL", "MIA" = "MIA",
 "CHI" = "CHI", "IND" = "IND", "LAC" = "LAC", "DEN" = "DEN",
 "PHX" = "PHX", "POR" = "POR", "SAC" = "SAC", "OKC" = "OKC",
 "NOP" = "NO", "MIL" = "MIL", "DET" = "DET", "WAS" = "WSH",
 "PHI" = "PHI", "BKN" = "BKN"
)

pbp_timeline <- pbp %>%
 filter(!is.na(home_score), !is.na(away_score), !is.na(wallclock)) %>%
 mutate(
   pbp_timestamp = as.POSIXct(wallclock, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
   home_margin = home_score - away_score,
   period = period_number
 ) %>%
 select(game_id, pbp_timestamp, home_margin, period) %>%
 distinct(game_id, pbp_timestamp, .keep_all = TRUE)

kalshi_games <- kalshi_data %>%
 mutate(
   home_abbrev_hoopr = KALSHI_TO_HOOPR[home_kalshi],
   away_abbrev_hoopr = KALSHI_TO_HOOPR[away_kalshi],
   game_date_from_ticker = as.Date(str_extract(market_ticker, "\\d{2}[A-Z]{3}\\d{2}"), format = "%y%b%d")
 ) %>%
 distinct(market_ticker, game_date_from_ticker, home_kalshi, away_kalshi,
          home_abbrev_hoopr, away_abbrev_hoopr)

matched_games <- kalshi_games %>%
 inner_join(schedule, by = c("game_date_from_ticker" = "game_date",
                              "home_abbrev_hoopr" = "home_abbrev",
                              "away_abbrev_hoopr" = "away_abbrev")) %>%
 rename(home_won = home_winner)

kalshi_prices <- kalshi_data %>%
 inner_join(matched_games %>% select(market_ticker, game_id, home_won), by = "market_ticker") %>%
 select(game_id, timestamp = minute_bucket, kalshi_price = price_mean, home_won = home_won.y)

# Match to PBP
message("Matching Kalshi prices to game states...")

all_matched <- list()
game_ids <- unique(kalshi_prices$game_id)

for (gid in game_ids) {
 k_game <- kalshi_prices %>% filter(game_id == gid) %>% arrange(timestamp)
 p_game <- pbp_timeline %>% filter(game_id == gid) %>% arrange(pbp_timestamp)

 if (nrow(k_game) == 0 || nrow(p_game) == 0) next

 for (i in 1:nrow(k_game)) {
   kalshi_ts <- k_game$timestamp[i]
   diffs <- abs(difftime(p_game$pbp_timestamp, kalshi_ts, units = "secs"))
   idx <- which.min(diffs)

   if (as.numeric(diffs[idx]) <= 600) {
     all_matched[[length(all_matched) + 1]] <- tibble(
       game_id = gid,
       timestamp = kalshi_ts,
       kalshi_price = k_game$kalshi_price[i],
       home_margin = p_game$home_margin[idx],
       period = p_game$period[idx]
     )
   }
 }
}

all_obs <- bind_rows(all_matched) %>%
 mutate(
   lead = abs(home_margin),
   home_leading = home_margin > 0,
   leader_price = ifelse(home_leading, kalshi_price, 1 - kalshi_price),
   trailer_price = 1 - leader_price,
   blown_rate = map2_dbl(lead, period, get_actual_blown_rate),
   breakeven_blown = calc_breakeven_blown(trailer_price),
   edge = blown_rate - breakeven_blown,
   is_fade_eligible = lead >= 7 & period <= 4 & edge > 0
 ) %>%
 arrange(game_id, timestamp)

message(sprintf("Total observations: %d", nrow(all_obs)))
message(sprintf("Unique games: %d", n_distinct(all_obs$game_id)))

# ------------------------------------------------------------------------------
# Get all +EV fade opportunities
# ------------------------------------------------------------------------------

fade_opps <- all_obs %>%
  filter(is_fade_eligible) %>%
  group_by(game_id) %>%
  mutate(opp_num = row_number()) %>%
  ungroup()

message(sprintf("\nTotal +EV fade opportunities: %d", nrow(fade_opps)))
message(sprintf("Games with fade opportunities: %d", n_distinct(fade_opps$game_id)))
message(sprintf("Avg opportunities per game: %.1f", nrow(fade_opps) / n_distinct(fade_opps$game_id)))

# Distribution of opportunities per game
opp_dist <- fade_opps %>%
  group_by(game_id) %>%
  summarise(n_opps = n(), .groups = "drop")

message("\nOpportunities per game distribution:")
print(quantile(opp_dist$n_opps, probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 1)))

# ------------------------------------------------------------------------------
# Test Different Caps
# ------------------------------------------------------------------------------

BET_SIZE <- 0.02
caps_to_test <- c(1, 2, 3, 4, 5, 7, 10, 15, 20, Inf)

set.seed(42)
N_SIMS <- 5000

message(sprintf("\nTesting %d different caps with %d simulations each...\n",
                length(caps_to_test), N_SIMS))

results_by_cap <- list()

for (cap in caps_to_test) {
  cap_label <- if (is.infinite(cap)) "Unlimited" else as.character(cap)
  message(sprintf("Testing cap: %s...", cap_label))

  # Filter to capped positions per game
  if (is.infinite(cap)) {
    positions <- fade_opps
  } else {
    positions <- fade_opps %>%
      filter(opp_num <= cap)
  }

  n_positions <- nrow(positions)
  n_games <- n_distinct(positions$game_id)
  avg_per_game <- n_positions / n_games
  avg_edge <- mean(positions$edge)

  # Monte Carlo
  sim_pnls <- numeric(N_SIMS)
  sim_wins <- numeric(N_SIMS)

  for (sim in 1:N_SIMS) {
    blown <- runif(n_positions) < positions$blown_rate
    pnl <- ifelse(blown,
                  (1 - positions$trailer_price) * 0.93 * BET_SIZE * 1000,
                  -positions$trailer_price * BET_SIZE * 1000)
    sim_pnls[sim] <- sum(pnl)
    sim_wins[sim] <- sum(blown)
  }

  results_by_cap[[cap_label]] <- tibble(
    cap = cap_label,
    cap_num = if (is.infinite(cap)) 999 else cap,
    n_positions = n_positions,
    n_games = n_games,
    avg_per_game = avg_per_game,
    avg_edge = avg_edge * 100,
    mean_pnl = mean(sim_pnls),
    median_pnl = median(sim_pnls),
    std_dev = sd(sim_pnls),
    sharpe = mean(sim_pnls) / sd(sim_pnls),
    prob_profit = mean(sim_pnls > 0) * 100,
    pct_5 = quantile(sim_pnls, 0.05),
    pct_95 = quantile(sim_pnls, 0.95),
    avg_win_rate = mean(sim_wins) / n_positions * 100,
    ev_per_position = mean(sim_pnls) / n_positions
  )
}

final_results <- bind_rows(results_by_cap) %>%
  arrange(cap_num)

# ------------------------------------------------------------------------------
# Results
# ------------------------------------------------------------------------------

message("\n", strrep("=", 80))
message("  FADE RE-ENTRY CAPS COMPARISON")
message(strrep("=", 80), "\n")

print(final_results %>%
  select(cap, n_positions, avg_per_game, avg_edge, mean_pnl, std_dev, sharpe, prob_profit) %>%
  mutate(
    avg_per_game = sprintf("%.1f", avg_per_game),
    avg_edge = sprintf("%.1f%%", avg_edge),
    mean_pnl = sprintf("$%.0f", mean_pnl),
    std_dev = sprintf("$%.0f", std_dev),
    sharpe = sprintf("%.2f", sharpe),
    prob_profit = sprintf("%.1f%%", prob_profit)
  ))

message("\n=== DETAILED METRICS ===\n")

print(final_results %>%
  select(cap, n_positions, ev_per_position, pct_5, pct_95) %>%
  mutate(
    ev_per_position = sprintf("$%.2f", ev_per_position),
    pct_5 = sprintf("$%.0f", pct_5),
    pct_95 = sprintf("$%.0f", pct_95)
  ))

# ------------------------------------------------------------------------------
# Analysis: Marginal Value of Each Additional Position
# ------------------------------------------------------------------------------

message("\n=== MARGINAL VALUE ANALYSIS ===\n")

marginal <- final_results %>%
  arrange(cap_num) %>%
  mutate(
    positions_added = n_positions - lag(n_positions, default = 0),
    pnl_added = mean_pnl - lag(mean_pnl, default = 0),
    marginal_ev = pnl_added / positions_added
  ) %>%
  filter(cap != "1")  # Skip first row

print(marginal %>%
  select(cap, positions_added, pnl_added, marginal_ev) %>%
  mutate(
    pnl_added = sprintf("$%.0f", pnl_added),
    marginal_ev = sprintf("$%.2f", marginal_ev)
  ))

# Find optimal cap based on Sharpe ratio
optimal_sharpe <- final_results %>%
  filter(sharpe == max(sharpe))

# Find practical optimum (balance P&L and execution feasibility)
practical <- final_results %>%
  filter(cap_num <= 5) %>%
  filter(mean_pnl == max(mean_pnl))

message("\n=== RECOMMENDATIONS ===\n")

message(sprintf("Highest Sharpe: Cap = %s (%.2f Sharpe, $%.0f P&L)",
                optimal_sharpe$cap, optimal_sharpe$sharpe, optimal_sharpe$mean_pnl))

message(sprintf("Practical Optimum (max 5/game): Cap = %s ($%.0f P&L, %.2f Sharpe)",
                practical$cap, practical$mean_pnl, practical$sharpe))

message(sprintf("\nDiminishing returns after cap = 5:"))
after_5 <- final_results %>% filter(cap_num > 5, cap_num < 999)
if (nrow(after_5) > 0) {
  for (i in 1:nrow(after_5)) {
    r <- after_5[i, ]
    prev <- final_results %>% filter(cap_num == 5)
    message(sprintf("  Cap %s: +$%.0f P&L, but Sharpe drops from %.2f to %.2f",
                    r$cap, r$mean_pnl - prev$mean_pnl, prev$sharpe, r$sharpe))
  }
}

# ------------------------------------------------------------------------------
# Capital Requirements
# ------------------------------------------------------------------------------

message("\n=== CAPITAL & RISK ANALYSIS ===\n")

capital_analysis <- final_results %>%
  mutate(
    total_capital = n_positions * BET_SIZE * 1000,
    roi = mean_pnl / total_capital * 100,
    max_drawdown_approx = -pct_5,
    capital_at_risk = total_capital
  ) %>%
  select(cap, n_positions, total_capital, mean_pnl, roi, max_drawdown_approx)

print(capital_analysis %>%
  mutate(
    total_capital = sprintf("$%.0f", total_capital),
    mean_pnl = sprintf("$%.0f", mean_pnl),
    roi = sprintf("%.1f%%", roi),
    max_drawdown_approx = sprintf("$%.0f", max_drawdown_approx)
  ))

# Save results
saveRDS(final_results, "fade_reentry_caps_results.rds")
message("\nSaved to fade_reentry_caps_results.rds")

message("\n=== Done ===")
