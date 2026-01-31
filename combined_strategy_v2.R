# Combined Strategy V2 - Optimized
# With Leader: Q3/Q4 at 15+ pts OR any period at 21+ pts (no stop-loss)
# Selective Fade: 7+ pts, +EV condition, hold to settlement

library(tidyverse)
library(hoopR)

message("\n=== COMBINED STRATEGY V2 (OPTIMIZED) ===\n")

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
   breakeven_blown = calc_breakeven_blown(trailer_price)
 )

message(sprintf("Total observations: %d", nrow(all_obs)))
message(sprintf("Unique games: %d", n_distinct(all_obs$game_id)))

# ------------------------------------------------------------------------------
# Strategy Parameters
# ------------------------------------------------------------------------------

BET_SIZE <- 0.02

# WITH LEADER V2: Q3/Q4 at 15+ pts OR any period at 21+ pts
# No stop-loss - hold to settlement

with_leader_eligible <- all_obs %>%
  filter(
    period <= 4,
    (lead >= 21) | (period >= 3 & lead >= 15)
  )

with_leader_entries <- with_leader_eligible %>%
  arrange(game_id, timestamp) %>%
  group_by(game_id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    strategy = "with_leader_v2",
    entry_price = leader_price,
    hold_rate = 1 - blown_rate
  )

# SELECTIVE FADE: 7+ pts, +EV condition
fade_entries <- all_obs %>%
  filter(
    lead >= 7,
    period <= 4,
    blown_rate > breakeven_blown
  ) %>%
  arrange(game_id, timestamp) %>%
  group_by(game_id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    strategy = "fade",
    entry_price = trailer_price,
    edge = blown_rate - breakeven_blown
  )

message(sprintf("\nWith Leader V2 entries: %d games", nrow(with_leader_entries)))
message(sprintf("  - Q3/Q4 at 15+ pts entries: %d",
                sum(with_leader_entries$lead < 21 & with_leader_entries$period >= 3)))
message(sprintf("  - 21+ pt lead entries: %d", sum(with_leader_entries$lead >= 21)))

message(sprintf("\nSelective Fade entries: %d games", nrow(fade_entries)))

both_active <- intersect(with_leader_entries$game_id, fade_entries$game_id)
message(sprintf("Games with BOTH strategies: %d", length(both_active)))

# ------------------------------------------------------------------------------
# Monte Carlo Simulation
# ------------------------------------------------------------------------------

set.seed(42)
N_SIMS <- 10000

message(sprintf("\nRunning %d Monte Carlo simulations...", N_SIMS))

sim_results <- list()

for (sim in 1:N_SIMS) {

  # WITH LEADER V2 (no stop-loss, hold to settlement)
  wl_blown <- runif(nrow(with_leader_entries)) < (1 - with_leader_entries$hold_rate)
  wl_pnl <- ifelse(wl_blown,
                   -with_leader_entries$entry_price * BET_SIZE * 1000,
                   (1 - with_leader_entries$entry_price) * 0.93 * BET_SIZE * 1000)

  # SELECTIVE FADE (hold to settlement)
  fade_blown <- runif(nrow(fade_entries)) < fade_entries$blown_rate
  fade_pnl <- ifelse(fade_blown,
                     (1 - fade_entries$entry_price) * 0.93 * BET_SIZE * 1000,
                     -fade_entries$entry_price * BET_SIZE * 1000)

  sim_results[[sim]] <- tibble(
    sim = sim,
    wl_pnl = sum(wl_pnl),
    wl_wins = sum(!wl_blown),
    wl_losses = sum(wl_blown),
    fade_pnl = sum(fade_pnl),
    fade_wins = sum(fade_blown),
    fade_losses = sum(!fade_blown),
    total_pnl = sum(wl_pnl) + sum(fade_pnl)
  )

  if (sim %% 2000 == 0) message(sprintf("  %d simulations complete", sim))
}

results <- bind_rows(sim_results)

# ------------------------------------------------------------------------------
# Results
# ------------------------------------------------------------------------------

message("\n", strrep("=", 70))
message("  COMBINED STRATEGY V2 RESULTS")
message(strrep("=", 70), "\n")

message("=== STRATEGY CONFIGURATION ===\n")
message("WITH LEADER V2:")
message("  - Entry: Q3/Q4 at 15+ pts OR any period at 21+ pts")
message("  - Exit: Hold to settlement (no stop-loss)")
message("\nSELECTIVE FADE:")
message("  - Entry: 7+ pts when blown_rate > breakeven")
message("  - Exit: Hold to settlement")

message("\n=== WITH LEADER V2 ===\n")
message(sprintf("Positions: %d", nrow(with_leader_entries)))
message(sprintf("Avg leader price: %.1f¢", mean(with_leader_entries$entry_price) * 100))
message(sprintf("Avg blown rate: %.1f%%", mean(1 - with_leader_entries$hold_rate) * 100))
message(sprintf("Mean P&L: $%.2f", mean(results$wl_pnl)))
message(sprintf("Std Dev: $%.2f", sd(results$wl_pnl)))
message(sprintf("Sharpe: %.2f", mean(results$wl_pnl) / sd(results$wl_pnl)))
message(sprintf("Prob of Profit: %.1f%%", mean(results$wl_pnl > 0) * 100))
message(sprintf("Win rate: %.1f%%", mean(results$wl_wins) / nrow(with_leader_entries) * 100))

message("\n=== SELECTIVE FADE ===\n")
message(sprintf("Positions: %d", nrow(fade_entries)))
message(sprintf("Avg trailer price: %.1f¢", mean(fade_entries$entry_price) * 100))
message(sprintf("Avg edge: %.1f%%", mean(fade_entries$edge) * 100))
message(sprintf("Mean P&L: $%.2f", mean(results$fade_pnl)))
message(sprintf("Std Dev: $%.2f", sd(results$fade_pnl)))
message(sprintf("Sharpe: %.2f", mean(results$fade_pnl) / sd(results$fade_pnl)))
message(sprintf("Prob of Profit: %.1f%%", mean(results$fade_pnl > 0) * 100))
message(sprintf("Win rate: %.1f%%", mean(results$fade_wins) / nrow(fade_entries) * 100))

message("\n=== COMBINED PORTFOLIO ===\n")
total_trades <- nrow(with_leader_entries) + nrow(fade_entries)
message(sprintf("Total positions: %d", total_trades))
message(sprintf("Unique games traded: %d", n_distinct(c(with_leader_entries$game_id, fade_entries$game_id))))
message(sprintf("Games with BOTH active: %d", length(both_active)))
message(sprintf("Bet size: $%d per trade (2%% of $1000)", BET_SIZE * 1000))
message(sprintf("\nMean P&L: $%.2f", mean(results$total_pnl)))
message(sprintf("Median P&L: $%.2f", median(results$total_pnl)))
message(sprintf("Std Dev: $%.2f", sd(results$total_pnl)))
message(sprintf("5th percentile: $%.2f", quantile(results$total_pnl, 0.05)))
message(sprintf("95th percentile: $%.2f", quantile(results$total_pnl, 0.95)))
message(sprintf("Probability of profit: %.1f%%", mean(results$total_pnl > 0) * 100))
message(sprintf("Sharpe ratio: %.2f", mean(results$total_pnl) / sd(results$total_pnl)))

# Correlation
corr <- cor(results$wl_pnl, results$fade_pnl)
message(sprintf("\nStrategy correlation: %.3f", corr))

# Per-trade metrics
message("\n=== PER-TRADE METRICS ===\n")
message(sprintf("With Leader EV per trade: $%.2f", mean(results$wl_pnl) / nrow(with_leader_entries)))
message(sprintf("Fade EV per trade: $%.2f", mean(results$fade_pnl) / nrow(fade_entries)))
message(sprintf("Combined EV per trade: $%.2f", mean(results$total_pnl) / total_trades))

# ROI
total_capital_deployed <- total_trades * BET_SIZE * 1000
message(sprintf("\nTotal capital deployed: $%.0f", total_capital_deployed))
message(sprintf("Expected ROI: %.1f%%", mean(results$total_pnl) / total_capital_deployed * 100))

# Save
saveRDS(list(
  with_leader = with_leader_entries,
  fade = fade_entries,
  simulations = results
), "combined_strategy_v2_results.rds")

message("\nSaved to combined_strategy_v2_results.rds")
message("\n=== Done ===")
