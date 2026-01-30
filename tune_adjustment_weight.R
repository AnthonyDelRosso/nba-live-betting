# Tune the adjustment weight for combining WP and Blown Lead models
# Tests different weights and compares backtest performance

library(tidyverse)
library(hoopR)

source("win_probability_model.R")
source("nba_largest_lead.R")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

WEIGHTS_TO_TEST <- seq(0, 1, by = 0.1)  # Test 0.0, 0.1, 0.2, ..., 1.0

# Constants from kalshi_predictor.R
EDGE_THRESHOLD <- 0.03
STRONG_EDGE_THRESHOLD <- 0.05
HIGH_EDGE_THRESHOLD <- 0.07
MIN_PERIOD_FOR_SIGNAL <- 3
MIN_LEAD_FOR_SIGNAL <- 15

BET_SIZE_TIER1 <- 0.05
BET_SIZE_TIER2 <- 0.04
BET_SIZE_TIER3 <- 0.03
BET_SIZE_TIER4 <- 0.02

# ------------------------------------------------------------------------------
# Load models and data once
# ------------------------------------------------------------------------------

message("\n=== ADJUSTMENT WEIGHT TUNING ===\n")

message("Loading models...")
wp_model <- load_wp_model("win_probability_model.rds")
blown_lead_model <- load_model("blown_lead_model.rds")

message("Loading Kalshi backtest data...")
kalshi_data <- readRDS("kalshi_nba_backtest_data.rds")

# Team mapping
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

message("Loading schedule and PBP...")
schedule <- load_nba_schedule(seasons = 2026) %>%
  filter(status_type_completed == TRUE) %>%
  select(game_id, game_date, home_abbrev = home_abbreviation,
         away_abbrev = away_abbreviation, home_winner) %>%
  mutate(game_date = as.Date(game_date))

pbp <- load_nba_pbp(seasons = 2026)

# Build PBP timeline
pbp_timeline <- pbp %>%
  filter(!is.na(home_score), !is.na(away_score), !is.na(wallclock)) %>%
  mutate(
    pbp_timestamp = as.POSIXct(wallclock, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    home_margin = home_score - away_score,
    seconds_remaining = case_when(
      period_number <= 4 ~ (4 - period_number) * 720 + clock_minutes * 60 + clock_seconds,
      period_number > 4 ~ clock_minutes * 60 + clock_seconds
    ),
    period = period_number
  ) %>%
  filter(!is.na(pbp_timestamp)) %>%
  select(game_id, pbp_timestamp, home_score, away_score, home_margin,
         seconds_remaining, period) %>%
  distinct(game_id, pbp_timestamp, .keep_all = TRUE)

# Match Kalshi games to schedule
kalshi_games <- kalshi_data %>%
  mutate(
    home_abbrev_hoopr = KALSHI_TO_HOOPR[home_kalshi],
    away_abbrev_hoopr = KALSHI_TO_HOOPR[away_kalshi],
    ticker_date_str = str_extract(market_ticker, "\\d{2}[A-Z]{3}\\d{2}"),
    game_date_from_ticker = as.Date(ticker_date_str, format = "%y%b%d")
  ) %>%
  distinct(market_ticker, game_date_from_ticker, home_kalshi, away_kalshi,
           home_abbrev_hoopr, away_abbrev_hoopr, home_won) %>%
  rename(game_date = game_date_from_ticker)

matched_games <- kalshi_games %>%
  inner_join(
    schedule %>% select(game_id, game_date, home_abbrev, away_abbrev, home_winner),
    by = c("game_date", "home_abbrev_hoopr" = "home_abbrev", "away_abbrev_hoopr" = "away_abbrev")
  )

# Add game_id to Kalshi data (kalshi_data already has home_won, just need game_id)
kalshi_with_games <- kalshi_data %>%
  inner_join(
    matched_games %>% select(market_ticker, game_id),
    by = "market_ticker"
  )

# Match to PBP (do this once)
message("Matching Kalshi prices to PBP states...")

match_to_pbp <- function(kalshi_df, pbp_df) {
  games <- unique(kalshi_df$game_id)
  results <- list()

  for (gid in games) {
    kalshi_game <- kalshi_df %>% filter(game_id == gid)
    pbp_game <- pbp_df %>% filter(game_id == gid) %>% arrange(pbp_timestamp)

    if (nrow(pbp_game) == 0) next

    for (i in 1:nrow(kalshi_game)) {
      kalshi_ts <- kalshi_game$minute_bucket[i]
      time_diffs <- abs(difftime(pbp_game$pbp_timestamp, kalshi_ts, units = "secs"))
      closest_idx <- which.min(time_diffs)
      min_diff <- as.numeric(time_diffs[closest_idx])

      if (min_diff <= 600) {
        pbp_state <- pbp_game[closest_idx, ]
        results[[length(results) + 1]] <- tibble(
          game_id = gid,
          kalshi_timestamp = kalshi_ts,
          kalshi_price = kalshi_game$price_mean[i],
          home_won = kalshi_game$home_won[i],
          home_score = pbp_state$home_score,
          away_score = pbp_state$away_score,
          home_margin = pbp_state$home_margin,
          seconds_remaining = pbp_state$seconds_remaining,
          period = pbp_state$period
        )
      }
    }
  }
  bind_rows(results)
}

matched_data <- match_to_pbp(kalshi_with_games, pbp_timeline)
message(sprintf("Score-matched observations: %d", nrow(matched_data)))

# Filter to Q3+ with 15+ leads
filtered_data <- matched_data %>%
  filter(period >= MIN_PERIOD_FOR_SIGNAL) %>%
  filter(abs(home_margin) >= MIN_LEAD_FOR_SIGNAL)

message(sprintf("After filters: %d observations\n", nrow(filtered_data)))

# ------------------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------------------

# Vectorized helper functions
get_baseline_wp_vec <- function(home_margin, seconds_remaining, period) {
  # predict_wp should handle vectors
  sapply(seq_along(home_margin), function(i) {
    predict_wp(wp_model, home_margin[i], seconds_remaining[i], period[i], 0)
  })
}

get_blown_lead_prob_vec <- function(home_margin, period, minutes_elapsed) {
  sapply(seq_along(home_margin), function(i) {
    if (abs(home_margin[i]) < 3) return(0.5)
    predict_blown_lead(
      model = blown_lead_model,
      largest_lead = abs(home_margin[i]),
      minutes_elapsed = minutes_elapsed[i],
      period = period[i],
      is_home = home_margin[i] > 0,
      rest_advantage = 0,
      net_rating_advantage = 0
    )
  })
}

get_adjusted_wp_vec <- function(baseline_wp, blown_lead_prob, home_margin, weight) {
  adjusted_wp <- baseline_wp

  # Home leading
  home_leading <- home_margin > 0
  adjustment_home <- blown_lead_prob * weight * (baseline_wp - 0.5)
  adjusted_wp[home_leading] <- baseline_wp[home_leading] - adjustment_home[home_leading]

  # Away leading
  away_leading <- home_margin < 0
  adjustment_away <- blown_lead_prob * weight * (0.5 - baseline_wp)
  adjusted_wp[away_leading] <- baseline_wp[away_leading] + adjustment_away[away_leading]

  pmax(0.001, pmin(0.999, adjusted_wp))
}

calculate_bet_tier <- function(period, lead, abs_edge) {
  is_q4 <- period >= 4
  strong_edge <- abs_edge >= STRONG_EDGE_THRESHOLD

  if (is_q4 && lead >= 20) {
    return(list(tier = "TIER_1", bet_size = BET_SIZE_TIER1))
  }
  if ((is_q4 && lead >= 15) || lead >= 20) {
    return(list(tier = "TIER_2", bet_size = BET_SIZE_TIER2))
  }
  if (strong_edge) {
    return(list(tier = "TIER_3", bet_size = BET_SIZE_TIER3))
  }
  list(tier = "TIER_4", bet_size = BET_SIZE_TIER4)
}

# ------------------------------------------------------------------------------
# Run backtest for each weight
# ------------------------------------------------------------------------------

message("Testing adjustment weights...\n")

results_by_weight <- list()

for (weight in WEIGHTS_TO_TEST) {

  # Calculate predictions for this weight (vectorized)
  predictions <- filtered_data %>%
    mutate(
      minutes_elapsed = (2880 - seconds_remaining) / 60,
      baseline_wp = get_baseline_wp_vec(home_margin, seconds_remaining, period),
      blown_lead_prob = get_blown_lead_prob_vec(home_margin, period, minutes_elapsed),
      adjusted_wp = get_adjusted_wp_vec(baseline_wp, blown_lead_prob, home_margin, weight),
      edge = adjusted_wp - kalshi_price
    )

  # Generate signals
  signals <- predictions %>%
    mutate(
      signal = case_when(
        home_margin > 0 & adjusted_wp >= 0.80 & abs(edge) >= EDGE_THRESHOLD ~ "BUY_YES",
        home_margin < 0 & adjusted_wp <= 0.20 & abs(edge) >= EDGE_THRESHOLD ~ "BUY_NO",
        TRUE ~ "HOLD"
      )
    ) %>%
    filter(signal %in% c("BUY_YES", "BUY_NO"))

  if (nrow(signals) == 0) {
    results_by_weight[[as.character(weight)]] <- tibble(
      weight = weight,
      n_trades = 0,
      win_rate = NA,
      avg_edge = NA,
      tiered_pnl = 0,
      flat_pnl = 0
    )
    next
  }

  # Calculate P&L
  trades <- signals %>%
    mutate(
      lead = abs(home_margin),
      abs_edge = abs(edge),
      bet_tier = pmap(list(period, lead, abs_edge),
                      ~ calculate_bet_tier(..1, ..2, ..3)),
      bet_size = map_dbl(bet_tier, ~ .x$bet_size),
      bet_won = case_when(
        signal == "BUY_YES" ~ home_won == TRUE,
        signal == "BUY_NO" ~ home_won == FALSE
      ),
      unit_pnl = case_when(
        signal == "BUY_YES" & bet_won ~ (1 - kalshi_price) * 0.93,
        signal == "BUY_YES" & !bet_won ~ -kalshi_price,
        signal == "BUY_NO" & bet_won ~ kalshi_price * 0.93,
        signal == "BUY_NO" & !bet_won ~ -(1 - kalshi_price)
      ),
      tiered_pnl = unit_pnl * bet_size * 1000,
      flat_pnl = unit_pnl * 0.02 * 1000
    )

  results_by_weight[[as.character(weight)]] <- tibble(
    weight = weight,
    n_trades = nrow(trades),
    n_games = n_distinct(trades$game_id),
    win_rate = mean(trades$bet_won) * 100,
    avg_edge = mean(abs(trades$edge)) * 100,
    tiered_pnl = sum(trades$tiered_pnl),
    flat_pnl = sum(trades$flat_pnl),
    buy_yes = sum(trades$signal == "BUY_YES"),
    buy_no = sum(trades$signal == "BUY_NO")
  )

  message(sprintf("Weight %.1f: %d trades, %.1f%% WR, $%.2f tiered P&L",
                  weight, nrow(trades), mean(trades$bet_won) * 100, sum(trades$tiered_pnl)))
}

# ------------------------------------------------------------------------------
# Results summary
# ------------------------------------------------------------------------------

results_df <- bind_rows(results_by_weight)

message("\n", strrep("=", 70))
message("  ADJUSTMENT WEIGHT TUNING RESULTS")
message(strrep("=", 70), "\n")

print(results_df %>%
        mutate(
          win_rate = sprintf("%.1f%%", win_rate),
          avg_edge = sprintf("%.2f%%", avg_edge),
          tiered_pnl = sprintf("$%.2f", tiered_pnl),
          flat_pnl = sprintf("$%.2f", flat_pnl)
        ))

# Find optimal weight
optimal <- results_df %>%
  filter(win_rate == max(win_rate, na.rm = TRUE)) %>%
  filter(tiered_pnl == max(tiered_pnl)) %>%
  slice(1)

message("\n=== OPTIMAL WEIGHT ===\n")
message(sprintf("Best adjustment weight: %.1f", optimal$weight))
message(sprintf("  Trades: %d across %d games", optimal$n_trades, optimal$n_games))
message(sprintf("  Win Rate: %.1f%%", as.numeric(gsub("%", "", results_df$win_rate[results_df$weight == optimal$weight]))))
message(sprintf("  Tiered P&L: $%.2f", optimal$tiered_pnl))
message(sprintf("  BUY_YES: %d, BUY_NO: %d", optimal$buy_yes, optimal$buy_no))

# Save results
saveRDS(results_df, "adjustment_weight_tuning.rds")
message("\nSaved results to adjustment_weight_tuning.rds")

message("\n=== Done ===")
