# Backtest In-Game Adjustments Strategy on January 2026 data
# Tests the impact of velocity adjustments on fade strategy performance
#
# Strategy: FADE V2 at 35-45c price range (no hedging)
# Hedging was tested and found to destroy value - removed
#
# Compares:
# 1. Base V2 strategy (trailer 35-45c)
# 2. V2 + In-game velocity adjustments

library(tidyverse)
library(hoopR)

# Source the predictor (which sources the models)
source("kalshi_predictor.R")

message("\n", strrep("=", 70))
message("  BACKTEST: FADE V2 (35-45c, NO HEDGING) + IN-GAME ADJUSTMENTS")
message(strrep("=", 70), "\n")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

KALSHI_FEE <- 0.07
BASE_SPREAD <- 0.02
STARTING_BALANCE <- 4500  # Match the app's bankroll

# Entry price bounds (no hedging - cap at 45c)
FADE_MIN_PRICE <- 0.35    # Minimum trailer price for entry
FADE_MAX_PRICE <- 0.45    # Maximum trailer price - above this, breakeven exceeds win rate

# Bet sizing by price tier (matches kalshi_predictor.R)
BET_SIZE_35_40 <- 0.03    # 3% at 35-40c (best risk/reward)
BET_SIZE_40_45 <- 0.02    # 2% at 40-45c (still profitable)

#' Get bet size for price tier with period filter
#' Based on backtest: 40-45c only profitable in Q3/Q4
get_bet_size_for_tier <- function(trailer_price, period = NULL) {
  if (trailer_price < FADE_MIN_PRICE || trailer_price > FADE_MAX_PRICE) {
    return(NULL)  # Price out of range
  }
  if (trailer_price < 0.40) {
    # 35-40c: all periods (net profitable)
    return(list(bet_size = BET_SIZE_35_40, tier = "35-40c"))
  } else {
    # 40-45c: Q3/Q4 only (61-75% WR vs 47% breakeven)
    if (!is.null(period) && period < 3) {
      return(NULL)
    }
    return(list(bet_size = BET_SIZE_40_45, tier = "40-45c"))
  }
}

# In-game adjustment parameters (EMPIRICALLY VALIDATED)
# Velocity thresholds - determined by grid search (50% win rate diff at 4.5c/min)
VELOCITY_THRESHOLD <- 0.045     # 4.5c/min - empirically optimal
SHARP_SPIKE_THRESHOLD <- 0.06   # 6c/min blocks entry
VELOCITY_BOOST <- 1.20          # 20% boost on dips
VELOCITY_PENALTY <- 0.80        # 20% penalty on spikes

# Run differential - DISABLED (no predictive signal found)
USE_RUN_DIFFERENTIAL <- FALSE

# ------------------------------------------------------------------------------
# Helper Functions
# ------------------------------------------------------------------------------

#' Calculate price velocity from game's Kalshi data
#' Uses ALL price snapshots (not just fade-eligible) for better coverage
calc_velocity <- function(game_kalshi, current_time, current_trailer_price, home_leading, minutes_back = 5) {
  lookback_time <- current_time - (minutes_back * 60)

  # Find price closest to lookback time
  price_ago_row <- game_kalshi %>%
    filter(timestamp <= lookback_time) %>%
    arrange(desc(timestamp)) %>%
    slice(1)

  if (nrow(price_ago_row) == 0) return(list(velocity = NA, price_ago = NA))

  # Calculate trailer price at that time
  old_market_price <- price_ago_row$price
  old_trailer_price <- if (home_leading) (1 - old_market_price) else old_market_price

  # Calculate actual time difference
  actual_minutes <- as.numeric(difftime(current_time, price_ago_row$timestamp, units = "mins"))

  if (actual_minutes < 2 || actual_minutes > 10) {
    return(list(velocity = NA, price_ago = NA))  # Too short or too long
  }

  velocity <- (current_trailer_price - old_trailer_price) / actual_minutes

  list(velocity = velocity, price_ago = old_trailer_price, minutes = actual_minutes)
}

#' Calculate combined in-game adjustment
calc_combined_adjustment <- function(velocity) {
  velocity_adj <- 1.0
  blocked <- FALSE
  reasons <- c()

  # Velocity adjustment (EMPIRICALLY VALIDATED - 50% win rate diff)
  if (!is.na(velocity)) {
    if (velocity >= SHARP_SPIKE_THRESHOLD) {
      blocked <- TRUE
      reasons <- c(reasons, sprintf("BLOCKED: Sharp spike +%.1fc/min", velocity * 100))
    } else if (velocity >= VELOCITY_THRESHOLD) {
      velocity_adj <- VELOCITY_PENALTY
      reasons <- c(reasons, sprintf("Spike penalty -%.0f%%", (1 - VELOCITY_PENALTY) * 100))
    } else if (velocity <= -VELOCITY_THRESHOLD) {
      velocity_adj <- VELOCITY_BOOST
      reasons <- c(reasons, sprintf("Dip boost +%.0f%%", (VELOCITY_BOOST - 1) * 100))
    }
  }

  list(
    adjustment = velocity_adj,
    blocked = blocked,
    reasons = if (length(reasons) > 0) paste(reasons, collapse = "; ") else "No adjustments"
  )
}

#' Simulate spread
simulate_spread <- function(mid_price, game_state = NULL) {
  spread <- BASE_SPREAD

  if (!is.null(game_state)) {
    period <- game_state$period %||% 1
    seconds_remaining <- game_state$seconds_remaining %||% 2880
    lead <- abs(game_state$home_margin %||% 0)

    if (period >= 4 && seconds_remaining < 300) spread <- spread * 1.5
    if (lead < 5 && period >= 3) spread <- spread * 1.3
    if (lead > 20) spread <- spread * 0.7
  }

  half_spread <- spread / 2
  bid <- max(0.01, mid_price - half_spread)
  ask <- min(0.99, mid_price + half_spread)

  list(bid = bid, ask = ask, spread = ask - bid)
}

# ------------------------------------------------------------------------------
# Load Data
# ------------------------------------------------------------------------------

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

message("Loading Kalshi data...")
kalshi_data <- readRDS("kalshi_nba_full_data.rds")
kalshi_data <- kalshi_data %>% filter(market_team == home_team)
message(sprintf("  %d Kalshi observations", nrow(kalshi_data)))

message("Loading schedule...")
schedule <- load_nba_schedule(seasons = 2026) %>%
  filter(status_type_completed == TRUE) %>%
  select(game_id, game_date, home_abbrev = home_abbreviation, away_abbrev = away_abbreviation,
         home_score, away_score, home_winner) %>%
  mutate(game_date = as.Date(game_date))

message("Loading play-by-play...")
pbp <- load_nba_pbp(seasons = 2026)

message("Loading team ratings...")
team_box <- load_nba_team_box(seasons = 2026)

team_ratings <- team_box %>%
  filter(season_type == 2) %>%
  arrange(team_abbreviation, game_date) %>%
  mutate(poss = field_goals_attempted + 0.44 * free_throws_attempted - offensive_rebounds + turnovers) %>%
  group_by(team_abbreviation) %>%
  mutate(
    cum_pts = lag(cumsum(team_score), default = NA_real_),
    cum_pts_allowed = lag(cumsum(opponent_team_score), default = NA_real_),
    cum_poss = lag(cumsum(poss), default = NA_real_),
    net_rating = ((cum_pts / cum_poss) - (cum_pts_allowed / cum_poss)) * 100
  ) %>%
  ungroup() %>%
  select(game_id, team_abbreviation, net_rating)

# ------------------------------------------------------------------------------
# Initialize Predictors
# ------------------------------------------------------------------------------

message("\nInitializing predictor...")
predictor <- KalshiPredictor$new(fade_mode = "market")

# ------------------------------------------------------------------------------
# Process Games - Compare Base vs With Adjustments
# ------------------------------------------------------------------------------

# Tracking
base_trades <- list()      # Base V2 strategy (35-45c, no hedge)
adj_trades <- list()       # V2 + velocity adjustments

# Counters
base_entries <- 0
adj_entries <- 0
adj_blocked <- 0
adj_boosted <- 0

unique_games <- kalshi_data %>%
  distinct(ticker, game_date, home_team, away_team, result)

message(sprintf("\nProcessing %d games...\n", nrow(unique_games)))

for (g in 1:nrow(unique_games)) {
  game_info <- unique_games[g, ]

  home_abbrev_hoopr <- KALSHI_TO_HOOPR[game_info$home_team]
  away_abbrev_hoopr <- KALSHI_TO_HOOPR[game_info$away_team]

  if (is.na(home_abbrev_hoopr) || is.na(away_abbrev_hoopr)) next

  sched_game <- schedule %>%
    filter(game_date == game_info$game_date,
           home_abbrev == home_abbrev_hoopr,
           away_abbrev == away_abbrev_hoopr)

  if (nrow(sched_game) != 1) next

  game_id <- as.character(sched_game$game_id)
  home_won <- sched_game$home_winner

  # Get net ratings
  home_rating <- team_ratings %>%
    filter(game_id == sched_game$game_id, team_abbreviation == home_abbrev_hoopr) %>%
    pull(net_rating)
  away_rating <- team_ratings %>%
    filter(game_id == sched_game$game_id, team_abbreviation == away_abbrev_hoopr) %>%
    pull(net_rating)

  home_rating <- if (length(home_rating) == 0 || is.na(home_rating)) 0 else home_rating
  away_rating <- if (length(away_rating) == 0 || is.na(away_rating)) 0 else away_rating

  # Get PBP
  game_pbp <- pbp %>%
    filter(game_id == sched_game$game_id) %>%
    filter(!is.na(home_score), !is.na(away_score), !is.na(wallclock)) %>%
    mutate(
      pbp_timestamp = as.POSIXct(wallclock, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      home_margin = home_score - away_score
    ) %>%
    arrange(pbp_timestamp)

  if (nrow(game_pbp) == 0) next

  # Get Kalshi data for this game
  game_kalshi <- kalshi_data %>%
    filter(ticker == game_info$ticker) %>%
    mutate(timestamp = as.POSIXct(timestamp, tz = "UTC")) %>%
    arrange(timestamp)

  if (nrow(game_kalshi) == 0) next

  # Reset predictor positions for this game
  predictor$reset_positions()

  # Track positions for this game
  base_game_positions <- 0
  adj_game_positions <- 0

  # Process each Kalshi snapshot
  for (i in 1:nrow(game_kalshi)) {
    snap <- game_kalshi[i, ]

    # Find closest PBP entry
    closest_pbp <- game_pbp %>%
      mutate(time_diff = abs(as.numeric(difftime(pbp_timestamp, snap$timestamp, units = "secs")))) %>%
      filter(time_diff < 120) %>%  # Within 2 minutes
      arrange(time_diff) %>%
      slice(1)

    if (nrow(closest_pbp) == 0) next

    # Build game state
    home_margin <- closest_pbp$home_margin
    home_leading <- home_margin > 0
    lead <- abs(home_margin)

    # Skip if lead too small or game tied
    if (lead < 7) next

    # Calculate trailer price
    market_price <- snap$price
    trailer_price <- if (home_leading) (1 - market_price) else market_price

    # Get game state info from PBP
    period <- closest_pbp$period_number %||% 1
    if (period > 4) next  # Skip overtime

    # Calculate seconds remaining (approximate)
    seconds_elapsed <- case_when(
      period <= 4 ~ (period - 1) * 720 + (720 - (closest_pbp$clock_minutes %||% 0) * 60 - (closest_pbp$clock_seconds %||% 0)),
      TRUE ~ 2880
    )
    seconds_remaining <- max(0, 2880 - seconds_elapsed)

    # Skip final minute
    if (seconds_remaining < 60 && period >= 4) next

    game_state <- list(
      home_margin = home_margin,
      period = period,
      seconds_remaining = seconds_remaining,
      minutes_elapsed = seconds_elapsed / 60,
      home_rating_advantage = home_rating - away_rating,
      net_rating_advantage = if (home_leading) (home_rating - away_rating) else (away_rating - home_rating)
    )

    # Get bet size for this price tier (40-45c requires Q3/Q4)
    bet_params <- get_bet_size_for_tier(trailer_price, period)

    # ========================================
    # BASE V2 STRATEGY (35-45c, NO HEDGE)
    # ========================================

    if (!is.null(bet_params) && base_game_positions < 3) {
      # Apply spread
      spread_data <- simulate_spread(market_price, game_state)
      fade_exec_price <- if (home_leading) (1 - spread_data$bid) else spread_data$ask

      # Determine outcome
      fade_won <- if (home_leading) !home_won else home_won

      # Calculate P&L (simple - no hedge)
      if (fade_won) {
        pnl <- (1 - KALSHI_FEE) - fade_exec_price
      } else {
        pnl <- -fade_exec_price
      }

      base_trades[[length(base_trades) + 1]] <- list(
        game_id = game_id,
        timestamp = snap$timestamp,
        trailer_price = trailer_price,
        execution_price = fade_exec_price,
        bet_size = bet_params$bet_size,
        tier = bet_params$tier,
        lead = lead,
        period = period,
        won = fade_won,
        pnl = pnl,
        strategy = "base_v2"
      )

      base_game_positions <- base_game_positions + 1
      base_entries <- base_entries + 1
    }

    # ========================================
    # V2 + IN-GAME VELOCITY ADJUSTMENTS
    # ========================================

    if (!is.null(bet_params) && adj_game_positions < 3) {
      # Calculate velocity
      vel_data <- calc_velocity(game_kalshi, snap$timestamp, trailer_price, home_leading, minutes_back = 5)

      # Calculate adjustment
      adj_data <- calc_combined_adjustment(vel_data$velocity)

      # Check if blocked
      if (adj_data$blocked) {
        adj_blocked <- adj_blocked + 1
        next  # Skip this entry
      }

      # Apply spread
      spread_data <- simulate_spread(market_price, game_state)
      fade_exec_price <- if (home_leading) (1 - spread_data$bid) else spread_data$ask

      # Determine outcome
      fade_won <- if (home_leading) !home_won else home_won

      # Calculate bet multiplier from velocity adjustment
      bet_multiplier <- 1.0
      if (adj_data$adjustment >= 1.15) {
        bet_multiplier <- 1.25
        adj_boosted <- adj_boosted + 1
      }

      # Calculate P&L
      if (fade_won) {
        pnl <- (1 - KALSHI_FEE) - fade_exec_price
      } else {
        pnl <- -fade_exec_price
      }

      # Apply bet multiplier to P&L
      pnl <- pnl * bet_multiplier

      adj_trades[[length(adj_trades) + 1]] <- list(
        game_id = game_id,
        timestamp = snap$timestamp,
        trailer_price = trailer_price,
        execution_price = fade_exec_price,
        bet_size = bet_params$bet_size,
        tier = bet_params$tier,
        lead = lead,
        period = period,
        won = fade_won,
        pnl = pnl,
        velocity = vel_data$velocity,
        adjustment = adj_data$adjustment,
        bet_multiplier = bet_multiplier,
        reasons = adj_data$reasons,
        strategy = "v2_with_velocity"
      )

      adj_game_positions <- adj_game_positions + 1
      adj_entries <- adj_entries + 1
    }
  }

  if (g %% 20 == 0) {
    message(sprintf("  Processed %d/%d games... (Base: %d, Adj: %d trades)",
                   g, nrow(unique_games), base_entries, adj_entries))
  }
}

# ------------------------------------------------------------------------------
# Results Analysis
# ------------------------------------------------------------------------------

message("\n", strrep("=", 70))
message("  BACKTEST RESULTS")
message(strrep("=", 70), "\n")

# Convert to tibbles
base_df <- bind_rows(base_trades)
adj_df <- bind_rows(adj_trades)

# Base V2 Results
message("BASE V2 STRATEGY (35-45c, NO HEDGING):")
message(strrep("-", 50))
if (nrow(base_df) > 0) {
  base_wins <- sum(base_df$won)
  base_losses <- nrow(base_df) - base_wins
  base_win_rate <- base_wins / nrow(base_df) * 100

  # Calculate P&L using variable bet sizes per tier
  base_df <- base_df %>%
    mutate(dollar_pnl = pnl * STARTING_BALANCE * bet_size)
  base_total_pnl <- sum(base_df$dollar_pnl)
  base_avg_pnl <- mean(base_df$dollar_pnl)

  message(sprintf("  Total trades:   %d", nrow(base_df)))
  message(sprintf("  Wins/Losses:    %d / %d", base_wins, base_losses))
  message(sprintf("  Win rate:       %.1f%%", base_win_rate))
  message(sprintf("  Total P&L:      $%.2f", base_total_pnl))
  message(sprintf("  Avg P&L/trade:  $%.2f", base_avg_pnl))

  # Tier breakdown
  tier_35_40 <- base_df %>% filter(tier == "35-40c")
  tier_40_45 <- base_df %>% filter(tier == "40-45c")

  message(sprintf("  35-40c tier: %d trades, %.1f%% WR, $%.2f P&L",
                 nrow(tier_35_40),
                 if (nrow(tier_35_40) > 0) mean(tier_35_40$won) * 100 else 0,
                 if (nrow(tier_35_40) > 0) sum(tier_35_40$dollar_pnl) else 0))
  message(sprintf("  40-45c tier: %d trades, %.1f%% WR, $%.2f P&L",
                 nrow(tier_40_45),
                 if (nrow(tier_40_45) > 0) mean(tier_40_45$won) * 100 else 0,
                 if (nrow(tier_40_45) > 0) sum(tier_40_45$dollar_pnl) else 0))
} else {
  message("  No trades")
}

message("")

# V2 + Adjustments Results
message("V2 + IN-GAME VELOCITY ADJUSTMENTS:")
message(strrep("-", 50))
if (nrow(adj_df) > 0) {
  adj_wins <- sum(adj_df$won)
  adj_losses <- nrow(adj_df) - adj_wins
  adj_win_rate <- adj_wins / nrow(adj_df) * 100

  # Calculate P&L using variable bet sizes
  adj_df <- adj_df %>%
    mutate(dollar_pnl = pnl * STARTING_BALANCE * bet_size)
  adj_total_pnl <- sum(adj_df$dollar_pnl)
  adj_avg_pnl <- mean(adj_df$dollar_pnl)

  message(sprintf("  Total trades:   %d", nrow(adj_df)))
  message(sprintf("  Wins/Losses:    %d / %d", adj_wins, adj_losses))
  message(sprintf("  Win rate:       %.1f%%", adj_win_rate))
  message(sprintf("  Total P&L:      $%.2f", adj_total_pnl))
  message(sprintf("  Avg P&L/trade:  $%.2f", adj_avg_pnl))
  message(sprintf("  Entries blocked (sharp spikes): %d", adj_blocked))
  message(sprintf("  Entries boosted (velocity dips): %d", adj_boosted))
} else {
  message("  No trades")
}

message("")

# Comparison
message("COMPARISON:")
message(strrep("-", 50))
if (nrow(base_df) > 0 && nrow(adj_df) > 0) {
  base_total_pnl <- sum(base_df$dollar_pnl)
  adj_total_pnl <- sum(adj_df$dollar_pnl)
  pnl_diff <- adj_total_pnl - base_total_pnl

  base_win_rate <- sum(base_df$won) / nrow(base_df) * 100
  adj_win_rate <- sum(adj_df$won) / nrow(adj_df) * 100

  message(sprintf("  P&L difference:     $%.2f (%s)",
                 pnl_diff, if (pnl_diff > 0) "BETTER" else "WORSE"))
  message(sprintf("  Win rate change:    %+.1f%%", adj_win_rate - base_win_rate))
  message(sprintf("  Trade count change: %d -> %d (%d blocked)",
                 nrow(base_df), nrow(adj_df), adj_blocked))
}

# Velocity breakdown
message("")
message("VELOCITY ADJUSTMENT BREAKDOWN:")
message(strrep("-", 50))

if (nrow(adj_df) > 0) {
  vel_trades <- adj_df %>% filter(!is.na(velocity))
  if (nrow(vel_trades) > 0) {
    dip_trades <- vel_trades %>% filter(velocity <= -VELOCITY_THRESHOLD)
    spike_trades <- vel_trades %>% filter(velocity >= VELOCITY_THRESHOLD)
    neutral_trades <- vel_trades %>% filter(abs(velocity) < VELOCITY_THRESHOLD)

    message(sprintf("  Trades with velocity data: %d", nrow(vel_trades)))
    if (nrow(dip_trades) > 0) {
      message(sprintf("    - Price dips (boost):  %d trades, %.1f%% win rate, $%.2f P&L",
                     nrow(dip_trades),
                     sum(dip_trades$won) / nrow(dip_trades) * 100,
                     sum(dip_trades$dollar_pnl)))
    }
    if (nrow(spike_trades) > 0) {
      message(sprintf("    - Price spikes (penalty): %d trades, %.1f%% win rate, $%.2f P&L",
                     nrow(spike_trades),
                     sum(spike_trades$won) / nrow(spike_trades) * 100,
                     sum(spike_trades$dollar_pnl)))
    }
    if (nrow(neutral_trades) > 0) {
      message(sprintf("    - Neutral: %d trades, %.1f%% win rate, $%.2f P&L",
                     nrow(neutral_trades),
                     sum(neutral_trades$won) / nrow(neutral_trades) * 100,
                     sum(neutral_trades$dollar_pnl)))
    }
  }

  # Boosted trades
  boosted <- adj_df %>% filter(bet_multiplier > 1)
  if (nrow(boosted) > 0) {
    message(sprintf("  Boosted bet trades: %d, %.1f%% win rate, $%.2f P&L",
                   nrow(boosted),
                   sum(boosted$won) / nrow(boosted) * 100,
                   sum(boosted$dollar_pnl)))
  }
}

# Save results
results <- list(
  base_trades = base_df,
  adj_trades = adj_df,
  summary = list(
    base_entries = base_entries,
    adj_entries = adj_entries,
    adj_blocked = adj_blocked,
    adj_boosted = adj_boosted
  )
)
saveRDS(results, "backtest_ingame_results_2026.rds")
message("\nResults saved to backtest_ingame_results_2026.rds")
