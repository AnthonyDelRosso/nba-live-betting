# Backtest KalshiPredictor with Simulated Bid-Ask Spread on January 2026 data
# Tests the impact of realistic market spreads on strategy performance

library(tidyverse)
library(hoopR)

# Source the predictor (which sources the models)
source("kalshi_predictor.R")

message("\n", strrep("=", 70))
message("  BACKTEST WITH SIMULATED BID-ASK SPREAD (JAN 2026)")
message(strrep("=", 70), "\n")

# ------------------------------------------------------------------------------
# Spread Simulation Functions (from paper_trading.R)
# ------------------------------------------------------------------------------

KALSHI_FEE <- 0.07
BASE_SPREAD <- 0.02  # 2 cents base spread

#' Simulate bid-ask spread based on game state
#' @param mid_price The mid-market price (model's fair value)
#' @param game_state Current game state
#' @return List with bid, ask, and spread info
simulate_spread <- function(mid_price, game_state = NULL) {
  spread <- BASE_SPREAD

  if (!is.null(game_state) && is.list(game_state)) {
    period <- game_state$period %||% 1
    seconds_remaining <- game_state$seconds_remaining %||% 2880
    lead <- abs(game_state$home_margin %||% 0)

    # Widen spread in Q4 crunch time
    if (period >= 4 && seconds_remaining < 300) {
      spread <- spread * 1.5
    }

    # Widen spread for close games (more uncertainty)
    if (lead < 5 && period >= 3) {
      spread <- spread * 1.3
    }

    # Tighten spread for blowouts (more certainty)
    if (lead > 20) {
      spread <- spread * 0.7
    }
  }

  # Ensure spread doesn't push prices outside [0.01, 0.99]
  half_spread <- spread / 2
  bid <- max(0.01, mid_price - half_spread)
  ask <- min(0.99, mid_price + half_spread)

  # Recalculate actual spread after bounds
  actual_spread <- ask - bid

  list(
    bid = bid,
    ask = ask,
    mid = mid_price,
    spread = actual_spread,
    spread_pct = actual_spread / mid_price * 100,
    buy_yes_price = ask,      # Pay the ask to buy YES
    buy_no_price = 1 - bid    # Pay 1-bid to buy NO
  )
}

#' Apply spread to market price and recalculate edge
#' @param market_mid_price The mid-market price from Kalshi data
#' @param model_win_prob Model's probability for the position winning
#' @param side "BUY_YES" or "BUY_NO"
#' @param game_state Current game state
#' @return List with edge info
apply_spread_to_trade <- function(market_mid_price, model_win_prob, side, game_state = NULL) {
  # Simulate spread based on game state
  spread_data <- simulate_spread(market_mid_price, game_state)

  # Get execution price (what we actually pay)
  # When buying YES: we pay the ask (mid + half_spread)
  # When buying NO: we pay 1 - bid = 1 - (mid - half_spread)
  if (side == "BUY_YES") {
    execution_price <- spread_data$buy_yes_price  # = ask
    mid_execution <- market_mid_price
  } else {
    execution_price <- spread_data$buy_no_price   # = 1 - bid
    mid_execution <- 1 - market_mid_price
  }

  # Calculate breakeven probabilities using fee-on-profit model:
  #   payout_if_win = 1 - KALSHI_FEE * (1 - price)
  #   breakeven     = price / payout_if_win
  # This matches paper_trading.R. The old formula (price / (1 - fee)) incorrectly
  # applied the fee to the full $1 payout rather than only the profit.
  breakeven_with_spread <- execution_price / (1 - KALSHI_FEE * (1 - execution_price))
  breakeven_at_mid <- mid_execution / (1 - KALSHI_FEE * (1 - mid_execution))

  # Edge calculations
  edge_at_mid <- model_win_prob - breakeven_at_mid
  edge_after_spread <- model_win_prob - breakeven_with_spread
  edge_lost <- edge_at_mid - edge_after_spread

  list(
    execution_price = execution_price,
    mid_execution = mid_execution,
    spread = spread_data$spread,
    spread_pct = spread_data$spread_pct,
    edge_at_mid = edge_at_mid,
    edge_after_spread = edge_after_spread,
    edge_lost = edge_lost,
    breakeven_at_mid = breakeven_at_mid,
    breakeven_with_spread = breakeven_with_spread,
    model_win_prob = model_win_prob
  )
}

# ------------------------------------------------------------------------------
# Initialize Predictor
# ------------------------------------------------------------------------------

message("Initializing KalshiPredictor...")
predictor <- KalshiPredictor$new()
predictor_spread <- KalshiPredictor$new()  # Independent predictor for spread scenario

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

kalshi_data <- readRDS("kalshi_nba_full_data.rds")

# Filter to home team markets only
kalshi_data <- kalshi_data %>%
  filter(market_team == home_team)

message(sprintf("Loaded %d Kalshi observations", nrow(kalshi_data)))

# Load schedule and PBP
schedule <- load_nba_schedule(seasons = 2026) %>%
  filter(status_type_completed == TRUE) %>%
  select(game_id, game_date, home_name = home_display_name, away_name = away_display_name,
         home_abbrev = home_abbreviation, away_abbrev = away_abbreviation,
         home_score, away_score, home_winner) %>%
  mutate(game_date = as.Date(game_date))

pbp <- load_nba_pbp(seasons = 2026)

# Load team ratings
message("Loading team ratings...")
team_box <- load_nba_team_box(seasons = 2026)

team_ratings <- team_box %>%
  filter(season_type == 2) %>%
  arrange(team_abbreviation, game_date) %>%
  mutate(
    poss = field_goals_attempted + 0.44 * free_throws_attempted - offensive_rebounds + turnovers
  ) %>%
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
# Process Games - WITH and WITHOUT spread
# ------------------------------------------------------------------------------

# Tier-based fade bet sizing (matches kalshi_predictor.R and backtest_ingame_adjustments_2026.R)
FADE_BET_SIZE_35_40 <- 0.03   # 3% for 35-40c tier (best risk/reward, all periods)
FADE_BET_SIZE_40_45 <- 0.02   # 2% for 40-45c tier (Q3/Q4 only)
STARTING_BALANCE <- 1000

#' Return the tier-appropriate fade bet size given the trailer's entry price
get_fade_bet_size <- function(entry_price) {
  if (entry_price < 0.40) FADE_BET_SIZE_35_40 else FADE_BET_SIZE_40_45
}

# Tracking for NO SPREAD scenario
no_spread_fade_positions <- list()
no_spread_wl_positions <- list()

# Tracking for WITH SPREAD scenario
spread_fade_positions <- list()
spread_wl_positions <- list()

# Counters
fade_count_no_spread <- 0
wl_count_no_spread <- 0
fade_count_spread <- 0
wl_count_spread <- 0
fade_skipped_due_to_spread <- 0
wl_skipped_due_to_spread <- 0
games_processed <- 0

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
      home_margin = home_score - away_score,
      period = period_number,
      seconds_elapsed = case_when(
        period_number <= 4 ~ (period_number - 1) * 720 + (720 - (clock_minutes * 60 + clock_seconds)),
        TRUE ~ 2880 + (period_number - 5) * 300 + (300 - (clock_minutes * 60 + clock_seconds))
      ),
      seconds_remaining = 2880 - seconds_elapsed
    ) %>%
    arrange(pbp_timestamp)

  if (nrow(game_pbp) == 0) next

  game_kalshi <- kalshi_data %>%
    filter(ticker == game_info$ticker) %>%
    arrange(timestamp)

  if (nrow(game_kalshi) == 0) next

  games_processed <- games_processed + 1

  # Reset predictors for this game
  predictor$game_positions[[game_id]] <- NULL
  predictor$open_fades[[game_id]] <- NULL
  predictor_spread$game_positions[[game_id]] <- NULL
  predictor_spread$open_fades[[game_id]] <- NULL

  # Process each Kalshi observation
  for (i in 1:nrow(game_kalshi)) {
    kalshi_ts <- game_kalshi$timestamp[i]
    kalshi_price <- game_kalshi$price[i]

    if (is.na(kalshi_price)) next

    # Match to PBP
    diffs <- abs(difftime(game_pbp$pbp_timestamp, kalshi_ts, units = "secs"))
    idx <- which.min(diffs)

    if (as.numeric(diffs[idx]) > 600) next

    pbp_row <- game_pbp[idx, ]

    # Create game state
    game_state <- create_game_state(
      home_score = pbp_row$home_score,
      away_score = pbp_row$away_score,
      seconds_remaining = max(0, pbp_row$seconds_remaining),
      period = pbp_row$period,
      home_net_rating = home_rating,
      away_net_rating = away_rating
    )

    home_leading <- game_state$home_margin > 0
    lead <- abs(game_state$home_margin)

    # ========== NO SPREAD SCENARIO ==========
    signal_no_spread <- tryCatch({
      predictor$get_signal(game_id, game_state, kalshi_price, kalshi_ts)
    }, error = function(e) NULL)

    if (!is.null(signal_no_spread)) {
      # Record fade position (NO SPREAD)
      if (signal_no_spread$fade$signal != "HOLD") {
        fade_count_no_spread <- fade_count_no_spread + 1

        leader_at_entry_won <- (home_leading & home_won) | (!home_leading & !home_won)
        fade_won <- !leader_at_entry_won

        fade_bet_size_ns <- get_fade_bet_size(signal_no_spread$fade$entry_price)

        no_spread_fade_positions[[length(no_spread_fade_positions) + 1]] <- tibble(
          position_id = fade_count_no_spread,
          game_date = game_info$game_date,
          matchup = sprintf("%s @ %s", sched_game$away_name, sched_game$home_name),
          period = game_state$period,
          lead = lead,
          home_leading = home_leading,
          entry_price = signal_no_spread$fade$entry_price,
          bet_size = fade_bet_size_ns,
          trailer_win_prob = signal_no_spread$fade$blown_rate,
          breakeven = signal_no_spread$fade$breakeven,
          edge = signal_no_spread$fade$edge,
          spread = 0,
          edge_after_spread = signal_no_spread$fade$edge,
          fade_won = fade_won,
          pnl = ifelse(fade_won,
                       (1 - signal_no_spread$fade$entry_price) * 0.93 * fade_bet_size_ns * 1000,
                       -signal_no_spread$fade$entry_price * fade_bet_size_ns * 1000)
        )

        predictor$record_position(game_id, "fade", signal_no_spread$fade, kalshi_ts)
      }

      # Record with_leader position (NO SPREAD)
      if (signal_no_spread$with_leader$signal != "HOLD") {
        wl_count_no_spread <- wl_count_no_spread + 1

        wl_won <- (home_leading & home_won) | (!home_leading & !home_won)

        no_spread_wl_positions[[length(no_spread_wl_positions) + 1]] <- tibble(
          position_id = wl_count_no_spread,
          game_date = game_info$game_date,
          matchup = sprintf("%s @ %s", sched_game$away_name, sched_game$home_name),
          period = game_state$period,
          lead = lead,
          home_leading = home_leading,
          entry_price = signal_no_spread$with_leader$entry_price,
          hold_rate = signal_no_spread$with_leader$hold_rate,
          spread = 0,
          wl_won = wl_won,
          pnl = ifelse(wl_won,
                       (1 - signal_no_spread$with_leader$entry_price) * 0.93 * signal_no_spread$with_leader$bet_size * 1000,
                       -signal_no_spread$with_leader$entry_price * signal_no_spread$with_leader$bet_size * 1000)
        )

        predictor$record_position(game_id, "with_leader", signal_no_spread$with_leader, kalshi_ts)
      }
    }

    # ========== WITH SPREAD SCENARIO ==========
    # Generate independent signal using predictor_spread's own position tracking.
    # This ensures position limits (max fades, min time between entries, max 1 WL)
    # reflect only trades actually taken in the spread scenario, not the no-spread scenario.
    signal_spread <- tryCatch({
      predictor_spread$get_signal(game_id, game_state, kalshi_price, kalshi_ts)
    }, error = function(e) NULL)

    if (!is.null(signal_spread)) {
      # Process fade with spread
      if (signal_spread$fade$signal != "HOLD") {
        fade_side <- signal_spread$fade$signal  # BUY_YES or BUY_NO

        # For fade: we're betting on the trailer
        market_mid_trailer <- signal_spread$fade$entry_price
        model_trailer_win_prob <- signal_spread$fade$blown_rate

        # Apply spread to the trade
        spread_info <- apply_spread_to_trade(
          market_mid_price = if (fade_side == "BUY_YES") market_mid_trailer else (1 - market_mid_trailer),
          model_win_prob = model_trailer_win_prob,
          side = fade_side,
          game_state = game_state
        )

        # Only take trade if edge still positive after spread (use 2% threshold)
        if (spread_info$edge_after_spread >= 0.02) {
          fade_count_spread <- fade_count_spread + 1

          leader_at_entry_won <- (home_leading & home_won) | (!home_leading & !home_won)
          fade_won <- !leader_at_entry_won

          # Use the pre-spread mid price to determine the tier, not the execution price
          fade_bet_size_sp <- get_fade_bet_size(market_mid_trailer)

          spread_fade_positions[[length(spread_fade_positions) + 1]] <- tibble(
            position_id = fade_count_spread,
            game_date = game_info$game_date,
            matchup = sprintf("%s @ %s", sched_game$away_name, sched_game$home_name),
            period = game_state$period,
            lead = lead,
            home_leading = home_leading,
            entry_price = spread_info$execution_price,
            mid_price = spread_info$mid_execution,
            bet_size = fade_bet_size_sp,
            trailer_win_prob = model_trailer_win_prob,
            breakeven = spread_info$breakeven_with_spread,
            edge = spread_info$edge_at_mid,
            spread = spread_info$spread,
            spread_pct = spread_info$spread_pct,
            edge_after_spread = spread_info$edge_after_spread,
            edge_lost = spread_info$edge_lost,
            fade_won = fade_won,
            pnl = ifelse(fade_won,
                         (1 - spread_info$execution_price) * 0.93 * fade_bet_size_sp * 1000,
                         -spread_info$execution_price * fade_bet_size_sp * 1000)
          )

          adjusted_signal <- signal_spread$fade
          adjusted_signal$entry_price <- spread_info$execution_price
          predictor_spread$record_position(game_id, "fade", adjusted_signal, kalshi_ts)
        } else {
          fade_skipped_due_to_spread <- fade_skipped_due_to_spread + 1
        }
      }

      # Process with_leader with spread
      if (signal_spread$with_leader$signal != "HOLD") {
        wl_side <- signal_spread$with_leader$signal  # BUY_YES or BUY_NO

        # For with_leader: we're betting on the leader
        market_mid_leader <- signal_spread$with_leader$entry_price
        model_leader_win_prob <- signal_spread$with_leader$hold_rate

        # Apply spread to the trade
        spread_info <- apply_spread_to_trade(
          market_mid_price = if (wl_side == "BUY_YES") market_mid_leader else (1 - market_mid_leader),
          model_win_prob = model_leader_win_prob,
          side = wl_side,
          game_state = game_state
        )

        # With leader has built-in edge from high win rate, use 0% threshold
        if (spread_info$edge_after_spread >= 0) {
          wl_count_spread <- wl_count_spread + 1

          wl_won <- (home_leading & home_won) | (!home_leading & !home_won)

          spread_wl_positions[[length(spread_wl_positions) + 1]] <- tibble(
            position_id = wl_count_spread,
            game_date = game_info$game_date,
            matchup = sprintf("%s @ %s", sched_game$away_name, sched_game$home_name),
            period = game_state$period,
            lead = lead,
            home_leading = home_leading,
            entry_price = spread_info$execution_price,
            mid_price = spread_info$mid_execution,
            hold_rate = model_leader_win_prob,
            spread = spread_info$spread,
            spread_pct = spread_info$spread_pct,
            edge_after_spread = spread_info$edge_after_spread,
            wl_won = wl_won,
            pnl = ifelse(wl_won,
                         (1 - spread_info$execution_price) * 0.93 * signal_spread$with_leader$bet_size * 1000,
                         -spread_info$execution_price * signal_spread$with_leader$bet_size * 1000)
          )

          adjusted_signal <- signal_spread$with_leader
          adjusted_signal$entry_price <- spread_info$execution_price
          predictor_spread$record_position(game_id, "with_leader", adjusted_signal, kalshi_ts)
        } else {
          wl_skipped_due_to_spread <- wl_skipped_due_to_spread + 1
        }
      }
    }
  }

  if (games_processed %% 50 == 0) {
    message(sprintf("  Processed %d games...", games_processed))
    message(sprintf("    No Spread: %d fade, %d WL", fade_count_no_spread, wl_count_no_spread))
    message(sprintf("    With Spread: %d fade, %d WL (skipped: %d fade, %d WL)",
                    fade_count_spread, wl_count_spread,
                    fade_skipped_due_to_spread, wl_skipped_due_to_spread))
  }
}

# ------------------------------------------------------------------------------
# Results
# ------------------------------------------------------------------------------

no_spread_fade_df <- bind_rows(no_spread_fade_positions)
no_spread_wl_df <- bind_rows(no_spread_wl_positions)
spread_fade_df <- bind_rows(spread_fade_positions)
spread_wl_df <- bind_rows(spread_wl_positions)

message("\n", strrep("=", 70))
message("  BACKTEST RESULTS: IMPACT OF BID-ASK SPREAD")
message(strrep("=", 70))

message(sprintf("\nGames processed: %d", games_processed))
message(sprintf("Base spread: %.0f cents (%.1f%%)", BASE_SPREAD * 100, BASE_SPREAD * 100))

# ========================
# NO SPREAD RESULTS
# ========================
message("\n", strrep("-", 50))
message("  SCENARIO 1: NO SPREAD (Mid-Price Execution)")
message(strrep("-", 50))

message("\n--- FADE STRATEGY (No Spread) ---")
if (nrow(no_spread_fade_df) > 0) {
  message(sprintf("Total positions: %d", nrow(no_spread_fade_df)))
  message(sprintf("Win rate: %.1f%% (%d/%d)",
                  mean(no_spread_fade_df$fade_won) * 100,
                  sum(no_spread_fade_df$fade_won),
                  nrow(no_spread_fade_df)))
  message(sprintf("Total P&L: $%.2f", sum(no_spread_fade_df$pnl)))
  message(sprintf("Avg P&L per trade: $%.2f", mean(no_spread_fade_df$pnl)))
}

message("\n--- WITH LEADER STRATEGY (No Spread) ---")
if (nrow(no_spread_wl_df) > 0) {
  message(sprintf("Total positions: %d", nrow(no_spread_wl_df)))
  message(sprintf("Win rate: %.1f%% (%d/%d)",
                  mean(no_spread_wl_df$wl_won) * 100,
                  sum(no_spread_wl_df$wl_won),
                  nrow(no_spread_wl_df)))
  message(sprintf("Total P&L: $%.2f", sum(no_spread_wl_df$pnl)))
}

no_spread_total_pnl <- sum(no_spread_fade_df$pnl, na.rm = TRUE) + sum(no_spread_wl_df$pnl, na.rm = TRUE)
message(sprintf("\nCOMBINED P&L (No Spread): $%.2f", no_spread_total_pnl))

# ========================
# WITH SPREAD RESULTS
# ========================
message("\n", strrep("-", 50))
message("  SCENARIO 2: WITH SPREAD (Realistic Execution)")
message(strrep("-", 50))

message("\n--- FADE STRATEGY (With Spread) ---")
if (nrow(spread_fade_df) > 0) {
  message(sprintf("Total positions: %d (skipped %d due to negative edge)",
                  nrow(spread_fade_df), fade_skipped_due_to_spread))
  message(sprintf("Win rate: %.1f%% (%d/%d)",
                  mean(spread_fade_df$fade_won) * 100,
                  sum(spread_fade_df$fade_won),
                  nrow(spread_fade_df)))
  message(sprintf("Total P&L: $%.2f", sum(spread_fade_df$pnl)))
  message(sprintf("Avg P&L per trade: $%.2f", mean(spread_fade_df$pnl)))

  message("\nSpread Statistics:")
  message(sprintf("  Average spread: %.1f cents (%.1f%%)",
                  mean(spread_fade_df$spread) * 100,
                  mean(spread_fade_df$spread_pct)))
  message(sprintf("  Avg edge before spread: %.1f%%", mean(spread_fade_df$edge) * 100))
  message(sprintf("  Avg edge after spread: %.1f%%", mean(spread_fade_df$edge_after_spread) * 100))
  message(sprintf("  Avg edge lost to spread: %.1f%%", mean(spread_fade_df$edge_lost) * 100))
}

message("\n--- WITH LEADER STRATEGY (With Spread) ---")
if (nrow(spread_wl_df) > 0) {
  message(sprintf("Total positions: %d (skipped %d due to negative edge)",
                  nrow(spread_wl_df), wl_skipped_due_to_spread))
  message(sprintf("Win rate: %.1f%% (%d/%d)",
                  mean(spread_wl_df$wl_won) * 100,
                  sum(spread_wl_df$wl_won),
                  nrow(spread_wl_df)))
  message(sprintf("Total P&L: $%.2f", sum(spread_wl_df$pnl)))

  message("\nSpread Statistics:")
  message(sprintf("  Average spread: %.1f cents (%.1f%%)",
                  mean(spread_wl_df$spread) * 100,
                  mean(spread_wl_df$spread_pct)))
}

spread_total_pnl <- sum(spread_fade_df$pnl, na.rm = TRUE) + sum(spread_wl_df$pnl, na.rm = TRUE)
message(sprintf("\nCOMBINED P&L (With Spread): $%.2f", spread_total_pnl))

# ========================
# COMPARISON
# ========================
message("\n", strrep("=", 70))
message("  SPREAD IMPACT SUMMARY")
message(strrep("=", 70))

pnl_difference <- no_spread_total_pnl - spread_total_pnl
pnl_pct_reduction <- (pnl_difference / no_spread_total_pnl) * 100

message(sprintf("\nP&L without spread: $%.2f", no_spread_total_pnl))
message(sprintf("P&L with spread:    $%.2f", spread_total_pnl))
message(sprintf("P&L reduction:      $%.2f (%.1f%%)", pnl_difference, pnl_pct_reduction))

message(sprintf("\nPositions without spread: %d fade + %d WL = %d total",
                nrow(no_spread_fade_df), nrow(no_spread_wl_df),
                nrow(no_spread_fade_df) + nrow(no_spread_wl_df)))
message(sprintf("Positions with spread:    %d fade + %d WL = %d total",
                nrow(spread_fade_df), nrow(spread_wl_df),
                nrow(spread_fade_df) + nrow(spread_wl_df)))
message(sprintf("Trades filtered out:      %d fade + %d WL = %d total",
                fade_skipped_due_to_spread, wl_skipped_due_to_spread,
                fade_skipped_due_to_spread + wl_skipped_due_to_spread))

# ROI comparison
no_spread_roi <- (no_spread_total_pnl / STARTING_BALANCE) * 100
spread_roi <- (spread_total_pnl / STARTING_BALANCE) * 100

message(sprintf("\nROI without spread: %.1f%%", no_spread_roi))
message(sprintf("ROI with spread:    %.1f%%", spread_roi))

# ========================
# Analysis by Period (Spread scenario)
# ========================
if (nrow(spread_fade_df) > 0) {
  message("\n--- FADE BY PERIOD (With Spread) ---")
  spread_fade_df %>%
    group_by(period) %>%
    summarise(
      n = n(),
      wins = sum(fade_won),
      win_rate = mean(fade_won) * 100,
      pnl = sum(pnl),
      avg_spread_pct = mean(spread_pct),
      avg_edge_after = mean(edge_after_spread) * 100,
      .groups = "drop"
    ) %>%
    print()
}

if (nrow(spread_wl_df) > 0) {
  message("\n--- WITH LEADER BY PERIOD (With Spread) ---")
  spread_wl_df %>%
    group_by(period) %>%
    summarise(
      n = n(),
      wins = sum(wl_won),
      win_rate = mean(wl_won) * 100,
      pnl = sum(pnl),
      avg_spread_pct = mean(spread_pct),
      .groups = "drop"
    ) %>%
    print()
}

message("\n", strrep("=", 70))
message("  BACKTEST COMPLETE")
message(strrep("=", 70))

# Save results
results <- list(
  no_spread = list(
    fade = no_spread_fade_df,
    wl = no_spread_wl_df,
    total_pnl = no_spread_total_pnl
  ),
  with_spread = list(
    fade = spread_fade_df,
    wl = spread_wl_df,
    total_pnl = spread_total_pnl,
    fade_skipped = fade_skipped_due_to_spread,
    wl_skipped = wl_skipped_due_to_spread
  ),
  summary = list(
    games_processed = games_processed,
    pnl_reduction = pnl_difference,
    pnl_reduction_pct = pnl_pct_reduction
  )
)

saveRDS(results, "backtest_spread_results_2026.rds")
message("\nResults saved to backtest_spread_results_2026.rds")
