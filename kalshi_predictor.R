# Kalshi NBA Live Betting Predictor
# Combines Win Probability Model + Blown Lead Model for edge detection
#
# TWO STRATEGIES:
#
# 1. WITH LEADER: Bet on leading team to hold lead
#    - Entry: Q3/Q4 at 15+ pts OR any period at 21+ pts
#    - Exit: Hold to settlement (NO stop-loss - hurts performance)
#    - Max 1 position per game (re-entry hurts performance)
#    - Backtest: $31 P&L, 0.48 Sharpe, 95.4% win rate
#
# 2. FADE V2 (Market-Based): Bet on trailing team when MARKET shows uncertainty
#    - Entry: 7+ pt lead AND trailer market price >= 35c
#    - Exit: Hold to settlement
#    - Max 3 positions per game
#    - KEY INSIGHT: When market prices trailer at 35c+, actual win rate is 53%
#    - This aligns WITH market uncertainty, not against it
#    - Backtest V2: 15 trades, 53% win rate, +$32 P&L
#
# Why V2 works better than V1 (model-based edge):
#   - V1 (model edge >= 5%): 267 trades, 18% win rate, -$106 P&L
#   - V2 (market >= 35c): 15 trades, 53% win rate, +$32 P&L
#   - Market is more accurate than model in "edge" situations
#   - When model >> market, the model is usually wrong (selection bias)
#
# Automated Trading Mode:
#   - Fade: Up to MAX_FADE_POSITIONS_PER_GAME entries (default 3)
#   - With Leader: Max 1 entry per game
#
# Usage:
#   source("kalshi_predictor.R")
#   predictor <- KalshiPredictor$new()
#   predictor$print_config()
#
#   # During game loop:
#   signal <- predictor$get_signal(game_id, game_state, market_price)
#   if (signal$fade$signal != "HOLD") {
#     # Execute fade trade
#     predictor$record_position(game_id, "fade", signal$fade)
#   }

library(tidyverse)
library(R6)

# Source the underlying models
source("win_probability_model.R")
source("nba_largest_lead.R")

# ------------------------------------------------------------------------------
# Actual Historical Blown Lead Rates (from 2022-2026 seasons, 5888 games)
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

# ------------------------------------------------------------------------------
# Constants
# ------------------------------------------------------------------------------

# WITH LEADER Strategy Parameters
WITH_LEADER_MIN_LEAD_Q3Q4 <- 15   # Minimum lead for Q3/Q4 entry
WITH_LEADER_MIN_LEAD_ANY <- 21   # Minimum lead for any period entry
MAX_WITH_LEADER_PER_GAME <- 1    # Re-entry hurts performance

# FADE V2 Strategy Parameters (Market-Based Entry)
FADE_MIN_LEAD <- 7               # Minimum lead for fade consideration
FADE_MIN_MARKET_PRICE <- 0.35    # V2: Only fade when trailer price >= 35c
FADE_MAX_MARKET_PRICE <- 0.45    # V2: Cap at 45c - above this breakeven exceeds win rate
                                 # Backtest: 35-45c = +$484, 45c+ = -$1,078
MAX_FADE_POSITIONS_PER_GAME <- 3 # V2: Reduced from 10 - fewer, higher quality trades
KALSHI_FEE <- 0.07               # 7% Kalshi fee on wins

# Bet sizing by price tier (no hedging - it doesn't improve EV)
BET_SIZE_35_40 <- 0.03           # 3% at 35-40c (best risk/reward)
BET_SIZE_40_45 <- 0.02           # 2% at 40-45c (still profitable)

# General Parameters
MIN_SECONDS_FOR_SIGNAL <- 60     # Don't signal in final minute
MIN_MINUTES_BETWEEN_FADES <- 2   # Avoid rapid-fire entries on same price move

# In-Game Adjustment Parameters
# Based on empirical analysis (analyze_ingame_thresholds.R):
#   - Velocity: 4.5c/min threshold shows 50% win rate difference (dips vs spikes)
#   - Run differential: No predictive signal found - DISABLED
PRICE_VELOCITY_THRESHOLD <- 0.045 # 4.5c/min - empirically determined
PRICE_VELOCITY_BOOST <- 1.20      # Boost when price drops (buy the dip)
PRICE_VELOCITY_PENALTY <- 0.80    # Penalty when price spikes (likely reversal)
MAX_INGAME_ADJUSTMENT <- 1.50     # Cap total adjustment at 50% boost

# Entry blocking thresholds
BLOCK_ON_SHARP_SPIKE <- TRUE      # Block fade entry when price spiking sharply
SHARP_SPIKE_THRESHOLD <- 0.06     # 6c/min = very sharp spike, block entry

# Run differential - DISABLED (no predictive signal found in analysis)
USE_RUN_DIFFERENTIAL <- FALSE     # Set to TRUE to re-enable
RUN_DIFF_THRESHOLD <- 8           # (unused when disabled)
RUN_DIFF_BOOST_SMALL <- 1.15      # (unused when disabled)
RUN_DIFF_BOOST_LARGE <- 1.30      # (unused when disabled)
BOOST_ON_BIG_RUN <- FALSE         # Disabled - no predictive signal
BIG_RUN_THRESHOLD <- 10           # (unused when disabled)

# Tiered bet sizing
BET_SIZE_TIER1 <- 0.05           # MAX: Q4 + 20+ pts lead
BET_SIZE_TIER2 <- 0.04           # LARGE: Q4 + 15+ pts OR any 21+ pts
BET_SIZE_TIER3 <- 0.03           # MEDIUM: Strong edge (5%+)
BET_SIZE_TIER4 <- 0.02           # STANDARD: All other signals

# ------------------------------------------------------------------------------
# Helper Functions
# ------------------------------------------------------------------------------

#' Calculate breakeven blown rate for fade strategy
calc_breakeven_blown <- function(trailer_price) {
  trailer_price / (trailer_price + (1 - trailer_price) * (1 - KALSHI_FEE))
}

#' Get bet size for a given trailer price
#' Period-filtered sizing based on backtest analysis.
#'
#' Backtest findings:
#'   35-40c: Net profitable (+$26) across all periods
#'   40-45c: Q1/Q2 loses (-$840), Q3/Q4 wins (+$529)
#'
#' Filter: 40-45c restricted to Q3/Q4 only
#'
#' @param trailer_price Current trailer market price
#' @param period Game period (1-4)
#' @return List with bet size and tier info
get_bet_size_for_price <- function(trailer_price, period = NULL) {
  if (trailer_price < FADE_MIN_MARKET_PRICE || trailer_price > FADE_MAX_MARKET_PRICE) {
    return(list(eligible = FALSE, reason = "Price out of range", bet_size = NA))
  }

  if (trailer_price < 0.40) {
    # 35-40c tier: all periods (net profitable)
    return(list(
      eligible = TRUE,
      tier = "35-40c",
      bet_size = BET_SIZE_35_40,
      breakeven = calc_breakeven_blown(trailer_price)
    ))
  } else {
    # 40-45c tier: Q3/Q4 only (61-75% WR vs 47% breakeven)
    if (!is.null(period) && period < 3) {
      return(list(
        eligible = FALSE,
        reason = sprintf("40-45c requires Q3/Q4 (currently Q%d)", period),
        bet_size = NA
      ))
    }
    return(list(
      eligible = TRUE,
      tier = "40-45c",
      bet_size = BET_SIZE_40_45,
      breakeven = calc_breakeven_blown(trailer_price)
    ))
  }
}

#' Calibrate blown lead probability using market-adaptive shrinkage
#' The blown lead model overestimates by ~11% on average. This calibration
#' blends the model prediction with the market price, using market price
#' level to determine the blend weight.
#'
#' Key findings from calibration analysis:
#'   - Model 11.3% overconfident overall (predicts 29%, actual 18%)
#'   - Market only 1% error (predicts 19%, actual 18%)
#'   - Model worst at 25-35c market (27% error!)
#'   - Model best at 35c+ market (-3% error, slightly underestimates)
#'   - Q4 leads almost never blown (0% actual)
#'
#' @param model_prob Blown lead probability from model
#' @param market_price Trailer's market price
#' @param period Game period (1-4)
#' @return Calibrated blown lead probability
calibrate_blown_prob <- function(model_prob, market_price, period) {
  # Q4 override: leads almost never blown in 4th quarter
  if (period == 4) {
    return(model_prob * 0.1)
  }

  # Market-adaptive shrinkage
  # Lower market prices = trust market more (model overconfident)
  # Higher market prices = trust model more (it's accurate at 35c+)
  if (market_price < 0.18) {
    alpha <- 0.3  # 30% model, 70% market (model very overconfident here)
  } else if (market_price < 0.35) {
    alpha <- 0.5  # 50/50 blend
  } else {
    alpha <- 0.8  # 80% model, 20% market (model accurate at high prices)
  }

  calibrated <- alpha * model_prob + (1 - alpha) * market_price
  return(calibrated)
}

#' Calculate run differential adjustment
#' NOTE: Empirical analysis found NO predictive signal for run differential.
#' This function is DISABLED by default (USE_RUN_DIFFERENTIAL = FALSE).
#'
#' @param trailer_pts_last_5 Trailer's points in last 5 minutes
#' @param leader_pts_last_5 Leader's points in last 5 minutes
#' @return Adjustment multiplier (1.0 = no adjustment)
calc_run_differential_adjustment <- function(trailer_pts_last_5, leader_pts_last_5) {
  # DISABLED - no predictive signal found in empirical analysis
  if (!USE_RUN_DIFFERENTIAL) {
    return(1.0)
  }

  if (is.null(trailer_pts_last_5) || is.null(leader_pts_last_5)) {
    return(1.0)  # No data available
  }

  run_diff <- trailer_pts_last_5 - leader_pts_last_5

  if (run_diff >= 12) {
    return(RUN_DIFF_BOOST_LARGE)
  } else if (run_diff >= RUN_DIFF_THRESHOLD) {
    return(RUN_DIFF_BOOST_SMALL)
  } else if (run_diff <= -8) {
    return(0.85)
  }

  return(1.0)
}

#' Calculate price velocity adjustment
#' EMPIRICALLY VALIDATED: Analysis of 2026 Kalshi data shows:
#'   - At 4.5c/min threshold, dips show 50% HIGHER win rate than spikes
#'   - Contrarian logic (buy dips, avoid spikes) is SUPPORTED by data
#'
#' Contrarian logic:
#'   - Sharp rise → reduce confidence (likely to reverse) → 20% penalty
#'   - Sharp fall → boost confidence (buying the dip) → 20% boost
#'   - Stable prices → neutral
#'
#' @param current_price Current trailer price
#' @param price_5min_ago Trailer price 5 minutes ago
#' @param minutes_elapsed Time between price snapshots (default 5)
#' @return Adjustment multiplier (1.0 = no adjustment)
calc_price_velocity_adjustment <- function(current_price, price_5min_ago, minutes_elapsed = 5) {
  if (is.null(price_5min_ago) || is.na(price_5min_ago)) {
    return(1.0)  # No historical price data
  }

  # Price velocity = change per minute
  velocity <- (current_price - price_5min_ago) / minutes_elapsed

  # CONTRARIAN LOGIC (empirically validated - 50% win rate difference)
  if (velocity >= PRICE_VELOCITY_THRESHOLD) {
    # Sharp rise in trailer price → likely to reverse down
    # REDUCE confidence in fade
    return(PRICE_VELOCITY_PENALTY)  # 0.80 (20% penalty)
  } else if (velocity <= -PRICE_VELOCITY_THRESHOLD) {
    # Sharp fall in trailer price → likely to bounce back
    # Good entry point for fade (buying the dip)
    return(PRICE_VELOCITY_BOOST)  # 1.20 (20% boost)
  }

  return(1.0)
}

#' Calculate combined in-game adjustment
#' Combines run differential and price velocity adjustments.
#'
#' @param live_data List with live game data:
#'   - trailer_pts_last_5: Trailer's points in last 5 minutes
#'   - leader_pts_last_5: Leader's points in last 5 minutes
#'   - trailer_price_5min_ago: Trailer's market price 5 minutes ago
#' @param current_trailer_price Current trailer market price
#' @return List with adjustment factor and breakdown
calc_ingame_adjustment <- function(live_data, current_trailer_price) {
  # Initialize adjustments
  run_adj <- 1.0
  velocity_adj <- 1.0
  reasons <- character()

  # Run differential adjustment
  if (!is.null(live_data$trailer_pts_last_5) && !is.null(live_data$leader_pts_last_5)) {
    run_adj <- calc_run_differential_adjustment(
      live_data$trailer_pts_last_5,
      live_data$leader_pts_last_5
    )
    run_diff <- live_data$trailer_pts_last_5 - live_data$leader_pts_last_5

    if (run_adj > 1.0) {
      reasons <- c(reasons, sprintf("Trailer on %+d run (%.0f%% boost)",
                                    run_diff, (run_adj - 1) * 100))
    } else if (run_adj < 1.0) {
      reasons <- c(reasons, sprintf("Leader extending (%+d run)", run_diff))
    }
  }

  # Price velocity adjustment (CONTRARIAN - mean reversion logic)
  if (!is.null(live_data$trailer_price_5min_ago)) {
    velocity_adj <- calc_price_velocity_adjustment(
      current_trailer_price,
      live_data$trailer_price_5min_ago
    )
    velocity <- (current_trailer_price - live_data$trailer_price_5min_ago) / 5

    if (velocity_adj > 1.0) {
      # Price dropped sharply → likely to bounce back (good entry)
      reasons <- c(reasons, sprintf("Price dip (%.1fc/min) - buy the dip %.0f%% boost",
                                    velocity * 100, (velocity_adj - 1) * 100))
    } else if (velocity_adj < 1.0) {
      # Price spiked sharply → likely to reverse (wait for pullback)
      reasons <- c(reasons, sprintf("Price spike (+%.1fc/min) - likely reversal, -%.0f%%",
                                    velocity * 100, (1 - velocity_adj) * 100))
    }
  }

  # Combine adjustments (multiplicative)
  combined_adj <- run_adj * velocity_adj

  # Cap at maximum adjustment
  combined_adj <- min(combined_adj, MAX_INGAME_ADJUSTMENT)

  list(
    adjustment = combined_adj,
    run_adjustment = run_adj,
    velocity_adjustment = velocity_adj,
    reasons = if (length(reasons) > 0) paste(reasons, collapse = "; ") else "No in-game adjustments",
    has_live_data = !is.null(live_data$trailer_pts_last_5) || !is.null(live_data$trailer_price_5min_ago)
  )
}

# ------------------------------------------------------------------------------
# KalshiPredictor R6 Class
# ------------------------------------------------------------------------------

KalshiPredictor <- R6::R6Class(

"KalshiPredictor",

  public = list(
    wp_model = NULL,
    blown_lead_model = NULL,
    adjustment_weight = NULL,

    # Position tracking
    positions = NULL,  # Tibble of all positions
    game_positions = NULL,  # List tracking positions per game
    open_fades = NULL,  # Track open fade position per game (who we're fading)

    # Configuration
    max_fade_per_game = NULL,
    max_wl_per_game = NULL,
    min_minutes_between_fades = NULL,
    fade_mode = NULL,  # "market" (V2) or "calibrated"

    #' Initialize the predictor with both models
    #' @param fade_mode "market" (V2, recommended) or "calibrated"
    #'   - "market": Entry when trailer price >= 35c (53% historical win rate)
    #'   - "calibrated": Entry when calibrated model edge >= 5% (28% win rate)
    initialize = function(wp_model_path = "win_probability_model.rds",
                          blown_lead_model_path = "blown_lead_model_v2.rds",
                          adjustment_weight = 1.0,
                          max_fade_per_game = MAX_FADE_POSITIONS_PER_GAME,
                          max_wl_per_game = MAX_WITH_LEADER_PER_GAME,
                          min_minutes_between_fades = MIN_MINUTES_BETWEEN_FADES,
                          fade_mode = "market") {

      message("Loading Win Probability model...")
      self$wp_model <- load_wp_model(wp_model_path)

      message("Loading Blown Lead model...")
      self$blown_lead_model <- load_model(blown_lead_model_path)

      self$adjustment_weight <- adjustment_weight
      self$max_fade_per_game <- max_fade_per_game
      self$max_wl_per_game <- max_wl_per_game
      self$min_minutes_between_fades <- min_minutes_between_fades
      self$fade_mode <- fade_mode

      # Initialize position tracking
      self$positions <- tibble(
        position_id = character(),
        game_id = character(),
        strategy = character(),
        timestamp = as.POSIXct(character()),
        entry_price = numeric(),
        bet_size = numeric(),
        lead = integer(),
        period = integer(),
        edge = numeric(),
        status = character()  # "open", "settled_win", "settled_loss"
      )

      self$game_positions <- list()
      self$open_fades <- list()  # Track open fade: list(fading_home = TRUE/FALSE)

      message("KalshiPredictor initialized successfully!")
      message(sprintf("  Fade mode: %s", self$fade_mode))
      message(sprintf("  Max fade positions per game: %d", self$max_fade_per_game))
      message(sprintf("  Max with-leader positions per game: %d", self$max_wl_per_game))
    },

    #' Get baseline win probability from WP model
    get_baseline_wp = function(game_state) {
      predict_wp(
        model = self$wp_model,
        home_margin = game_state$home_margin,
        seconds_remaining = game_state$seconds_remaining,
        period = game_state$period,
        home_rating_advantage = game_state$home_rating_advantage
      )
    },

    #' Get blown lead probability from blown lead model (v2)
    get_blown_lead_prob = function(game_state) {
      if (abs(game_state$home_margin) < 3) {
        return(0.5)
      }

      predict_blown_lead_v2(
        model = self$blown_lead_model,
        lead = abs(game_state$home_margin),
        minutes_elapsed = game_state$minutes_elapsed,
        period = game_state$period,
        is_home = game_state$home_margin > 0,
        net_rating_advantage = game_state$net_rating_advantage
      )
    },

    #' Calculate adjusted win probability combining both models
    #' Uses DIRECT method: blown_lead_prob = trailer's win probability
    #' This is well-calibrated with v2 model (0.3% error on historical data)
    get_adjusted_wp = function(game_state) {
      baseline_wp <- self$get_baseline_wp(game_state)
      blown_lead_prob <- self$get_blown_lead_prob(game_state)

      # DIRECT method: use blown_lead_prob directly as probability leader loses
      if (game_state$home_margin > 0) {
        # Home leading: blown_lead_prob = prob home loses
        adjusted_wp <- 1 - blown_lead_prob
      } else if (game_state$home_margin < 0) {
        # Away leading: blown_lead_prob = prob away loses = prob home wins
        adjusted_wp <- blown_lead_prob
      } else {
        adjusted_wp <- baseline_wp
      }

      adjusted_wp <- pmax(0.001, pmin(0.999, adjusted_wp))

      list(
        baseline_wp = baseline_wp,
        blown_lead_prob = blown_lead_prob,
        adjusted_wp = adjusted_wp,
        adjustment = adjusted_wp - baseline_wp
      )
    },

    #' Get current position count for a game
    get_position_count = function(game_id, strategy) {
      if (is.null(self$game_positions[[game_id]])) {
        return(0)
      }
      sum(self$game_positions[[game_id]]$strategy == strategy)
    },

    #' Get last fade entry time for a game
    get_last_fade_time = function(game_id) {
      if (is.null(self$game_positions[[game_id]])) {
        return(NULL)
      }
      fade_positions <- self$game_positions[[game_id]] %>%
        filter(strategy == "fade")
      if (nrow(fade_positions) == 0) {
        return(NULL)
      }
      max(fade_positions$timestamp)
    },

    #' Check if there's an open fade position for this game
    has_open_fade = function(game_id) {
      !is.null(self$open_fades[[game_id]])
    },

    #' Check if open fade should be closed based on game state
    #' Fade closes when: lead drops below threshold OR leader changes
    should_close_fade = function(game_id, game_state) {
      if (!self$has_open_fade(game_id)) {
        return(FALSE)
      }

      open_fade <- self$open_fades[[game_id]]
      lead <- abs(game_state$home_margin)
      home_leading <- game_state$home_margin > 0

      # Close if lead dropped below minimum threshold
      if (lead < FADE_MIN_LEAD) {
        return(TRUE)
      }

      # Close if the leader changed (the team we were fading is no longer leading)
      if (open_fade$fading_home != home_leading) {
        return(TRUE)
      }

      FALSE
    },

    #' Close the open fade position for a game
    close_fade = function(game_id) {
      self$open_fades[[game_id]] <- NULL
    },

    #' Open a new fade position (track who we're fading)
    open_fade = function(game_id, home_leading) {
      self$open_fades[[game_id]] <- list(fading_home = home_leading)
    },

    #' Check if WITH LEADER entry conditions are met
    check_with_leader_eligible = function(game_state) {
      lead <- abs(game_state$home_margin)
      period <- game_state$period

      (period >= 3 && lead >= WITH_LEADER_MIN_LEAD_Q3Q4) ||
      (lead >= WITH_LEADER_MIN_LEAD_ANY)
    },

    #' Check if FADE entry conditions are met
    #' Supports two modes:
    #'   - "market" (V2): Entry when market shows uncertainty (trailer >= 35c)
    #'   - "calibrated": Entry when calibrated model shows edge >= 5%
    #'
    #' V2 (market) is the default and recommended approach based on backtesting:
    #'   - V2: 15 trades, 53% win rate, +$32 P&L
    #'   - Calibrated: 29 trades, 28% win rate, +$25 P&L
    #'   - Original: 242 trades, 18% win rate, -$106 P&L
    #'
    #' @param game_state Current game state
    #' @param market_price Current market price for home team
    #' @param mode "market" or "calibrated"
    #' @param live_data Optional list with live game data for in-game adjustments:
    #'   - trailer_pts_last_5: Trailer's points in last 5 minutes
    #'   - leader_pts_last_5: Leader's points in last 5 minutes
    #'   - trailer_price_5min_ago: Trailer's market price 5 minutes ago
    check_fade_eligible = function(game_state, market_price, mode = "market", live_data = NULL, spread_data = NULL) {
      lead <- abs(game_state$home_margin)
      period <- game_state$period
      home_leading <- game_state$home_margin > 0

      # Basic filters
      if (lead < FADE_MIN_LEAD || period > 4) {
        return(list(
          eligible = FALSE,
          reason = if (lead < FADE_MIN_LEAD) "Lead too small" else "Overtime",
          trailer_price = NA,
          expected_win_rate = NA,
          calibrated_prob = NA,
          calibrated_edge = NA,
          ingame_adjustment = NULL
        ))
      }

      # Liquidity gates (only when real spread data with volume info is available)
      if (!is.null(spread_data) && is.list(spread_data)) {
        # Gate 1: Minimum depth at best ask — don't enter if <5 contracts available
        ask_size <- spread_data$yes_ask_size
        if (!is.null(ask_size) && !is.na(ask_size) && ask_size < 5) {
          return(list(
            eligible = FALSE,
            reason = sprintf("Insufficient liquidity (ask depth=%d contracts)", as.integer(ask_size)),
            trailer_price = NA,
            expected_win_rate = NA,
            calibrated_prob = NA,
            calibrated_edge = NA,
            ingame_adjustment = NULL
          ))
        }

        # Gate 2: Wide spread block — 10%+ spread means edge consumed by execution costs
        sp_pct <- spread_data$spread_pct
        if (!is.null(sp_pct) && !is.na(sp_pct) && sp_pct > 10) {
          return(list(
            eligible = FALSE,
            reason = sprintf("Spread too wide (%.1f%% > 10%%)", sp_pct),
            trailer_price = NA,
            expected_win_rate = NA,
            calibrated_prob = NA,
            calibrated_edge = NA,
            ingame_adjustment = NULL
          ))
        }
      }

      # Calculate trailer's market price
      trailer_price <- if (home_leading) (1 - market_price) else market_price

      # Get model prediction and calibrate it
      wp_result <- self$get_adjusted_wp(game_state)
      model_blown_prob <- if (home_leading) (1 - wp_result$adjusted_wp) else wp_result$adjusted_wp

      # Apply calibration (market-adaptive shrinkage)
      calibrated_prob <- calibrate_blown_prob(model_blown_prob, trailer_price, period)

      # ============================================================
      # IN-GAME ADJUSTMENTS (Run Differential + Price Velocity)
      # ============================================================
      ingame_adj <- list(adjustment = 1.0, reasons = "No live data", has_live_data = FALSE)

      if (!is.null(live_data)) {
        ingame_adj <- calc_ingame_adjustment(live_data, trailer_price)

        # Apply in-game adjustment to calibrated probability
        calibrated_prob <- calibrated_prob * ingame_adj$adjustment

        # Cap at reasonable bounds
        calibrated_prob <- min(calibrated_prob, 0.85)
      }

      # Calculate edges (with in-game adjusted probability)
      breakeven <- calc_breakeven_blown(trailer_price)
      calibrated_edge <- calibrated_prob - breakeven
      raw_edge <- model_blown_prob - breakeven

      # ============================================================
      # MODE: "market" (V2) - Market-based entry (RECOMMENDED)
      # ============================================================
      if (mode == "market") {
        # Check for sharp price spike - block entry if price spiking too fast
        if (BLOCK_ON_SHARP_SPIKE && !is.null(live_data$trailer_price_5min_ago)) {
          velocity <- (trailer_price - live_data$trailer_price_5min_ago) / 5
          if (velocity >= SHARP_SPIKE_THRESHOLD) {
            return(list(
              eligible = FALSE,
              reason = sprintf("BLOCKED: Sharp price spike (+%.1fc/min) - wait for pullback",
                              velocity * 100),
              trailer_price = trailer_price,
              expected_win_rate = NA,
              calibrated_prob = calibrated_prob,
              calibrated_edge = calibrated_edge,
              ingame_adjustment = ingame_adj,
              blocked_by_velocity = TRUE
            ))
          }
        }

        # Check if trailer price exceeds maximum (even with hedging)
        if (trailer_price > FADE_MAX_MARKET_PRICE) {
          return(list(
            eligible = FALSE,
            reason = sprintf("Trailer %.0fc exceeds max %.0fc",
                            trailer_price * 100, FADE_MAX_MARKET_PRICE * 100),
            trailer_price = trailer_price,
            expected_win_rate = NA,
            calibrated_prob = calibrated_prob,
            calibrated_edge = calibrated_edge,
            ingame_adjustment = ingame_adj,
            blocked_by_price_cap = TRUE
          ))
        }

        if (trailer_price >= FADE_MIN_MARKET_PRICE) {
          # Get bet size for this price level (40-45c requires Q3/Q4)
          bet_params <- get_bet_size_for_price(trailer_price, period)

          if (!bet_params$eligible) {
            return(list(
              eligible = FALSE,
              reason = bet_params$reason,
              trailer_price = trailer_price,
              expected_win_rate = NA,
              calibrated_prob = calibrated_prob,
              calibrated_edge = calibrated_edge,
              ingame_adjustment = ingame_adj
            ))
          }

          # Base expected win rate from historical analysis of 35-45c range
          base_expected_win_rate <- 0.48  # More conservative based on backtest

          # Adjust expected win rate by in-game factors
          adjusted_expected_win_rate <- min(base_expected_win_rate * ingame_adj$adjustment, 0.75)

          # Build reason string
          reason_str <- sprintf("V2: trailer %.0fc (%s tier)",
                               trailer_price * 100, bet_params$tier)
          if (ingame_adj$has_live_data && ingame_adj$adjustment != 1.0) {
            reason_str <- paste0(reason_str, " + ", ingame_adj$reasons)
          }

          return(list(
            eligible = TRUE,
            reason = reason_str,
            trailer_price = trailer_price,
            expected_win_rate = adjusted_expected_win_rate,
            base_expected_win_rate = base_expected_win_rate,
            calibrated_prob = calibrated_prob,
            calibrated_edge = calibrated_edge,
            raw_model_prob = model_blown_prob,
            raw_edge = raw_edge,
            ingame_adjustment = ingame_adj,
            model_info = wp_result,
            # Bet sizing (no hedge)
            recommended_bet_size = bet_params$bet_size,
            bet_tier = bet_params$tier,
            breakeven = bet_params$breakeven
          ))
        }

        return(list(
          eligible = FALSE,
          reason = sprintf("Market confident (trailer %.0fc < %.0fc)",
                          trailer_price * 100, FADE_MIN_MARKET_PRICE * 100),
          trailer_price = trailer_price,
          expected_win_rate = NA,
          calibrated_prob = calibrated_prob,
          calibrated_edge = calibrated_edge,
          ingame_adjustment = ingame_adj
        ))
      }

      # ============================================================
      # MODE: "calibrated" - Calibrated model edge entry
      # ============================================================
      if (mode == "calibrated") {
        min_calibrated_edge <- 0.05  # 5% calibrated edge threshold

        if (calibrated_edge >= min_calibrated_edge) {
          reason_str <- sprintf("Calibrated edge %.1f%% >= 5%%",
                               calibrated_edge * 100)
          if (ingame_adj$has_live_data && ingame_adj$adjustment != 1.0) {
            reason_str <- paste0(reason_str, " + ", ingame_adj$reasons)
          }

          return(list(
            eligible = TRUE,
            reason = reason_str,
            trailer_price = trailer_price,
            expected_win_rate = calibrated_prob,
            calibrated_prob = calibrated_prob,
            calibrated_edge = calibrated_edge,
            raw_model_prob = model_blown_prob,
            raw_edge = raw_edge,
            ingame_adjustment = ingame_adj,
            model_info = wp_result
          ))
        }

        return(list(
          eligible = FALSE,
          reason = sprintf("Calibrated edge %.1f%% < 5%% threshold",
                          calibrated_edge * 100),
          trailer_price = trailer_price,
          expected_win_rate = NA,
          calibrated_prob = calibrated_prob,
          calibrated_edge = calibrated_edge,
          ingame_adjustment = ingame_adj
        ))
      }

      # Unknown mode
      stop(sprintf("Unknown fade mode: %s. Use 'market' or 'calibrated'.", mode))
    },

    #' Calculate tiered bet size
    #' V2 Fade: Uses trailer market price and in-game factors for sizing
    #' @param game_state Game state
    #' @param strategy "with_leader" or "fade"
    #' @param trailer_price Trailer's market price (for fade)
    #' @param on_big_run TRUE if trailer is on 10+ pt scoring run
    #' @param ingame_adjustment Adjustment multiplier from in-game factors
    calculate_bet_tier = function(game_state, strategy, trailer_price = 0,
                                   on_big_run = FALSE, ingame_adjustment = 1.0,
                                   liquidity = NULL) {
      period <- game_state$period
      lead <- abs(game_state$home_margin)
      is_q4 <- period >= 4

      if (strategy == "with_leader") {
        if (is_q4 && lead >= 20) {
          return(list(tier = "TIER_1_MAX", bet_size = BET_SIZE_TIER1))
        }
        if ((is_q4 && lead >= 15) || lead >= 21) {
          return(list(tier = "TIER_2_LARGE", bet_size = BET_SIZE_TIER2))
        }
        return(list(tier = "TIER_3_MEDIUM", bet_size = BET_SIZE_TIER3))

      } else if (strategy == "fade") {
        # V2: Size based on market uncertainty + in-game factors
        base_tier <- "TIER_4_STANDARD"
        base_size <- BET_SIZE_TIER4

        # Higher trailer price = market more uncertain = better opportunity
        if (trailer_price >= 0.45) {
          base_tier <- "TIER_3_MEDIUM"
          base_size <- BET_SIZE_TIER3
        }

        # Boost bet size if trailer on big run AND price is favorable
        if (on_big_run && trailer_price >= 0.40) {
          # Upgrade one tier when momentum is clearly shifting
          if (base_tier == "TIER_4_STANDARD") {
            return(list(tier = "TIER_3_MEDIUM_MOMENTUM", bet_size = BET_SIZE_TIER3))
          } else if (base_tier == "TIER_3_MEDIUM") {
            return(list(tier = "TIER_2_LARGE_MOMENTUM", bet_size = BET_SIZE_TIER2))
          }
        }

        # Apply in-game adjustment to bet size (capped at TIER_2)
        if (ingame_adjustment >= 1.25) {
          # Strong in-game signal - increase bet size by 50%
          adjusted_size <- min(base_size * 1.5, BET_SIZE_TIER2)
          base_tier <- paste0(base_tier, "_BOOSTED")
          base_size <- adjusted_size
        }

        # Volume-adjusted sizing: reduce bet when orderbook depth is thin
        if (!is.null(liquidity) && is.list(liquidity)) {
          depth <- liquidity$total_contracts %||% NA
          if (!is.na(depth) && depth > 0) {
            # Contracts we'd want at this bet size (rough estimate: bet_amount / price)
            est_contracts <- max(1, round(base_size * 100))  # Approximate
            if (depth < est_contracts) {
              # Less depth than our order → minimum bet (1 contract equivalent)
              base_size <- 0.01  # ~1 contract
              base_tier <- paste0(base_tier, "_LIQ_MIN")
            } else if (depth < est_contracts * 2) {
              # Depth < 2x our order → reduce by 50%
              base_size <- base_size * 0.5
              base_tier <- paste0(base_tier, "_LIQ_REDUCED")
            }
            # depth >= 3x: keep original sizing (plenty of liquidity)
          }
        }

        return(list(tier = base_tier, bet_size = base_size))
      }

      list(tier = "NO_BET", bet_size = 0)
    },

    #' Generate trading signal based on game state and market price
    #' @param game_id Unique identifier for the game
    #' @param game_state List with all required game state variables
    #' @param market_price Current Kalshi market price for home team win (0-1)
    #' @param current_time Current timestamp (optional, for tracking)
    #' @param live_data Optional list with live game data for in-game adjustments:
    #'   - trailer_pts_last_5: Trailer's points in last 5 minutes
    #'   - leader_pts_last_5: Leader's points in last 5 minutes
    #'   - trailer_price_5min_ago: Trailer's market price 5 minutes ago
    get_signal = function(game_id, game_state, market_price, current_time = Sys.time(), live_data = NULL, spread_data = NULL) {
      if (market_price <= 0 || market_price >= 1) {
        stop("market_price must be between 0 and 1")
      }

      lead <- abs(game_state$home_margin)
      period <- game_state$period
      home_leading <- game_state$home_margin > 0

      # Don't signal in final minute
      if (game_state$seconds_remaining < MIN_SECONDS_FOR_SIGNAL && period >= 4) {
        return(list(
          with_leader = list(signal = "HOLD", reason = "Too close to end"),
          fade = list(signal = "HOLD", reason = "Too close to end"),
          game_id = game_id,
          game_state = game_state,
          market_price = market_price
        ))
      }

      # Skip overtime
      if (period > 4) {
        return(list(
          with_leader = list(signal = "HOLD", reason = "Overtime"),
          fade = list(signal = "HOLD", reason = "Overtime"),
          game_id = game_id,
          game_state = game_state,
          market_price = market_price
        ))
      }

      # Get model probabilities
      wp_result <- self$get_adjusted_wp(game_state)

      # ---------- WITH LEADER STRATEGY ----------
      wl_count <- self$get_position_count(game_id, "with_leader")
      wl_eligible <- self$check_with_leader_eligible(game_state)

      if (wl_eligible && lead > 0 && wl_count < self$max_wl_per_game) {
        leader_price <- if (home_leading) market_price else (1 - market_price)
        # Use logistic model instead of lookup table
        blown_rate <- wp_result$blown_lead_prob
        hold_rate <- 1 - blown_rate

        wl_bet_tier <- self$calculate_bet_tier(game_state, "with_leader")

        with_leader_signal <- list(
          signal = if (home_leading) "BUY_YES" else "BUY_NO",
          reason = sprintf("%s leading by %d in Q%d (hold rate: %.1f%%)",
                          if (home_leading) "Home" else "Away", lead, period, hold_rate * 100),
          entry_price = leader_price,
          hold_rate = hold_rate,
          bet_tier = wl_bet_tier$tier,
          bet_size = wl_bet_tier$bet_size,
          exit_strategy = "HOLD_TO_SETTLEMENT",
          position_number = wl_count + 1,
          max_positions = self$max_wl_per_game
        )
      } else if (wl_count >= self$max_wl_per_game) {
        with_leader_signal <- list(
          signal = "HOLD",
          reason = sprintf("Max positions reached (%d/%d)", wl_count, self$max_wl_per_game)
        )
      } else {
        with_leader_signal <- list(
          signal = "HOLD",
          reason = if (lead == 0) "Game tied" else sprintf("Lead %d pts below threshold", lead)
        )
      }

      # ---------- SELECTIVE FADE STRATEGY ----------
      # Check if open fade should be closed (lead dropped or leader changed)
      if (self$should_close_fade(game_id, game_state)) {
        self$close_fade(game_id)
      }

      fade_count <- self$get_position_count(game_id, "fade")
      fade_check <- self$check_fade_eligible(game_state, market_price, mode = self$fade_mode, live_data = live_data, spread_data = spread_data)

      # Check if we have an open fade (only one open fade at a time)
      has_open <- self$has_open_fade(game_id)

      # Check time since last fade
      last_fade_time <- self$get_last_fade_time(game_id)
      time_ok <- TRUE
      if (!is.null(last_fade_time)) {
        minutes_since <- as.numeric(difftime(current_time, last_fade_time, units = "mins"))
        time_ok <- minutes_since >= self$min_minutes_between_fades
      }

      if (fade_check$eligible && fade_count < self$max_fade_per_game && time_ok && !has_open) {
        # Get in-game adjustment info for bet sizing
        on_big_run <- isTRUE(fade_check$on_big_run)
        ingame_adj_value <- fade_check$ingame_adjustment$adjustment %||% 1.0

        fade_bet_tier <- self$calculate_bet_tier(
          game_state, "fade", fade_check$trailer_price,
          on_big_run = on_big_run,
          ingame_adjustment = ingame_adj_value
        )

        # Build reason string based on mode and in-game adjustments
        mode_label <- if (self$fade_mode == "market") "V2" else "Cal"
        reason_base <- sprintf("Fade %s: %s (+%d pts, trailer %.0fc, pos %d/%d)",
                              mode_label, if (home_leading) "home" else "away", lead,
                              fade_check$trailer_price * 100, fade_count + 1, self$max_fade_per_game)

        # Add in-game adjustment info if present
        if (!is.null(fade_check$ingame_adjustment) && fade_check$ingame_adjustment$has_live_data) {
          adj <- fade_check$ingame_adjustment
          if (adj$adjustment != 1.0) {
            reason_base <- paste0(reason_base, " [", adj$reasons, "]")
          }
        }

        # Use hedge parameters for bet size if available
        final_bet_size <- if (!is.null(fade_check$recommended_bet_size)) {
          fade_check$recommended_bet_size
        } else {
          fade_bet_tier$bet_size
        }

        fade_signal <- list(
          signal = if (home_leading) "BUY_NO" else "BUY_YES",
          reason = reason_base,
          entry_price = fade_check$trailer_price,
          expected_win_rate = fade_check$expected_win_rate,
          calibrated_prob = fade_check$calibrated_prob,
          calibrated_edge = fade_check$calibrated_edge,
          ingame_adjustment = fade_check$ingame_adjustment,
          bet_tier = fade_check$bet_tier %||% fade_bet_tier$tier,
          bet_size = final_bet_size,
          exit_strategy = "HOLD_TO_SETTLEMENT",
          position_number = fade_count + 1,
          max_positions = self$max_fade_per_game,
          breakeven = fade_check$breakeven
        )
      } else if (has_open) {
        fade_signal <- list(
          signal = "HOLD",
          reason = "Open fade position exists (wait for lead to drop or change)"
        )
      } else if (fade_count >= self$max_fade_per_game) {
        fade_signal <- list(
          signal = "HOLD",
          reason = sprintf("Max positions reached (%d/%d)", fade_count, self$max_fade_per_game)
        )
      } else if (!time_ok) {
        fade_signal <- list(
          signal = "HOLD",
          reason = sprintf("Too soon since last fade (wait %d mins)", self$min_minutes_between_fades)
        )
      } else {
        fade_signal <- list(
          signal = "HOLD",
          reason = if (!is.null(fade_check$reason)) {
            fade_check$reason
          } else if (lead >= FADE_MIN_LEAD) {
            sprintf("Market confident in leader (trailer < %.0fc)", FADE_MIN_MARKET_PRICE * 100)
          } else {
            sprintf("Lead %d pts below %d minimum", lead, FADE_MIN_LEAD)
          }
        )
      }

      list(
        with_leader = with_leader_signal,
        fade = fade_signal,
        model = list(
          baseline_wp = wp_result$baseline_wp,
          adjusted_wp = wp_result$adjusted_wp,
          blown_lead_prob = wp_result$blown_lead_prob
        ),
        game_id = game_id,
        game_state = game_state,
        market_price = market_price,
        positions_this_game = list(
          with_leader = wl_count,
          fade = fade_count
        )
      )
    },

    #' Record a position after execution
    #' @param game_id Game identifier
    #' @param strategy "with_leader" or "fade"
    #' @param signal_details Signal details from get_signal
    #' @param timestamp Time of execution
    record_position = function(game_id, strategy, signal_details, timestamp = Sys.time()) {
      position_id <- paste0(game_id, "_", strategy, "_",
                           self$get_position_count(game_id, strategy) + 1)

      new_position <- tibble(
        position_id = position_id,
        game_id = game_id,
        strategy = strategy,
        timestamp = timestamp,
        entry_price = signal_details$entry_price,
        bet_size = signal_details$bet_size,
        lead = abs(signal_details$lead %||% 0),
        period = signal_details$period %||% 0,
        edge = signal_details$edge %||% 0,
        status = "open"
      )

      self$positions <- bind_rows(self$positions, new_position)

      # Update game positions tracker
      if (is.null(self$game_positions[[game_id]])) {
        self$game_positions[[game_id]] <- new_position
      } else {
        self$game_positions[[game_id]] <- bind_rows(
          self$game_positions[[game_id]],
          new_position
        )
      }

      # Track open fade position (BUY_NO = fading home, BUY_YES = fading away)
      if (strategy == "fade") {
        fading_home <- signal_details$signal == "BUY_NO"
        self$open_fade(game_id, fading_home)
      }

      message(sprintf("Recorded %s position %s: entry=%.3f, size=%.1f%%",
                      strategy, position_id, signal_details$entry_price,
                      signal_details$bet_size * 100))

      invisible(position_id)
    },

    #' Settle a position
    #' @param position_id Position to settle
    #' @param won Whether the position won
    settle_position = function(position_id, won) {
      idx <- which(self$positions$position_id == position_id)
      if (length(idx) == 0) {
        warning(sprintf("Position %s not found", position_id))
        return(invisible(NULL))
      }

      self$positions$status[idx] <- if (won) "settled_win" else "settled_loss"

      message(sprintf("Settled %s: %s", position_id, if (won) "WIN" else "LOSS"))
      invisible(TRUE)
    },

    #' Settle all positions for a game
    #' @param game_id Game to settle
    #' @param home_won Whether home team won
    settle_game = function(game_id, home_won) {
      game_pos <- self$positions %>% filter(game_id == !!game_id, status == "open")

      if (nrow(game_pos) == 0) {
        message(sprintf("No open positions for game %s", game_id))
        return(invisible(NULL))
      }

      for (i in 1:nrow(game_pos)) {
        pos <- game_pos[i, ]
        # Determine if position won based on strategy and home_won
        # with_leader: wins if leader held (need to know who was leading)
        # fade: wins if lead was blown
        # For now, this is a simplified settlement - in practice you'd track
        # which team the position was on
        self$settle_position(pos$position_id, won = TRUE)  # Placeholder
      }

      message(sprintf("Settled %d positions for game %s", nrow(game_pos), game_id))
    },

    #' Get summary of all positions
    get_position_summary = function() {
      if (nrow(self$positions) == 0) {
        message("No positions recorded")
        return(NULL)
      }

      self$positions %>%
        group_by(strategy, status) %>%
        summarise(
          n = n(),
          total_bet = sum(bet_size),
          avg_entry_price = mean(entry_price),
          avg_edge = mean(edge),
          .groups = "drop"
        )
    },

    #' Get positions for a specific game
    get_game_positions = function(game_id) {
      self$positions %>% filter(game_id == !!game_id)
    },

    #' Reset all positions (for new session)
    reset_positions = function() {
      self$positions <- tibble(
        position_id = character(),
        game_id = character(),
        strategy = character(),
        timestamp = as.POSIXct(character()),
        entry_price = numeric(),
        bet_size = numeric(),
        lead = integer(),
        period = integer(),
        edge = numeric(),
        status = character()
      )
      self$game_positions <- list()
      self$open_fades <- list()
      message("Positions reset")
    },

    #' Print summary of predictor configuration
    print_config = function() {
      cat("KalshiPredictor Configuration (Automated Trading Mode)\n")
      cat("======================================================\n\n")

      cat("STRATEGY 1: WITH LEADER (Bet on leading team)\n")
      cat(sprintf("  Entry: Q3/Q4 at %d+ pts OR any period at %d+ pts\n",
                  WITH_LEADER_MIN_LEAD_Q3Q4, WITH_LEADER_MIN_LEAD_ANY))
      cat("  Exit: Hold to settlement (NO stop-loss)\n")
      cat(sprintf("  Max positions per game: %d\n", self$max_wl_per_game))
      cat("  Backtest: $31 P&L, 0.48 Sharpe, 95.4% win rate\n\n")

      cat(sprintf("STRATEGY 2: FADE (Mode: %s)\n", toupper(self$fade_mode)))

      if (self$fade_mode == "market") {
        cat(sprintf("  Entry: %d+ pt lead AND trailer price %.0fc-%.0fc\n",
                    FADE_MIN_LEAD, FADE_MIN_MARKET_PRICE * 100, FADE_MAX_MARKET_PRICE * 100))
        cat("  Logic: Bet WITH market uncertainty, not against it\n")
        cat("  NO HEDGING: Backtests showed hedging destroys value\n\n")

        cat("  BET SIZE TIERS:\n")
        cat("  +------------+----------+-------------+\n")
        cat("  | Price Tier | Bet Size | Breakeven   |\n")
        cat("  +------------+----------+-------------+\n")
        cat(sprintf("  | 35-40c     | %.1f%%     | ~38-43%%     |\n", BET_SIZE_35_40 * 100))
        cat(sprintf("  | 40-45c     | %.1f%%     | ~43-48%%     |\n", BET_SIZE_40_45 * 100))
        cat("  +------------+----------+-------------+\n\n")

        cat("  Why cap at 45c (no hedging):\n")
        cat("    - Backtest 35-45c: +$484 profit\n")
        cat("    - Backtest 45c+: -$1,078 loss (even with hedging)\n")
        cat("    - Hedging doesn't improve EV, only reduces variance\n")
        cat("    - Simple approach: trade favorable prices, skip unfavorable\n")
      } else if (self$fade_mode == "calibrated") {
        cat(sprintf("  Entry: %d+ pt lead AND calibrated edge >= 5%%\n", FADE_MIN_LEAD))
        cat("  Logic: Use market-calibrated blown lead probability\n")
        cat("  Calibration: Shrink model prediction toward market price\n")
        cat("  Backtest: 29 trades, 28% win rate, +$25 P&L\n")
      }

      cat("  Exit: Hold to settlement\n")
      cat(sprintf("  Max positions per game: %d\n", self$max_fade_per_game))
      cat(sprintf("  Min time between entries: %d minutes\n\n", self$min_minutes_between_fades))

      cat("FADE MODE COMPARISON:\n")
      cat("  Original (model edge >= 5%): 242 trades, 18% win, -$106 P&L\n")
      cat("  Calibrated (cal edge >= 5%):  29 trades, 28% win, +$25 P&L\n")
      cat("  Market V2 (trailer >= 35c):   15 trades, 53% win, +$32 P&L  <-- DEFAULT\n\n")

      cat("MODEL CALIBRATION:\n")
      cat("  Original model: 11.3% overconfident (predicted 29%, actual 18%)\n")
      cat("  Calibration formula: alpha * model + (1-alpha) * market\n")
      cat("    - market < 18c:  alpha = 0.3 (trust market more)\n")
      cat("    - market 18-35c: alpha = 0.5 (balanced)\n")
      cat("    - market >= 35c: alpha = 0.8 (trust model more)\n")
      cat("    - Q4: multiply by 0.1 (leads rarely blown)\n\n")

      cat("IN-GAME ADJUSTMENTS (EMPIRICALLY VALIDATED):\n")
      cat("  Price Velocity (CONTRARIAN - empirically validated):\n")
      cat(sprintf("    - Threshold: %.1fc/min (determined by grid search)\n",
                  PRICE_VELOCITY_THRESHOLD * 100))
      cat(sprintf("    - Price DROP: +%.0f%% boost (buy the dip)\n",
                  (PRICE_VELOCITY_BOOST - 1) * 100))
      cat(sprintf("    - Price SPIKE: -%.0f%% penalty (likely reversal)\n",
                  (1 - PRICE_VELOCITY_PENALTY) * 100))
      cat(sprintf("    - SHARP SPIKE (>%.0fc/min): BLOCKS ENTRY\n",
                  SHARP_SPIKE_THRESHOLD * 100))
      cat("    * Empirical finding: dips show 50% higher win rate than spikes\n")
      cat("  Run Differential: DISABLED (no predictive signal found)\n")
      cat(sprintf("  Max adjustment: +/-%.0f%%\n\n", (MAX_INGAME_ADJUSTMENT - 1) * 100))

      cat("BET SIZING:\n")
      cat(sprintf("  TIER 1 (MAX):      %.0f%% - Q4 + 20+ pts (with_leader)\n", BET_SIZE_TIER1 * 100))
      cat(sprintf("  TIER 2 (LARGE):    %.0f%% - Q4 + 15+ pts OR 21+ pts (with_leader)\n", BET_SIZE_TIER2 * 100))
      cat(sprintf("  TIER 3 (MEDIUM):   %.0f%% - trailer >= 45c (fade) OR standard (with_leader)\n", BET_SIZE_TIER3 * 100))
      cat(sprintf("  TIER 4 (STANDARD): %.0f%% - trailer 35-45c (fade)\n\n", BET_SIZE_TIER4 * 100))

      cat("POSITION TRACKING:\n")
      cat(sprintf("  Total positions: %d\n", nrow(self$positions)))
      cat(sprintf("  Open positions: %d\n", sum(self$positions$status == "open")))
      cat(sprintf("  Games tracked: %d\n", length(self$game_positions)))
    }
  )
)

# ------------------------------------------------------------------------------
# Helper function to create game state
# ------------------------------------------------------------------------------

create_game_state <- function(home_score,
                               away_score,
                               seconds_remaining,
                               period,
                               home_net_rating = 0,
                               away_net_rating = 0,
                               home_rest_days = 1,
                               away_rest_days = 1) {

  home_margin <- home_score - away_score
  minutes_elapsed <- (2880 - seconds_remaining) / 60

  if (home_margin > 0) {
    rest_advantage <- home_rest_days - away_rest_days
    net_rating_advantage <- home_net_rating - away_net_rating
  } else {
    rest_advantage <- away_rest_days - home_rest_days
    net_rating_advantage <- away_net_rating - home_net_rating
  }

  list(
    home_score = home_score,
    away_score = away_score,
    home_margin = home_margin,
    seconds_remaining = seconds_remaining,
    minutes_elapsed = minutes_elapsed,
    period = period,
    home_net_rating = home_net_rating,
    away_net_rating = away_net_rating,
    home_rating_advantage = home_net_rating - away_net_rating,
    home_rest_days = home_rest_days,
    away_rest_days = away_rest_days,
    rest_advantage = rest_advantage,
    net_rating_advantage = net_rating_advantage
  )
}

# ------------------------------------------------------------------------------
# Example Usage - Automated Trading Loop
# ------------------------------------------------------------------------------

# # Initialize predictor with custom caps
# predictor <- KalshiPredictor$new(
#   max_fade_per_game = 10,      # Take up to 10 fade positions per game
#   max_wl_per_game = 1,         # Only 1 with-leader position
#   min_minutes_between_fades = 2 # Wait 2 mins between fade entries
# )
# predictor$print_config()
#
# # Simulated game loop
# game_id <- "401584001"
#
# # Price update 1: Home up 12 in Q3
# game_state <- create_game_state(75, 63, 720, 3)
# signal <- predictor$get_signal(game_id, game_state, market_price = 0.88)
#
# if (signal$fade$signal != "HOLD") {
#   # Execute the trade via Kalshi API...
#   predictor$record_position(game_id, "fade", signal$fade)
# }
#
# # Price update 2: Lead grows to 15
# game_state <- create_game_state(80, 65, 600, 3)
# signal <- predictor$get_signal(game_id, game_state, market_price = 0.92)
#
# # Check with_leader signal
# if (signal$with_leader$signal != "HOLD") {
#   predictor$record_position(game_id, "with_leader", signal$with_leader)
# }
#
# # Check fade signal (might have +EV opportunity)
# if (signal$fade$signal != "HOLD") {
#   predictor$record_position(game_id, "fade", signal$fade)
# }
#
# # At game end
# predictor$settle_game(game_id, home_won = TRUE)
#
# # View summary
# predictor$get_position_summary()
