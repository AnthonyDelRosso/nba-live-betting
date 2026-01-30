# Kalshi NBA Live Betting Predictor
# Combines Win Probability Model + Blown Lead Model for edge detection
#
# Usage:
#   source("kalshi_predictor.R")
#   predictor <- KalshiPredictor$new()
#   signal <- predictor$get_signal(game_state, market_price)

library(tidyverse)
library(R6)

# Source the underlying models
source("win_probability_model.R")
source("nba_largest_lead.R")

# ------------------------------------------------------------------------------
# Constants
# ------------------------------------------------------------------------------

EDGE_THRESHOLD <- 0.03        # Minimum edge to signal (3%)
STRONG_EDGE_THRESHOLD <- 0.05 # Strong signal threshold (5%) - recalibrated
HIGH_EDGE_THRESHOLD <- 0.07   # High edge for tier upgrades (7%) - recalibrated
MAX_KELLY_FRACTION <- 0.25    # Cap Kelly at 25% of bankroll
MIN_SECONDS_FOR_SIGNAL <- 60  # Don't signal in final minute (too volatile)
MIN_PERIOD_FOR_SIGNAL <- 3    # Skip Q1 & Q2 (real Kalshi data shows Q2 loses money)
MIN_LEAD_FOR_SIGNAL <- 15     # Only trade when lead >= 15 points (validated by backtest)
BUY_YES_ONLY <- FALSE         # Allow both BUY_YES and BUY_NO signals

# Tiered bet sizing - RECALIBRATED based on score-matched backtest (100% WR across all)
# Tiers now based primarily on quarter + lead size, with edge as secondary factor
BET_SIZE_TIER1 <- 0.05        # MAX: Q4 + 20+ pts lead (safest situation)
BET_SIZE_TIER2 <- 0.04        # LARGE: Q4 + 15+ pts OR any 20+ pts lead
BET_SIZE_TIER3 <- 0.03        # MEDIUM: 5%+ edge (was 6%)
BET_SIZE_TIER4 <- 0.02        # SMALL: All other signals (was 1%)

# ------------------------------------------------------------------------------
# KalshiPredictor R6 Class
# ------------------------------------------------------------------------------

KalshiPredictor <- R6::R6Class(

"KalshiPredictor",

  public = list(
    wp_model = NULL,
    blown_lead_model = NULL,
    adjustment_weight = NULL,

    #' Initialize the predictor with both models
    #' @param wp_model_path Path to win probability model RDS
    #' @param blown_lead_model_path Path to blown lead model RDS
    #' @param adjustment_weight Weight for blown lead adjustment (0-1)
    initialize = function(wp_model_path = "win_probability_model.rds",
                          blown_lead_model_path = "blown_lead_model.rds",
                          adjustment_weight = 0.5) {

      message("Loading Win Probability model...")
      self$wp_model <- load_wp_model(wp_model_path)

      message("Loading Blown Lead model...")
      self$blown_lead_model <- load_model(blown_lead_model_path)

      self$adjustment_weight <- adjustment_weight

      message("KalshiPredictor initialized successfully!")
    },

    #' Get baseline win probability from WP model
    #' @param game_state List with: home_margin, seconds_remaining, period, home_rating_advantage
    #' @return Probability that home team wins (0-1)
    get_baseline_wp = function(game_state) {
      predict_wp(
        model = self$wp_model,
        home_margin = game_state$home_margin,
        seconds_remaining = game_state$seconds_remaining,
        period = game_state$period,
        home_rating_advantage = game_state$home_rating_advantage
      )
    },

    #' Get blown lead probability from blown lead model
    #' @param game_state List with: largest_lead, minutes_elapsed, period, is_home_leading,
    #'                   rest_advantage, net_rating_advantage
    #' @return Probability of blowing the lead (0-1)
    get_blown_lead_prob = function(game_state) {
      # Only calculate if there's a meaningful lead
      if (abs(game_state$home_margin) < 3) {
        return(0.5)  # No significant lead, neutral
      }

      predict_blown_lead(
        model = self$blown_lead_model,
        largest_lead = abs(game_state$home_margin),
        minutes_elapsed = game_state$minutes_elapsed,
        period = game_state$period,
        is_home = game_state$home_margin > 0,  # Is home team leading?
        rest_advantage = game_state$rest_advantage,
        net_rating_advantage = game_state$net_rating_advantage
      )
    },

    #' Calculate adjusted win probability combining both models
    #' @param game_state Full game state list
    #' @return List with baseline_wp, blown_lead_prob, adjusted_wp
    get_adjusted_wp = function(game_state) {
      # Get baseline from WP model
      baseline_wp <- self$get_baseline_wp(game_state)

      # Get blown lead probability
      blown_lead_prob <- self$get_blown_lead_prob(game_state)

      # Calculate adjustment based on who's leading
      if (game_state$home_margin > 0) {
        # Home team leading - blown lead hurts home win probability
        # Adjustment: reduce home WP proportionally to blown lead risk
        adjustment <- blown_lead_prob * self$adjustment_weight * (baseline_wp - 0.5)
        adjusted_wp <- baseline_wp - adjustment
      } else if (game_state$home_margin < 0) {
        # Away team leading - blown lead helps home win probability
        # Adjustment: increase home WP proportionally to away team's blown lead risk
        adjustment <- blown_lead_prob * self$adjustment_weight * (0.5 - baseline_wp)
        adjusted_wp <- baseline_wp + adjustment
      } else {
        # Tied game - no adjustment
        adjusted_wp <- baseline_wp
      }

      # Clamp to valid probability range
      adjusted_wp <- pmax(0.001, pmin(0.999, adjusted_wp))

      list(
        baseline_wp = baseline_wp,
        blown_lead_prob = blown_lead_prob,
        adjusted_wp = adjusted_wp,
        adjustment = adjusted_wp - baseline_wp
      )
    },

    #' Calculate edge vs market price
    #' @param model_prob Model's probability (0-1)
    #' @param market_price Kalshi market price (0-1)
    #' @return Edge (positive = model thinks underpriced)
    calculate_edge = function(model_prob, market_price) {
      model_prob - market_price
    },

    #' Calculate Kelly criterion bet size
    #' @param edge Edge vs market (can be positive or negative)
    #' @param market_price Current market price
    #' @return Recommended fraction of bankroll to bet (capped)
    calculate_kelly = function(edge, market_price) {
      if (abs(edge) < EDGE_THRESHOLD) {
        return(0)
      }

      # Kelly formula: f* = (bp - q) / b
      # where b = odds, p = prob of winning, q = 1-p
      # For binary markets: f* = edge / (1 - market_price) for YES
      #                     f* = -edge / market_price for NO

      if (edge > 0) {
        # Betting YES (home team)
        kelly <- edge / (1 - market_price)
      } else {
        # Betting NO (away team)
        kelly <- -edge / market_price
      }

      # Apply fractional Kelly (half Kelly is common) and cap
      kelly_fraction <- kelly * 0.5
      kelly_fraction <- pmin(kelly_fraction, MAX_KELLY_FRACTION)
      kelly_fraction <- pmax(kelly_fraction, 0)

      kelly_fraction
    },

    #' Calculate tiered bet size based on signal quality
    #' @param game_state Game state with period, home_margin
    #' @param edge Calculated edge vs market (positive for BUY_YES, negative for BUY_NO)
    #' @return List with tier name and bet size fraction
    calculate_bet_tier = function(game_state, edge) {
      period <- game_state$period
      lead <- abs(game_state$home_margin)
      is_q4 <- period >= 4
      # Use absolute edge for tier calculation (works for both BUY_YES and BUY_NO)
      abs_edge <- abs(edge)
      high_edge <- abs_edge >= HIGH_EDGE_THRESHOLD  # 7%+
      strong_edge <- abs_edge >= STRONG_EDGE_THRESHOLD  # 5%+

      # TIER 1: Q4 + 20+ pt lead (safest - game is essentially over)
      if (is_q4 && lead >= 20) {
        return(list(tier = "TIER_1_MAX", bet_size = BET_SIZE_TIER1, win_rate_hist = "100%"))
      }

      # TIER 2: Q4 + 15+ pt lead OR any quarter with 20+ pt lead
      if ((is_q4 && lead >= 15) || lead >= 20) {
        return(list(tier = "TIER_2_LARGE", bet_size = BET_SIZE_TIER2, win_rate_hist = "100%"))
      }

      # TIER 3: 5%+ edge (strong model confidence)
      if (strong_edge) {
        return(list(tier = "TIER_3_MEDIUM", bet_size = BET_SIZE_TIER3, win_rate_hist = "100%"))
      }

      # TIER 4: All other qualifying signals (3%+ edge, 15+ pt lead in Q3+)
      list(tier = "TIER_4_STANDARD", bet_size = BET_SIZE_TIER4, win_rate_hist = "100%")
    },

    #' Generate trading signal based on game state and market price
    #' @param game_state List with all required game state variables
    #' @param market_price Current Kalshi market price for home team win (0-1)
    #' @return List with signal details
    get_signal = function(game_state, market_price) {
      # Validate inputs
      if (market_price <= 0 || market_price >= 1) {
        stop("market_price must be between 0 and 1")
      }

      # Don't signal in final minute (too volatile/uncertain)
      if (game_state$seconds_remaining < MIN_SECONDS_FOR_SIGNAL &&
          game_state$period >= 4) {
        return(list(
          signal = "HOLD",
          signal_desc = "Too close to end of game",
          confidence = "NONE",
          bet_tier = "NO_BET",
          bet_size = 0,
          bet_size_pct = "0%",
          expected_win_rate = "N/A",
          edge = NA_real_,
          edge_pct = "NA",
          kelly_fraction = 0,
          kelly_pct = "0%",
          model_prob = NA_real_,
          market_price = market_price,
          baseline_wp = NA_real_,
          blown_lead_prob = NA_real_,
          adjustment = NA_real_,
          game_state = game_state
        ))
      }

      # Skip Q1 & Q2 - insufficient game data for reliable blown lead signals
      if (game_state$period < MIN_PERIOD_FOR_SIGNAL) {
        return(list(
          signal = "HOLD",
          signal_desc = sprintf("Q%d - insufficient game data", game_state$period),
          confidence = "NONE",
          bet_tier = "NO_BET",
          bet_size = 0,
          bet_size_pct = "0%",
          expected_win_rate = "N/A",
          edge = NA_real_,
          edge_pct = "NA",
          kelly_fraction = 0,
          kelly_pct = "0%",
          model_prob = NA_real_,
          market_price = market_price,
          baseline_wp = NA_real_,
          blown_lead_prob = NA_real_,
          adjustment = NA_real_,
          game_state = game_state
        ))
      }

      # Skip when lead is too small (backtest shows only 15+ point leads profitable)
      if (abs(game_state$home_margin) < MIN_LEAD_FOR_SIGNAL) {
        return(list(
          signal = "HOLD",
          signal_desc = sprintf("Lead too small (%d pts < %d min)",
                                abs(game_state$home_margin), MIN_LEAD_FOR_SIGNAL),
          confidence = "NONE",
          bet_tier = "NO_BET",
          bet_size = 0,
          bet_size_pct = "0%",
          expected_win_rate = "N/A",
          edge = NA_real_,
          edge_pct = "NA",
          kelly_fraction = 0,
          kelly_pct = "0%",
          model_prob = NA_real_,
          market_price = market_price,
          baseline_wp = NA_real_,
          blown_lead_prob = NA_real_,
          adjustment = NA_real_,
          game_state = game_state
        ))
      }

      # Get adjusted win probability
      wp_result <- self$get_adjusted_wp(game_state)

      # Calculate edge
      edge <- self$calculate_edge(wp_result$adjusted_wp, market_price)

      # Calculate Kelly sizing
      kelly_fraction <- self$calculate_kelly(edge, market_price)

      # Determine signal based on who is leading AND model confidence
      # Key insight: We want to bet WITH the leading team when model is confident
      # The signal should align with the team that has the lead

      if (abs(edge) >= STRONG_EDGE_THRESHOLD) {
        confidence <- "HIGH"
      } else if (abs(edge) >= EDGE_THRESHOLD) {
        confidence <- "MEDIUM"
      } else {
        confidence <- "LOW"
      }

      # Signal logic based on lead direction:
      # - When home leads (home_margin > 0): BUY_YES if model is confident home wins
      # - When away leads (home_margin < 0): BUY_NO if model is confident away wins

      if (game_state$home_margin > 0) {
        # Home is leading - consider BUY_YES (bet on home to hold lead)
        # Only signal if model probability is high enough (confirms the lead)
        if (wp_result$adjusted_wp >= 0.80 && abs(edge) >= EDGE_THRESHOLD) {
          signal <- "BUY_YES"
          signal_desc <- "Buy YES (home team leading, will hold)"
          # Adjust edge to be positive for proper tier calculation
          edge <- abs(edge)
        } else {
          signal <- "HOLD"
          signal_desc <- "Home leading but model not confident enough"
        }
      } else if (game_state$home_margin < 0) {
        # Away is leading - consider BUY_NO (bet on away to hold lead = home loses)
        if (BUY_YES_ONLY) {
          signal <- "HOLD"
          signal_desc <- "BUY_NO filtered (away leading)"
          kelly_fraction <- 0
        } else {
          # Only signal if model probability for away is high (home prob < 20%)
          if (wp_result$adjusted_wp <= 0.20 && abs(edge) >= EDGE_THRESHOLD) {
            signal <- "BUY_NO"
            signal_desc <- "Buy NO (away team leading, will hold)"
            # Keep edge negative for proper tier calculation
            edge <- -abs(edge)
          } else {
            signal <- "HOLD"
            signal_desc <- "Away leading but model not confident enough"
          }
        }
      } else {
        signal <- "HOLD"
        signal_desc <- "Game is tied - no signal"
      }

      # Calculate tiered bet sizing (for both BUY_YES and BUY_NO)
      if (signal %in% c("BUY_YES", "BUY_NO")) {
        bet_tier <- self$calculate_bet_tier(game_state, edge)
      } else {
        bet_tier <- list(tier = "NO_BET", bet_size = 0, win_rate_hist = "N/A")
      }

      list(
        signal = signal,
        signal_desc = signal_desc,
        confidence = confidence,
        bet_tier = bet_tier$tier,
        bet_size = bet_tier$bet_size,
        bet_size_pct = sprintf("%.1f%%", bet_tier$bet_size * 100),
        expected_win_rate = bet_tier$win_rate_hist,
        edge = edge,
        edge_pct = sprintf("%.1f%%", edge * 100),
        kelly_fraction = kelly_fraction,
        kelly_pct = sprintf("%.1f%%", kelly_fraction * 100),
        model_prob = wp_result$adjusted_wp,
        market_price = market_price,
        baseline_wp = wp_result$baseline_wp,
        blown_lead_prob = wp_result$blown_lead_prob,
        adjustment = wp_result$adjustment,
        game_state = game_state
      )
    },

    #' Analyze multiple scenarios for a given game
    #' @param game_state Base game state
    #' @param market_prices Vector of market prices to analyze
    #' @return Data frame with signals for each price
    analyze_price_sensitivity = function(game_state, market_prices = seq(0.1, 0.9, 0.05)) {
      results <- map_dfr(market_prices, function(price) {
        signal <- self$get_signal(game_state, price)
        tibble(
          market_price = price,
          model_prob = signal$model_prob,
          edge = signal$edge,
          signal = signal$signal,
          confidence = signal$confidence,
          kelly_fraction = signal$kelly_fraction
        )
      })

      results
    },

    #' Print summary of predictor configuration
    print_config = function() {
      cat("KalshiPredictor Configuration\n")
      cat("=============================\n\n")
      cat("Signal Logic:\n")
      cat("  BUY_YES: Home leading by 15+ pts + model prob >= 80%\n")
      cat("  BUY_NO:  Away leading by 15+ pts + model prob <= 20%\n\n")
      cat("Signal Filters:\n")
      cat(sprintf("  Edge Threshold: %.1f%%\n", EDGE_THRESHOLD * 100))
      cat(sprintf("  Strong Edge Threshold: %.1f%%\n", STRONG_EDGE_THRESHOLD * 100))
      cat(sprintf("  High Edge Threshold: %.1f%%\n", HIGH_EDGE_THRESHOLD * 100))
      cat(sprintf("  Min Period: Q%d+\n", MIN_PERIOD_FOR_SIGNAL))
      cat(sprintf("  Min Lead: %d pts\n", MIN_LEAD_FOR_SIGNAL))
      cat(sprintf("  BUY_YES Only: %s\n", ifelse(BUY_YES_ONLY, "YES", "NO")))
      cat(sprintf("  Min Seconds: %d\n\n", MIN_SECONDS_FOR_SIGNAL))
      cat("Tiered Bet Sizing (recalibrated for score-matched data):\n")
      cat(sprintf("  TIER 1 (MAX):      %.0f%% - Q4 + 20+ pts lead\n", BET_SIZE_TIER1 * 100))
      cat(sprintf("  TIER 2 (LARGE):    %.0f%% - Q4 + 15+ pts OR any 20+ pts\n", BET_SIZE_TIER2 * 100))
      cat(sprintf("  TIER 3 (MEDIUM):   %.0f%% - 5%%+ edge\n", BET_SIZE_TIER3 * 100))
      cat(sprintf("  TIER 4 (STANDARD): %.0f%% - All other signals\n\n", BET_SIZE_TIER4 * 100))
      cat(sprintf("Model Adjustment Weight: %.2f\n", self$adjustment_weight))
    }
  )
)

# ------------------------------------------------------------------------------
# Helper function to create game state
# ------------------------------------------------------------------------------

#' Create a game state object with all required fields
#' @param home_score Current home team score
#' @param away_score Current away team score
#' @param seconds_remaining Seconds remaining in regulation (negative in OT)
#' @param period Current period (1-4 regulation, 5+ OT)
#' @param home_net_rating Home team's net rating (optional, default 0)
#' @param away_net_rating Away team's net rating (optional, default 0)
#' @param home_rest_days Home team days of rest (optional, default 1)
#' @param away_rest_days Away team days of rest (optional, default 1)
#' @return List with all game state variables
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

  # Determine who's leading and their advantages
  if (home_margin > 0) {
    # Home team leading
    rest_advantage <- home_rest_days - away_rest_days
    net_rating_advantage <- home_net_rating - away_net_rating
  } else {
    # Away team leading (or tied)
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
# Batch prediction for backtesting
# ------------------------------------------------------------------------------

#' Run predictions on a dataframe of game states
#' @param predictor KalshiPredictor object
#' @param game_states_df Data frame with columns matching create_game_state params
#' @param market_prices Vector of market prices (same length as game_states_df rows)
#' @return Data frame with predictions appended
batch_predict <- function(predictor, game_states_df, market_prices) {

  if (nrow(game_states_df) != length(market_prices)) {
    stop("game_states_df rows must match market_prices length")
  }

  results <- map2_dfr(1:nrow(game_states_df), market_prices, function(i, price) {
    row <- game_states_df[i, ]

    game_state <- create_game_state(
      home_score = row$home_score,
      away_score = row$away_score,
      seconds_remaining = row$seconds_remaining,
      period = row$period,
      home_net_rating = row$home_net_rating %||% 0,
      away_net_rating = row$away_net_rating %||% 0,
      home_rest_days = row$home_rest_days %||% 1,
      away_rest_days = row$away_rest_days %||% 1
    )

    signal <- predictor$get_signal(game_state, price)

    tibble(
      row_id = i,
      market_price = price,
      model_prob = signal$model_prob,
      baseline_wp = signal$baseline_wp,
      blown_lead_prob = signal$blown_lead_prob,
      adjustment = signal$adjustment,
      edge = signal$edge,
      signal = signal$signal,
      confidence = signal$confidence,
      kelly_fraction = signal$kelly_fraction
    )
  })

  bind_cols(game_states_df, results %>% select(-row_id))
}

# ------------------------------------------------------------------------------
# Example Usage
# ------------------------------------------------------------------------------

# # Initialize predictor
# predictor <- KalshiPredictor$new()
# predictor$print_config()
#
# # Create a game state
# # Example: Home team up 12 with 8 minutes left in Q4
# # Home team is slightly better (+3 net rating)
# game_state <- create_game_state(
#   home_score = 98,
#   away_score = 86,
#   seconds_remaining = 480,  # 8 minutes left
#   period = 4,
#   home_net_rating = 3,
#   away_net_rating = 0
# )
#
# # Get signal with market price of 0.85 (85 cents for home win)
# signal <- predictor$get_signal(game_state, market_price = 0.85)
# print(signal)
#
# # Analyze price sensitivity
# sensitivity <- predictor$analyze_price_sensitivity(game_state)
# print(sensitivity)
#
# # Find fair value (where edge = 0)
# fair_value <- sensitivity %>%
#   filter(abs(edge) == min(abs(edge))) %>%
#   pull(market_price)
# cat(sprintf("Model fair value: %.1f%%\n", fair_value * 100))
