# Kalshi NBA Live Betting Predictor
# Combines Win Probability Model + Blown Lead Model for edge detection
#
# TWO STRATEGIES (Combined Strategy V2 - Optimized):
#
# 1. WITH LEADER: Bet on leading team to hold lead
#    - Entry: Q3/Q4 at 15+ pts OR any period at 21+ pts
#    - Exit: Hold to settlement (NO stop-loss - hurts performance)
#    - Max 1 position per game (re-entry hurts performance)
#    - Backtest: $31 P&L, 0.48 Sharpe, 95.4% win rate
#
# 2. SELECTIVE FADE: Bet on trailing team when +EV
#    - Entry: 7+ pts when blown_rate > breakeven_rate
#    - Exit: Hold to settlement
#    - MULTIPLE entries allowed (each +EV opportunity worth ~$1.27)
#    - Backtest (cap 10): $4,014 P&L, 11.25 Sharpe
#
# Automated Trading Mode:
#   - Fade: Up to MAX_FADE_POSITIONS_PER_GAME entries (default 10)
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

# SELECTIVE FADE Strategy Parameters
FADE_MIN_LEAD <- 7               # Minimum lead for fade consideration
MAX_FADE_POSITIONS_PER_GAME <- 10  # Each +EV opportunity worth ~$1.27
KALSHI_FEE <- 0.07               # 7% Kalshi fee on wins

# General Parameters
MIN_SECONDS_FOR_SIGNAL <- 60     # Don't signal in final minute
MIN_MINUTES_BETWEEN_FADES <- 2   # Avoid rapid-fire entries on same price move

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

    # Configuration
    max_fade_per_game = NULL,
    max_wl_per_game = NULL,
    min_minutes_between_fades = NULL,

    #' Initialize the predictor with both models
    initialize = function(wp_model_path = "win_probability_model.rds",
                          blown_lead_model_path = "blown_lead_model.rds",
                          adjustment_weight = 1.0,
                          max_fade_per_game = MAX_FADE_POSITIONS_PER_GAME,
                          max_wl_per_game = MAX_WITH_LEADER_PER_GAME,
                          min_minutes_between_fades = MIN_MINUTES_BETWEEN_FADES) {

      message("Loading Win Probability model...")
      self$wp_model <- load_wp_model(wp_model_path)

      message("Loading Blown Lead model...")
      self$blown_lead_model <- load_model(blown_lead_model_path)

      self$adjustment_weight <- adjustment_weight
      self$max_fade_per_game <- max_fade_per_game
      self$max_wl_per_game <- max_wl_per_game
      self$min_minutes_between_fades <- min_minutes_between_fades

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

      message("KalshiPredictor initialized successfully!")
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

    #' Get blown lead probability from blown lead model
    get_blown_lead_prob = function(game_state) {
      if (abs(game_state$home_margin) < 3) {
        return(0.5)
      }

      predict_blown_lead(
        model = self$blown_lead_model,
        largest_lead = abs(game_state$home_margin),
        minutes_elapsed = game_state$minutes_elapsed,
        period = game_state$period,
        is_home = game_state$home_margin > 0,
        rest_advantage = game_state$rest_advantage,
        net_rating_advantage = game_state$net_rating_advantage
      )
    },

    #' Calculate adjusted win probability combining both models
    get_adjusted_wp = function(game_state) {
      baseline_wp <- self$get_baseline_wp(game_state)
      blown_lead_prob <- self$get_blown_lead_prob(game_state)

      if (game_state$home_margin > 0) {
        adjustment <- blown_lead_prob * self$adjustment_weight * (baseline_wp - 0.5)
        adjusted_wp <- baseline_wp - adjustment
      } else if (game_state$home_margin < 0) {
        adjustment <- blown_lead_prob * self$adjustment_weight * (0.5 - baseline_wp)
        adjusted_wp <- baseline_wp + adjustment
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

    #' Check if WITH LEADER entry conditions are met
    check_with_leader_eligible = function(game_state) {
      lead <- abs(game_state$home_margin)
      period <- game_state$period

      (period >= 3 && lead >= WITH_LEADER_MIN_LEAD_Q3Q4) ||
      (lead >= WITH_LEADER_MIN_LEAD_ANY)
    },

    #' Check if SELECTIVE FADE entry conditions are met
    check_fade_eligible = function(game_state, market_price) {
      lead <- abs(game_state$home_margin)
      period <- game_state$period

      if (lead < FADE_MIN_LEAD || period > 4) {
        return(list(eligible = FALSE, edge = 0, blown_rate = NA, breakeven = NA))
      }

      blown_rate <- get_actual_blown_rate(lead, period)
      home_leading <- game_state$home_margin > 0
      trailer_price <- if (home_leading) (1 - market_price) else market_price
      breakeven <- calc_breakeven_blown(trailer_price)
      edge <- blown_rate - breakeven

      list(
        eligible = edge > 0,
        edge = edge,
        blown_rate = blown_rate,
        breakeven = breakeven,
        trailer_price = trailer_price
      )
    },

    #' Calculate tiered bet size
    calculate_bet_tier = function(game_state, strategy, edge = 0) {
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
        if (edge >= 0.10) {
          return(list(tier = "TIER_2_LARGE", bet_size = BET_SIZE_TIER2))
        } else if (edge >= 0.05) {
          return(list(tier = "TIER_3_MEDIUM", bet_size = BET_SIZE_TIER3))
        }
        return(list(tier = "TIER_4_STANDARD", bet_size = BET_SIZE_TIER4))
      }

      list(tier = "NO_BET", bet_size = 0)
    },

    #' Generate trading signal based on game state and market price
    #' @param game_id Unique identifier for the game
    #' @param game_state List with all required game state variables
    #' @param market_price Current Kalshi market price for home team win (0-1)
    #' @param current_time Current timestamp (optional, for tracking)
    get_signal = function(game_id, game_state, market_price, current_time = Sys.time()) {
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
        blown_rate <- get_actual_blown_rate(lead, period)
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
      fade_count <- self$get_position_count(game_id, "fade")
      fade_check <- self$check_fade_eligible(game_state, market_price)

      # Check time since last fade
      last_fade_time <- self$get_last_fade_time(game_id)
      time_ok <- TRUE
      if (!is.null(last_fade_time)) {
        minutes_since <- as.numeric(difftime(current_time, last_fade_time, units = "mins"))
        time_ok <- minutes_since >= self$min_minutes_between_fades
      }

      if (fade_check$eligible && fade_count < self$max_fade_per_game && time_ok) {
        fade_bet_tier <- self$calculate_bet_tier(game_state, "fade", fade_check$edge)

        fade_signal <- list(
          signal = if (home_leading) "BUY_NO" else "BUY_YES",
          reason = sprintf("Fade %s (+%d pts, edge: %.1f%%, position %d/%d)",
                          if (home_leading) "home" else "away", lead,
                          fade_check$edge * 100, fade_count + 1, self$max_fade_per_game),
          entry_price = fade_check$trailer_price,
          blown_rate = fade_check$blown_rate,
          breakeven = fade_check$breakeven,
          edge = fade_check$edge,
          bet_tier = fade_bet_tier$tier,
          bet_size = fade_bet_tier$bet_size,
          exit_strategy = "HOLD_TO_SETTLEMENT",
          position_number = fade_count + 1,
          max_positions = self$max_fade_per_game,
          ev_per_position = 1.27  # From backtest
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
          reason = if (fade_check$edge <= 0 && lead >= FADE_MIN_LEAD) {
            sprintf("No edge (blown: %.1f%% <= breakeven: %.1f%%)",
                    fade_check$blown_rate * 100, fade_check$breakeven * 100)
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

      cat("STRATEGY 2: SELECTIVE FADE (Bet on trailing team)\n")
      cat(sprintf("  Entry: %d+ pts when blown_rate > breakeven_rate\n", FADE_MIN_LEAD))
      cat("  Exit: Hold to settlement\n")
      cat(sprintf("  Max positions per game: %d\n", self$max_fade_per_game))
      cat(sprintf("  Min time between entries: %d minutes\n", self$min_minutes_between_fades))
      cat("  EV per position: ~$1.27\n")
      cat(sprintf("  Backtest (cap %d): $4,014 P&L, 11.25 Sharpe\n\n", self$max_fade_per_game))

      cat("COMBINED EXPECTED PERFORMANCE:\n")
      cat("  With Leader: $31 + Fade (10/game): $4,014 = ~$4,045 total\n")
      cat("  100% probability of profit in backtest\n\n")

      cat("BET SIZING:\n")
      cat(sprintf("  TIER 1 (MAX):      %.0f%% - Q4 + 20+ pts\n", BET_SIZE_TIER1 * 100))
      cat(sprintf("  TIER 2 (LARGE):    %.0f%% - Q4 + 15+ pts OR 21+ pts OR 10%%+ edge\n", BET_SIZE_TIER2 * 100))
      cat(sprintf("  TIER 3 (MEDIUM):   %.0f%% - 5%%+ edge\n", BET_SIZE_TIER3 * 100))
      cat(sprintf("  TIER 4 (STANDARD): %.0f%% - All other signals\n\n", BET_SIZE_TIER4 * 100))

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
