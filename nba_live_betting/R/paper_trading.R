# Paper Trading Engine
# Simulates trade execution without real money

library(R6)
library(tidyverse)

#' PaperTradingEngine R6 Class
#' Handles paper trade execution and settlement
PaperTradingEngine <- R6::R6Class(
  "PaperTradingEngine",

  public = list(
    state = NULL,           # TradingState object
    logger = NULL,          # Logger object
    kalshi_fee = NULL,      # Kalshi fee rate on wins

    # Configuration
    min_contract_value = NULL,  # Minimum contract value ($)
    default_bet_size = NULL,    # Default bet size (fraction of balance)

    # Spread simulation settings
    base_spread = NULL,         # Base bid-ask spread (e.g., 0.02 = 2 cents)
    volatility_spread_mult = NULL,  # Extra spread during volatile moments

    # Real market data
    kalshi_client = NULL,       # KalshiClient for fetching real prices
    use_real_prices = NULL,     # Whether to use real Kalshi prices
    market_ticker_cache = NULL, # Cache of game_id -> ticker mappings

    #' Initialize the paper trading engine
    #' @param state TradingState object
    #' @param logger Logger object
    #' @param kalshi_fee Fee rate on wins (default 7%)
    #' @param min_contract_value Minimum contract value in dollars
    #' @param base_spread Base bid-ask spread for simulation (default 2 cents)
    #' @param kalshi_client Optional KalshiClient for real price data
    initialize = function(state, logger = NULL, kalshi_fee = 0.07,
                         min_contract_value = 1, base_spread = 0.02,
                         kalshi_client = NULL) {
      self$state <- state
      self$logger <- logger
      self$kalshi_fee <- kalshi_fee
      self$min_contract_value <- min_contract_value
      self$default_bet_size <- 0.02  # 2% default
      self$base_spread <- base_spread
      self$volatility_spread_mult <- 1.5  # Spread widens 50% during high volatility

      # Real market data settings
      self$kalshi_client <- kalshi_client
      self$use_real_prices <- !is.null(kalshi_client)
      self$market_ticker_cache <- list()

      if (self$use_real_prices && !is.null(logger)) {
        logger$info("Paper trading using REAL Kalshi prices for market data")
      }
    },

    #' Simulate bid-ask spread based on game state
    #' @param mid_price The mid-market price (model's fair value)
    #' @param game_state Current game state (optional, for volatility adjustment)
    #' @return List with bid, ask, and spread info
    simulate_spread = function(mid_price, game_state = NULL) {
      # Base spread
      spread <- self$base_spread

      # Widen spread in certain situations
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
        buy_no_price = 1 - bid,   # Pay 1-bid to buy NO
        source = "simulated"
      )
    },

    #' Get market ticker for a game (with caching)
    #' @param game_id ESPN game ID
    #' @param game_state Game state with team info
    #' @return Market ticker or NULL
    get_market_ticker = function(game_id, game_state = NULL) {
      gid <- as.character(game_id)

      # Check cache first (use "NO_MATCH" sentinel since NULL removes keys)
      cached <- self$market_ticker_cache[[gid]]
      if (!is.null(cached)) {
        return(if (identical(cached, "NO_MATCH")) NULL else cached)
      }

      if (is.null(self$kalshi_client)) {
        return(NULL)
      }

      # Try to find market
      ticker <- tryCatch({
        home_team <- game_state$home_team_abbrev %||% ""
        away_team <- game_state$away_team_abbrev %||% ""

        # Skip lookup if we don't have team abbreviations
        if (!nzchar(home_team) || !nzchar(away_team)) {
          return(NULL)
        }

        self$kalshi_client$find_game_market(game_id, home_team, away_team, Sys.Date())
      }, error = function(e) {
        NULL
      })

      # Cache the result (use sentinel for NULL to avoid re-lookup)
      self$market_ticker_cache[[gid]] <- if (is.null(ticker)) "NO_MATCH" else ticker

      ticker
    },

    #' Get real spread data from Kalshi
    #' @param game_id ESPN game ID
    #' @param game_state Game state for ticker lookup
    #' @return Spread data from Kalshi, or NULL if unavailable
    get_real_spread_data = function(game_id, game_state = NULL) {
      if (!self$use_real_prices || is.null(self$kalshi_client)) {
        return(NULL)
      }

      ticker <- self$get_market_ticker(game_id, game_state)
      if (is.null(ticker)) {
        return(NULL)
      }

      spread_data <- tryCatch({
        self$kalshi_client$get_spread_data(ticker)
      }, error = function(e) {
        if (!is.null(self$logger)) {
          self$logger$debug(sprintf("Failed to get Kalshi spread: %s", e$message))
        }
        NULL
      })

      # Guard: ensure spread_data is a proper list before modifying
      if (!is.null(spread_data) && !is.list(spread_data)) {
        spread_data <- NULL
      }

      if (!is.null(spread_data)) {
        spread_data$source <- "kalshi"
        spread_data$ticker <- ticker
      }

      spread_data
    },

    #' Get fresh spread data from Kalshi (bypasses cache, for trade execution)
    #' @param game_id ESPN game ID
    #' @param game_state Game state for ticker lookup
    #' @return Fresh spread data from Kalshi, or NULL if unavailable
    get_fresh_spread_data = function(game_id, game_state = NULL) {
      if (!self$use_real_prices || is.null(self$kalshi_client)) {
        return(NULL)
      }

      ticker <- self$get_market_ticker(game_id, game_state)
      if (is.null(ticker)) {
        return(NULL)
      }

      spread_data <- tryCatch({
        self$kalshi_client$get_fresh_spread_data(ticker)
      }, error = function(e) {
        if (!is.null(self$logger)) {
          self$logger$debug(sprintf("Failed to get fresh Kalshi spread: %s", e$message))
        }
        NULL
      })

      # Guard: ensure spread_data is a proper list before modifying
      if (!is.null(spread_data) && !is.list(spread_data)) {
        spread_data <- NULL
      }

      if (!is.null(spread_data)) {
        spread_data$source <- "kalshi_fresh"
        spread_data$ticker <- ticker
      }

      spread_data
    },

    #' Get spread data (real or simulated)
    #' Tries real Kalshi data first, falls back to simulation
    #' @param game_id ESPN game ID
    #' @param mid_price Mid price for simulation fallback
    #' @param game_state Game state
    #' @return Spread data
    get_spread_data = function(game_id, mid_price, game_state = NULL) {
      # Try real Kalshi data first
      if (self$use_real_prices) {
        real_data <- self$get_real_spread_data(game_id, game_state)
        if (!is.null(real_data) && !is.na(real_data$mid_price)) {
          message(sprintf("[SPREAD] Using REAL Kalshi data: mid=%.3f, spread=%.1f%%, ticker=%s",
                         real_data$mid_price, real_data$spread_pct %||% 0, real_data$ticker %||% "?"))
          return(real_data)
        }
      }

      # Fall back to simulated spread
      simulated <- self$simulate_spread(mid_price, game_state)
      message(sprintf("[SPREAD] Using SIMULATED spread: mid=%.3f, spread=%.1f%%",
                     mid_price, simulated$spread_pct))
      simulated
    },

    #' Calculate actual edge accounting for simulated spread
    #' @param market_price Market price for spread simulation (leader/trailer's price)
    #' @param model_win_prob Model's probability we win the bet (hold_rate or expected_prob)
    #' @param side "yes" or "no" (which side you're betting)
    #' @param game_state Current game state
    #' @return List with edge info
    calculate_paper_edge = function(market_price, model_win_prob, side, game_state = NULL) {
      # market_price: used for spread simulation
      # model_win_prob: our estimate of true win probability (what we expect to happen)

      # Simulate spread using market price
      spread_data <- self$simulate_spread(market_price, game_state)

      # Get execution price based on side
      if (side == "yes" || side == "BUY_YES") {
        execution_price <- spread_data$buy_yes_price
      } else {
        execution_price <- spread_data$buy_no_price
      }

      # Calculate breakeven with fee-on-profit model
      payout_if_win <- 1 - self$kalshi_fee * (1 - execution_price)
      breakeven_prob <- execution_price / payout_if_win

      # Actual edge after spread and fees
      # model_win_prob is already the probability we win (not home team)
      actual_edge <- model_win_prob - breakeven_prob

      # Edge at mid (what we'd have without spread)
      mid_execution <- if (side == "yes" || side == "BUY_YES") market_price else (1 - market_price)
      mid_payout <- 1 - self$kalshi_fee * (1 - mid_execution)
      mid_breakeven <- mid_execution / mid_payout
      mid_edge <- model_win_prob - mid_breakeven

      list(
        actual_edge = actual_edge,
        mid_edge = mid_edge,
        edge_lost_to_spread = mid_edge - actual_edge,
        execution_price = execution_price,
        market_price = market_price,
        model_win_prob = model_win_prob,
        spread = spread_data$spread,
        spread_pct = spread_data$spread_pct,
        breakeven_prob = breakeven_prob
      )
    },

    #' Calculate number of contracts for a bet
    #' @param bet_size Fraction of balance to bet
    #' @param entry_price Price per contract
    #' @return Number of contracts (integer)
    calculate_contracts = function(bet_size, entry_price) {
      # Use available balance
      available <- self$state$cash_balance

      # Calculate target bet amount
      target_amount <- available * bet_size

      # Calculate number of contracts
      # Each contract costs entry_price and pays $1 if won
      contracts <- floor(target_amount / entry_price)

      # Ensure at least 1 contract if we can afford it
      if (contracts < 1 && available >= entry_price) {
        contracts <- 1
      }

      as.integer(max(0, contracts))
    },

    #' Execute a paper trade
    #' @param game_id Game identifier
    #' @param strategy "with_leader" or "fade"
    #' @param signal Signal details from predictor
    #' @param game_state Current game state for spread simulation (optional)
    #' @return Position ID if successful, NULL otherwise
    execute_trade = function(game_id, strategy, signal, game_state = NULL) {
      if (is.null(signal) || !is.list(signal)) {
        message(sprintf("[TRADE] Invalid signal for %s", game_id))
        return(NULL)
      }

      if (signal$signal == "HOLD") {
        return(NULL)
      }

      # Calculate contracts
      bet_size <- signal$bet_size %||% self$default_bet_size
      model_entry_price <- signal$entry_price

      if (is.null(model_entry_price) || !is.numeric(model_entry_price)) {
        message(sprintf("[TRADE] Invalid entry_price for %s", game_id))
        return(NULL)
      }

      # Calculate spread-adjusted execution price
      side <- signal$signal  # "BUY_YES" or "BUY_NO"

      # Get model's win probability from signal
      # WITH LEADER: hold_rate (probability leader holds and wins)
      # FADE: expected_win_rate or calibrated_prob (probability trailer wins)
      if (!is.null(signal$hold_rate)) {
        # WITH LEADER: hold_rate is the probability our bet wins
        model_win_prob <- signal$hold_rate
      } else if (!is.null(signal$expected_win_rate)) {
        # FADE: expected_win_rate is the probability the trailer wins
        model_win_prob <- signal$expected_win_rate
      } else if (!is.null(signal$calibrated_prob)) {
        # FADE fallback: calibrated_prob is the calibrated probability
        model_win_prob <- signal$calibrated_prob
      } else {
        # Fallback to market price (shouldn't happen for properly formed signals)
        model_win_prob <- if (side == "BUY_YES") model_entry_price else (1 - model_entry_price)
        message(sprintf("[TRADE] WARNING: No model probability in signal, using market price"))
      }

      # Get spread data (tries real Kalshi first, falls back to simulated)
      # model_entry_price is the leader/trailer's price from the signal
      spread_data <- self$get_spread_data(game_id, model_entry_price, game_state)

      # Guard: ensure spread_data is a proper list
      if (!is.list(spread_data)) {
        message(sprintf("[TRADE] SKIPPED: spread_data is not a list (type=%s)", typeof(spread_data)))
        return(NULL)
      }

      # Get execution price based on side
      if (side == "BUY_YES") {
        execution_price <- spread_data$buy_yes_price %||% (model_entry_price + 0.01)
        market_mid <- spread_data$mid_price %||% spread_data$mid %||% model_entry_price
      } else {
        execution_price <- spread_data$buy_no_price %||% (1 - model_entry_price + 0.01)
        market_mid <- 1 - (spread_data$mid_price %||% spread_data$mid %||% model_entry_price)
      }

      # Handle NA execution_price (e.g., no liquidity)
      if (is.na(execution_price) || !is.numeric(execution_price)) {
        execution_price <- if (side == "BUY_YES") model_entry_price + 0.01 else (1 - model_entry_price + 0.01)
      }
      if (is.na(market_mid) || !is.numeric(market_mid)) {
        market_mid <- model_entry_price
      }

      # Calculate breakeven with fee-on-profit model
      # Kalshi charges fee on profits (not gross payout):
      #   payout_if_win = 1 - fee * (1 - execution_price)
      #   breakeven = execution_price / payout_if_win
      payout_if_win <- 1 - self$kalshi_fee * (1 - execution_price)
      breakeven_prob <- execution_price / payout_if_win

      # Actual edge after spread and fees
      actual_edge <- model_win_prob - breakeven_prob

      # Handle NA edge
      if (is.na(actual_edge)) {
        message(sprintf("[TRADE] SKIPPED: edge is NA (exec=%.3f, model_prob=%.3f)", execution_price, model_win_prob))
        return(NULL)
      }

      # Log with source info
      spread_source <- spread_data$source %||% "unknown"
      spread_pct <- spread_data$spread_pct %||% ((spread_data$spread %||% 0.02) / max(market_mid, 0.01) * 100)

      message(sprintf("[TRADE] %s %s: signal=%s, market=%.3f, model_prob=%.1f%%, exec_price=%.3f, spread=%.1f%% (%s), edge=%.1f%%",
                     game_id, strategy, signal$signal, market_mid, model_win_prob * 100, execution_price,
                     spread_pct %||% 0, spread_source, actual_edge * 100))

      # Check if edge is still positive after spread
      if (actual_edge < 0) {
        message(sprintf("[TRADE] SKIPPED: Edge negative after spread (%.1f%%)", actual_edge * 100))
        if (!is.null(self$logger)) {
          self$logger$info(sprintf("Trade skipped - edge negative after spread: %.1f%%", actual_edge * 100),
                          game_id = game_id, strategy = strategy)
        }
        return(NULL)
      }

      contracts <- self$calculate_contracts(bet_size, execution_price)

      if (contracts < 1) {
        if (!is.null(self$logger)) {
          self$logger$warn("Insufficient balance for trade",
                          game_id = game_id,
                          strategy = strategy,
                          required = execution_price)
        }
        return(NULL)
      }

      # Update signal with actual execution price and edge info
      signal_with_spread <- signal
      signal_with_spread$entry_price <- execution_price
      signal_with_spread$model_price <- model_entry_price
      signal_with_spread$spread <- spread_data$spread %||% 0.02
      signal_with_spread$spread_source <- spread_data$source %||% "simulated"
      signal_with_spread$actual_edge <- actual_edge
      signal_with_spread$model_win_prob <- model_win_prob
      signal_with_spread$breakeven_prob <- breakeven_prob

      # Add position through state
      position_id <- self$state$add_position(
        game_id = game_id,
        strategy = strategy,
        signal = signal_with_spread,
        contracts = contracts
      )

      if (!is.null(position_id)) {
        message(sprintf("[TRADE] SUCCESS: Position %s created at %.3f (%s spread), edge=%.1f%%, balance now $%.2f",
                       position_id, execution_price, spread_data$source %||% "sim", actual_edge * 100, self$state$cash_balance))

        if (!is.null(self$logger)) {
          self$logger$log_trade(
            game_id = game_id,
            strategy = strategy,
            signal = signal$signal,
            price = execution_price,
            size = contracts
          )
        }
      } else {
        message(sprintf("[TRADE] FAILED: add_position returned NULL for %s", game_id))
      }

      position_id
    },

    #' Settle a position
    #' @param position_id Position to settle
    #' @param home_won Whether home team won
    #' @return P&L if successful
    settle_position = function(position_id, home_won) {
      # Get position details
      pos <- self$state$positions %>%
        filter(position_id == !!position_id) %>%
        slice(1)

      if (nrow(pos) == 0) {
        if (!is.null(self$logger)) {
          self$logger$warn("Position not found", position_id = position_id)
        }
        return(NULL)
      }

      # Determine if position won
      # BUY_YES wins if home wins
      # BUY_NO wins if home loses
      position_won <- (pos$signal == "BUY_YES" && home_won) ||
                      (pos$signal == "BUY_NO" && !home_won)

      # Settle through state
      pnl <- self$state$settle_position(
        position_id = position_id,
        won = position_won,
        kalshi_fee = self$kalshi_fee
      )

      pnl
    },

    #' Settle all open positions for a game
    #' @param game_id Game to settle
    #' @param home_won Whether home team won
    #' @return Total P&L for the game
    settle_game = function(game_id, home_won) {
      open_positions <- self$state$get_game_positions(game_id, status = "open")

      if (nrow(open_positions) == 0) {
        return(0)
      }

      total_pnl <- 0

      for (i in 1:nrow(open_positions)) {
        pnl <- self$settle_position(
          position_id = open_positions$position_id[i],
          home_won = home_won
        )
        if (!is.null(pnl)) {
          total_pnl <- total_pnl + pnl
        }
      }

      if (!is.null(self$logger)) {
        self$logger$info(
          sprintf("Settled %d positions for game %s", nrow(open_positions), game_id),
          total_pnl = round(total_pnl, 2)
        )
      }

      total_pnl
    },

    #' Get market price from game state
    #' Tries real Kalshi prices first, falls back to model estimate
    #' @param game_state Game state from ESPN
    #' @param predictor KalshiPredictor with WP model
    #' @param include_spread If TRUE, returns full spread data
    #' @param game_id Optional game ID for real price lookup
    #' @return Market price for home team (or list with spread data)
    estimate_market_price = function(game_state, predictor, include_spread = FALSE, game_id = NULL) {
      # Validate inputs
      if (!is.list(game_state) || is.null(predictor)) {
        if (include_spread) {
          return(list(mid_price = 0.5, spread_data = NULL, source = "default"))
        }
        return(0.5)
      }

      # Try real Kalshi prices first
      if (self$use_real_prices && !is.null(game_id)) {
        real_data <- self$get_real_spread_data(game_id, game_state)
        if (!is.null(real_data) && !is.na(real_data$mid_price)) {
          message(sprintf("[MARKET] Using REAL Kalshi price: %.3f (ticker: %s)",
                         real_data$mid_price, real_data$ticker %||% "?"))
          if (include_spread) {
            return(list(
              mid_price = real_data$mid_price,
              spread_data = real_data,
              source = "kalshi"
            ))
          }
          return(real_data$mid_price)
        }
      }

      # Fall back to model estimate
      message(sprintf("[MARKET] Using MODEL estimate (game_id=%s, real_prices=%s)",
                     game_id %||% "NULL", self$use_real_prices))

      # Debug: Log game state being used for WP calculation
      message(sprintf("[WP] Game state: margin=%d, period=%d, sec_remain=%.0f, rating_adv=%.1f",
                     game_state$home_margin %||% 0,
                     game_state$period %||% 0,
                     game_state$seconds_remaining %||% 0,
                     game_state$home_rating_advantage %||% 0))

      # Use safe wrapper if available in global environment
      wp_result <- tryCatch({
        if (exists("safe_get_wp", envir = globalenv())) {
          get("safe_get_wp", envir = globalenv())(predictor, game_state)
        } else {
          # Direct call with validation
          predictor$get_adjusted_wp(game_state)
        }
      }, error = function(e) {
        message(sprintf("[WP] Error getting WP: %s", e$message))
        NULL
      })

      # Safety check for wp_result
      if (!is.list(wp_result)) {
        if (include_spread) {
          return(list(mid_price = 0.5, spread_data = NULL))
        }
        return(0.5)
      }

      adjusted_wp <- wp_result$adjusted_wp
      baseline_wp <- wp_result$baseline_wp %||% NA

      # Ensure it's a valid single numeric value
      if (!is.numeric(adjusted_wp) || length(adjusted_wp) != 1 || is.na(adjusted_wp)) {
        if (include_spread) {
          return(list(mid_price = 0.5, spread_data = NULL))
        }
        return(0.5)
      }

      # Clip to reasonable bounds
      mid_price <- max(0.05, min(0.95, adjusted_wp))

      # Get spread data if requested
      if (include_spread) {
        spread_data <- self$simulate_spread(mid_price, game_state)

        message(sprintf("[WP] Result: mid=%.3f, spread=%.1f%%, buy_yes=%.3f, buy_no=%.3f",
                       mid_price, spread_data$spread_pct,
                       spread_data$buy_yes_price, spread_data$buy_no_price))

        return(list(
          mid_price = mid_price,
          adjusted_wp = adjusted_wp,
          baseline_wp = baseline_wp,
          blown_lead_prob = wp_result$blown_lead_prob %||% NA,
          spread_data = spread_data
        ))
      }

      # Debug: Log WP result
      message(sprintf("[WP] Result: baseline=%.3f, adjusted=%.3f, blown_prob=%.3f",
                     baseline_wp %||% 0,
                     adjusted_wp %||% 0,
                     wp_result$blown_lead_prob %||% 0))

      mid_price
    },

    #' Get execution summary
    get_summary = function() {
      list(
        mode = "paper",
        balance = self$state$cash_balance,
        open_positions = nrow(self$state$get_open_positions()),
        total_trades = nrow(self$state$trade_log),
        pnl = self$state$get_pnl_summary()
      )
    }
  )
)

#' Create a paper trading engine
#' @param state TradingState object
#' @param logger Logger object
#' @return PaperTradingEngine instance
create_paper_engine <- function(state, logger = NULL) {
  PaperTradingEngine$new(state = state, logger = logger)
}
