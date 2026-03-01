# Trading State R6 Class
# Shared state object for the live betting platform

library(R6)
library(tidyverse)

#' TradingState R6 Class
#' Manages all state for the live betting platform including positions,
#' trade log, game states, and balances
TradingState <- R6::R6Class(
  "TradingState",

  public = list(
    # Configuration
    mode = NULL,              # "paper" or "live"
    paper_balance = NULL,     # Starting paper balance

    # State
    positions = NULL,         # Tibble of all positions
    trade_log = NULL,         # Tibble of all trades
    game_states = NULL,       # List of current game states by game_id
    market_prices = NULL,     # List of current market prices by game_id

    # Balances
    cash_balance = NULL,      # Current cash balance
    position_value = NULL,    # Current value of open positions

    # Status
    is_running = NULL,        # Whether trading loop is active
    last_update = NULL,       # Last state update time

    # Logger
    logger = NULL,

    #' Initialize the trading state
    #' @param mode "paper" or "live"
    #' @param paper_balance Starting balance for paper trading
    #' @param logger Logger instance
    initialize = function(mode = "paper", paper_balance = 1000, logger = NULL) {
      self$mode <- mode
      self$paper_balance <- paper_balance
      self$cash_balance <- paper_balance
      self$position_value <- 0
      self$is_running <- FALSE
      self$last_update <- Sys.time()
      self$logger <- logger

      # Initialize empty tibbles
      self$positions <- tibble(
        position_id = character(),
        game_id = character(),
        strategy = character(),       # "with_leader" or "fade"
        signal = character(),         # "BUY_YES" or "BUY_NO"
        entry_time = as.POSIXct(character()),
        entry_price = numeric(),
        bet_size = numeric(),         # Fraction of balance
        contracts = integer(),        # Number of contracts
        cost_basis = numeric(),       # Total cost
        lead_at_entry = integer(),
        period_at_entry = integer(),
        edge_at_entry = numeric(),
        status = character(),         # "open", "settled_win", "settled_loss"
        exit_time = as.POSIXct(character()),
        exit_price = numeric(),
        pnl = numeric()
      )

      self$trade_log <- tibble(
        trade_id = character(),
        position_id = character(),
        game_id = character(),
        timestamp = as.POSIXct(character()),
        action = character(),         # "OPEN", "CLOSE", "SETTLE"
        strategy = character(),
        signal = character(),
        price = numeric(),
        contracts = integer(),
        amount = numeric(),
        balance_after = numeric(),
        notes = character()
      )

      self$game_states <- list()
      self$market_prices <- list()

      if (!is.null(self$logger)) {
        self$logger$info(sprintf("TradingState initialized in %s mode", mode),
                        balance = paper_balance)
      }
    },

    #' Add a new position
    #' @param game_id Game identifier
    #' @param strategy "with_leader" or "fade"
    #' @param signal Signal details from predictor
    #' @param contracts Number of contracts to buy
    #' @return Position ID
    add_position = function(game_id, strategy, signal, contracts) {
      message(sprintf("[STATE] add_position called: game=%s, strategy=%s, contracts=%d",
                     game_id, strategy, contracts))

      # Hard constraint: max 1 open position per game per strategy
      existing_open <- tryCatch({
        self$positions %>%
          filter(game_id == !!game_id, strategy == !!strategy, status == "open") %>%
          nrow()
      }, error = function(e) 0)

      if (existing_open >= 1) {
        message(sprintf("[STATE] REJECTED: Already have %d open %s position(s) for game %s",
                       existing_open, strategy, game_id))
        if (!is.null(self$logger)) {
          self$logger$warn(sprintf("Duplicate position blocked: %d open %s for game %s",
                                  existing_open, strategy, game_id),
                          game_id = game_id)
        }
        return(NULL)
      }

      position_id <- paste0(
        game_id, "_", strategy, "_",
        format(Sys.time(), "%H%M%S")
      )

      entry_price <- signal$entry_price
      cost_basis <- contracts * entry_price

      message(sprintf("[STATE] entry_price=%.2f, cost_basis=%.2f, current_balance=%.2f",
                     entry_price, cost_basis, self$cash_balance))

      # Check if we have enough balance
      if (cost_basis > self$cash_balance) {
        if (!is.null(self$logger)) {
          self$logger$warn("Insufficient balance for trade",
                          game_id = game_id,
                          required = cost_basis,
                          available = self$cash_balance)
        }
        message(sprintf("[STATE] REJECTED: Insufficient balance (need %.2f, have %.2f)",
                       cost_basis, self$cash_balance))
        return(NULL)
      }

      # Build position record FIRST (before touching balance)
      # This ensures if tibble creation fails, balance is untouched
      # Extract signal fields BEFORE tibble() to avoid column name shadowing
      # (tibble() evaluates sequentially, so a column named "signal" would
      #  shadow the function parameter on subsequent lines)
      sig_signal <- as.character(signal$signal %||% "UNKNOWN")
      sig_bet_size <- as.numeric(signal$bet_size %||% 0.02)
      sig_lead <- as.integer(abs(signal$lead %||% 0))
      sig_period <- as.integer(signal$period %||% 0)
      sig_edge <- as.numeric(signal$edge %||% signal$actual_edge %||% 0)

      new_position <- tryCatch({
        tibble(
          position_id = position_id,
          game_id = as.character(game_id),
          strategy = as.character(strategy),
          signal = sig_signal,
          entry_time = Sys.time(),
          entry_price = as.numeric(entry_price),
          bet_size = sig_bet_size,
          contracts = as.integer(contracts),
          cost_basis = as.numeric(cost_basis),
          lead_at_entry = sig_lead,
          period_at_entry = sig_period,
          edge_at_entry = sig_edge,
          status = "open",
          exit_time = as.POSIXct(NA),
          exit_price = NA_real_,
          pnl = NA_real_
        )
      }, error = function(e) {
        message(sprintf("[STATE] ERROR creating position tibble: %s", e$message))
        if (!is.null(self$logger)) {
          self$logger$error(sprintf("Position tibble creation failed: %s", e$message),
                           game_id = game_id)
        }
        NULL
      })

      if (is.null(new_position)) {
        return(NULL)
      }

      # Add to positions tibble BEFORE deducting balance
      positions_before <- nrow(self$positions)
      updated_positions <- tryCatch({
        bind_rows(self$positions, new_position)
      }, error = function(e) {
        message(sprintf("[STATE] ERROR in bind_rows: %s", e$message))
        if (!is.null(self$logger)) {
          self$logger$error(sprintf("bind_rows failed: %s", e$message),
                           game_id = game_id)
        }
        NULL
      })

      if (is.null(updated_positions)) {
        return(NULL)
      }

      # Only NOW deduct balance (position is safely recorded)
      self$positions <- updated_positions
      self$cash_balance <- self$cash_balance - cost_basis

      if (!is.null(self$logger)) {
        self$logger$info(sprintf("BALANCE: $%.2f -> $%.2f (-%s %.2f)",
                                self$cash_balance + cost_basis, self$cash_balance,
                                strategy, cost_basis),
                        game_id = game_id)
      }
      message(sprintf("[STATE] Balance: $%.2f -> $%.2f | Positions: %d -> %d (added %s)",
                     self$cash_balance + cost_basis, self$cash_balance,
                     positions_before, nrow(self$positions), position_id))

      # Log trade (non-critical, errors won't corrupt balance)
      tryCatch({
        self$log_trade(
          position_id = position_id,
          game_id = game_id,
          action = "OPEN",
          strategy = strategy,
          signal = as.character(signal$signal %||% "UNKNOWN"),
          price = entry_price,
          contracts = contracts,
          amount = -cost_basis,
          notes = sprintf("Edge: %.1f%%, Lead: %d",
                         (signal$edge %||% 0) * 100,
                         abs(signal$lead %||% 0))
        )
      }, error = function(e) {
        message(sprintf("[STATE] WARNING: log_trade failed: %s", e$message))
      })

      self$last_update <- Sys.time()

      position_id
    },

    #' Settle a position
    #' @param position_id Position to settle
    #' @param won Whether the position won
    #' @param kalshi_fee Fee rate on wins (default 7%)
    settle_position = function(position_id, won, kalshi_fee = 0.07) {
      idx <- which(self$positions$position_id == position_id)

      if (length(idx) == 0) {
        if (!is.null(self$logger)) {
          self$logger$warn("Position not found for settlement",
                          position_id = position_id)
        }
        return(NULL)
      }

      pos <- self$positions[idx, ]

      if (pos$status != "open") {
        if (!is.null(self$logger)) {
          self$logger$warn("Position already settled",
                          position_id = position_id,
                          status = pos$status)
        }
        return(NULL)
      }

      # Calculate payout
      if (won) {
        # Win: receive $1 per contract, fee applies only to profit
        gross_payout <- pos$contracts * 1.0
        profit <- gross_payout - pos$cost_basis
        fee_amount <- if (profit > 0) kalshi_fee * profit else 0
        payout <- gross_payout - fee_amount
        pnl <- payout - pos$cost_basis
        status <- "settled_win"
      } else {
        # Loss: lose entire cost basis
        payout <- 0
        pnl <- -pos$cost_basis
        status <- "settled_loss"
      }

      # Update balance
      self$cash_balance <- self$cash_balance + payout

      # Update position
      self$positions$status[idx] <- status
      self$positions$exit_time[idx] <- Sys.time()
      self$positions$exit_price[idx] <- if (won) 1 else 0
      self$positions$pnl[idx] <- pnl

      # Log settlement
      self$log_trade(
        position_id = position_id,
        game_id = pos$game_id,
        action = "SETTLE",
        strategy = pos$strategy,
        signal = pos$signal,
        price = if (won) 1 else 0,
        contracts = pos$contracts,
        amount = payout,
        notes = sprintf("%s: P&L $%.2f", if (won) "WIN" else "LOSS", pnl)
      )

      if (!is.null(self$logger)) {
        self$logger$log_settlement(
          game_id = pos$game_id,
          position_id = position_id,
          result = if (won) "WIN" else "LOSS",
          pnl = pnl
        )
      }

      self$last_update <- Sys.time()

      invisible(pnl)
    },

    #' Log a trade event
    log_trade = function(position_id, game_id, action, strategy, signal,
                         price, contracts, amount, notes = "") {
      trade_id <- paste0("T", format(Sys.time(), "%Y%m%d%H%M%S"),
                        sprintf("%04d", nrow(self$trade_log) + 1))

      new_log <- tibble(
        trade_id = trade_id,
        position_id = position_id,
        game_id = game_id,
        timestamp = Sys.time(),
        action = action,
        strategy = strategy,
        signal = signal,
        price = price,
        contracts = contracts,
        amount = amount,
        balance_after = self$cash_balance,
        notes = notes
      )

      self$trade_log <- bind_rows(self$trade_log, new_log)
    },

    #' Update game state
    #' @param game_id Game identifier
    #' @param game_state Game state from ESPN
    update_game_state = function(game_id, game_state) {
      self$game_states[[game_id]] <- game_state
      self$last_update <- Sys.time()
    },

    #' Update market price
    #' @param game_id Game identifier
    #' @param price Current market price for home team
    update_market_price = function(game_id, price) {
      self$market_prices[[game_id]] <- price
      self$last_update <- Sys.time()
    },

    #' Get open positions
    get_open_positions = function() {
      self$positions %>% filter(status == "open")
    },

    #' Get positions for a specific game
    get_game_positions = function(game_id, status = NULL) {
      result <- self$positions %>% filter(game_id == !!game_id)
      if (!is.null(status)) {
        result <- result %>% filter(status == !!status)
      }
      result
    },

    #' Get position count by strategy for a game
    get_position_count = function(game_id, strategy) {
      self$positions %>%
        filter(game_id == !!game_id, strategy == !!strategy) %>%
        nrow()
    },

    #' Get total balance (cash + position value)
    get_total_balance = function() {
      # Position value = sum of open position cost bases (conservative)
      open_positions <- self$get_open_positions()
      self$position_value <- sum(open_positions$cost_basis)
      self$cash_balance + self$position_value
    },

    #' Get P&L summary
    get_pnl_summary = function() {
      settled <- self$positions %>%
        filter(status %in% c("settled_win", "settled_loss"))

      if (nrow(settled) == 0) {
        return(list(
          total_pnl = 0,
          win_count = 0,
          loss_count = 0,
          win_rate = NA,
          avg_win = NA,
          avg_loss = NA
        ))
      }

      wins <- settled %>% filter(status == "settled_win")
      losses <- settled %>% filter(status == "settled_loss")

      list(
        total_pnl = sum(settled$pnl),
        win_count = nrow(wins),
        loss_count = nrow(losses),
        win_rate = nrow(wins) / nrow(settled),
        avg_win = if (nrow(wins) > 0) mean(wins$pnl) else NA,
        avg_loss = if (nrow(losses) > 0) mean(losses$pnl) else NA
      )
    },

    #' Get performance by strategy
    get_strategy_performance = function() {
      self$positions %>%
        filter(status %in% c("settled_win", "settled_loss")) %>%
        group_by(strategy) %>%
        summarise(
          total_trades = n(),
          wins = sum(status == "settled_win"),
          losses = sum(status == "settled_loss"),
          win_rate = wins / total_trades,
          total_pnl = sum(pnl),
          avg_pnl = mean(pnl),
          .groups = "drop"
        )
    },

    #' Save state to file
    #' @param path File path (defaults to data/trading_state.rds)
    save_state = function(path = NULL) {
      if (is.null(path)) {
        # Use DATA_DIR if available, otherwise default relative path
        path <- if (exists("DATA_DIR")) {
          file.path(DATA_DIR, "trading_state.rds")
        } else {
          "data/trading_state.rds"
        }
      }
      state <- list(
        mode = self$mode,
        paper_balance = self$paper_balance,
        cash_balance = self$cash_balance,
        positions = self$positions,
        trade_log = self$trade_log,
        game_states = self$game_states,
        market_prices = self$market_prices,
        saved_at = Sys.time()
      )

      saveRDS(state, path)

      if (!is.null(self$logger)) {
        self$logger$info("State saved", path = path)
      }
    },

    #' Load state from file
    #' @param path File path
    load_state = function(path = NULL) {
      if (is.null(path)) {
        path <- if (exists("DATA_DIR")) {
          file.path(DATA_DIR, "trading_state.rds")
        } else {
          "data/trading_state.rds"
        }
      }
      if (!file.exists(path)) {
        if (!is.null(self$logger)) {
          self$logger$warn("State file not found", path = path)
        }
        return(FALSE)
      }

      state <- readRDS(path)

      self$mode <- state$mode
      self$paper_balance <- state$paper_balance
      self$cash_balance <- state$cash_balance
      self$positions <- state$positions
      self$trade_log <- state$trade_log
      self$game_states <- state$game_states %||% list()
      self$market_prices <- state$market_prices %||% list()

      if (!is.null(self$logger)) {
        self$logger$info("State loaded",
                        path = path,
                        positions = nrow(self$positions),
                        balance = self$cash_balance)
      }

      TRUE
    },

    #' Reset state (for new session)
    reset = function() {
      # Ensure paper_balance has a valid value
      if (is.null(self$paper_balance) || !is.numeric(self$paper_balance) || is.na(self$paper_balance)) {
        self$paper_balance <- 1000  # Default fallback
      }

      self$cash_balance <- self$paper_balance
      self$position_value <- 0
      self$is_running <- FALSE

      self$positions <- tibble(
        position_id = character(),
        game_id = character(),
        strategy = character(),
        signal = character(),
        entry_time = as.POSIXct(character()),
        entry_price = numeric(),
        bet_size = numeric(),
        contracts = integer(),
        cost_basis = numeric(),
        lead_at_entry = integer(),
        period_at_entry = integer(),
        edge_at_entry = numeric(),
        status = character(),
        exit_time = as.POSIXct(character()),
        exit_price = numeric(),
        pnl = numeric()
      )

      self$trade_log <- tibble(
        trade_id = character(),
        position_id = character(),
        game_id = character(),
        timestamp = as.POSIXct(character()),
        action = character(),
        strategy = character(),
        signal = character(),
        price = numeric(),
        contracts = integer(),
        amount = numeric(),
        balance_after = numeric(),
        notes = character()
      )

      self$game_states <- list()
      self$market_prices <- list()
      self$last_update <- Sys.time()

      if (!is.null(self$logger)) {
        self$logger$info("State reset", balance = self$paper_balance)
      }

      invisible(TRUE)
    },

    #' Print state summary
    print = function() {
      cat("TradingState Summary\n")
      cat("====================\n")
      cat(sprintf("Mode: %s\n", self$mode))
      cat(sprintf("Cash Balance: $%.2f\n", self$cash_balance))
      cat(sprintf("Position Value: $%.2f\n", self$position_value))
      cat(sprintf("Total Balance: $%.2f\n", self$get_total_balance()))
      cat(sprintf("Open Positions: %d\n", nrow(self$get_open_positions())))
      cat(sprintf("Total Trades: %d\n", nrow(self$trade_log)))
      cat(sprintf("Running: %s\n", self$is_running))
      cat(sprintf("Last Update: %s\n", format(self$last_update, "%Y-%m-%d %H:%M:%S")))

      pnl <- self$get_pnl_summary()
      if (!is.na(pnl$win_rate)) {
        cat(sprintf("Win Rate: %.1f%% (%d W / %d L)\n",
                   pnl$win_rate * 100, pnl$win_count, pnl$loss_count))
        cat(sprintf("Total P&L: $%.2f\n", pnl$total_pnl))
      }

      invisible(self)
    }
  )
)
