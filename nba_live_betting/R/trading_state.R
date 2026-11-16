# Trading State R6 Class
# Shared state object for the live betting platform

library(R6)
library(tidyverse)

# In-game strategies that trade the same moneyline market. At most one open
# position per game across all of these, so strategies never pay two spreads
# and two fees to hold offsetting sides of the same market.
MONEYLINE_STRATEGIES <- c("with_leader", "fade")

#' Kalshi trading fee, charged at execution on every fill (win or lose):
#' ceil_to_cent(rate * contracts * price * (1 - price)).
#' The inner round() keeps FP noise (e.g. 1.0000000002) from bumping a cent.
kalshi_trade_fee <- function(contracts, price, rate = 0.07) {
  ceiling(round(rate * contracts * price * (1 - price) * 100, 6)) / 100
}

empty_positions_tbl <- function() {
  tibble(
    position_id = character(),
    game_id = character(),
    strategy = character(),       # "with_leader", "fade", "pregame_big_fav"
    signal = character(),         # "BUY_YES" or "BUY_NO"
    entry_time = as.POSIXct(character()),
    entry_price = numeric(),
    bet_size = numeric(),         # Fraction of balance
    contracts = integer(),        # Number of contracts
    cost_basis = numeric(),       # contracts * entry_price (excludes fee)
    fee = numeric(),              # Kalshi fee paid at entry
    lead_at_entry = integer(),
    period_at_entry = integer(),
    edge_at_entry = numeric(),
    # Entry context, so losses can be traced to bad data vs. an informed market
    ticker = character(),
    home_score = integer(),
    away_score = integer(),
    clock = character(),
    model_prob = numeric(),       # lookup-table / model win prob for our side
    bid = numeric(),              # bid of the side we bought
    ask = numeric(),              # ask of the side we bought
    spread_source = character(),
    status = character(),         # "open", "settled_win", "settled_loss", "void"
    exit_time = as.POSIXct(character()),
    exit_price = numeric(),
    pnl = numeric()               # net of fee
  )
}

empty_trade_log_tbl <- function() {
  tibble(
    trade_id = character(),
    position_id = character(),
    game_id = character(),
    timestamp = as.POSIXct(character()),
    action = character(),         # "OPEN", "SETTLE", "VOID", "ADJUST"
    strategy = character(),
    signal = character(),
    price = numeric(),
    contracts = integer(),
    amount = numeric(),           # signed cash flow; the ledger
    balance_after = numeric(),
    notes = character()
  )
}

empty_skipped_tbl <- function() {
  tibble(
    timestamp = as.POSIXct(character()),
    game_id = character(),
    strategy = character(),
    signal = character(),
    reason = character(),
    ticker = character(),
    home_score = integer(),
    away_score = integer(),
    period = integer(),
    clock = character(),
    model_prob = numeric(),
    bid = numeric(),
    ask = numeric(),
    fee_per_contract = numeric()
  )
}

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
    trade_log = NULL,         # Tibble of all cash flows (the ledger)
    skipped_entries = NULL,   # Signals that passed the model but were blocked by a guard
    fee_rate = 0.07,          # Kalshi fee coefficient
    game_states = NULL,       # List of current game states by game_id
    market_prices = NULL,     # List of current market prices by game_id

    # Balances
    cash_balance = NULL,      # Cache of ledger_balance(); never updated directly
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

      self$positions <- empty_positions_tbl()
      self$trade_log <- empty_trade_log_tbl()
      self$skipped_entries <- empty_skipped_tbl()

      self$game_states <- list()
      self$market_prices <- list()

      if (!is.null(self$logger)) {
        self$logger$info(sprintf("TradingState initialized in %s mode", mode),
                        balance = paper_balance)
      }
    },

    #' Add a new position
    #' @param game_id Game identifier
    #' @param strategy "with_leader", "fade", or a spread strategy
    #' @param signal Signal details; may carry entry context (ticker, scores,
    #'   clock, model_win_prob, bid, ask, spread_source, fee)
    #' @param contracts Number of contracts to buy
    #' @return Position ID
    add_position = function(game_id, strategy, signal, contracts) {
      message(sprintf("[STATE] add_position called: game=%s, strategy=%s, contracts=%d",
                     game_id, strategy, contracts))

      open_here <- tryCatch(
        self$positions %>% filter(game_id == !!game_id, status == "open"),
        error = function(e) self$positions[0, ])

      # Hard constraint: max 1 open position per game per strategy
      existing_open <- sum(open_here$strategy == strategy)
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

      # Hard constraint: max 1 open moneyline position per game across strategies
      if (strategy %in% MONEYLINE_STRATEGIES) {
        other <- open_here$strategy[open_here$strategy %in% MONEYLINE_STRATEGIES]
        if (length(other) > 0) {
          message(sprintf("[STATE] REJECTED: game %s already has an open %s moneyline position",
                         game_id, other[1]))
          if (!is.null(self$logger)) {
            self$logger$warn(sprintf("Cross-strategy position blocked: %s wanted %s, %s already open",
                                    strategy, game_id, other[1]), game_id = game_id)
          }
          return(NULL)
        }
      }

      position_id <- paste0(
        game_id, "_", strategy, "_",
        format(Sys.time(), "%H%M%S")
      )

      entry_price <- as.numeric(signal$entry_price)
      cost_basis <- contracts * entry_price
      fee <- as.numeric(signal$fee %||% kalshi_trade_fee(contracts, entry_price, self$fee_rate))
      total_cost <- cost_basis + fee

      message(sprintf("[STATE] entry_price=%.3f, cost_basis=%.2f, fee=%.2f, current_balance=%.2f",
                     entry_price, cost_basis, fee, self$cash_balance))

      if (total_cost > self$cash_balance) {
        if (!is.null(self$logger)) {
          self$logger$warn("Insufficient balance for trade",
                          game_id = game_id,
                          required = total_cost,
                          available = self$cash_balance)
        }
        message(sprintf("[STATE] REJECTED: Insufficient balance (need %.2f, have %.2f)",
                       total_cost, self$cash_balance))
        return(NULL)
      }

      # Extract signal fields BEFORE tibble() to avoid column name shadowing
      # (tibble() evaluates sequentially, so a column named "signal" would
      #  shadow the function parameter on subsequent lines)
      num1 <- function(x) { x <- suppressWarnings(as.numeric(x %||% NA)); if (length(x) == 1) x else NA_real_ }
      chr1 <- function(x) { x <- as.character(x %||% NA); if (length(x) == 1) x else NA_character_ }
      sig_signal <- as.character(signal$signal %||% "UNKNOWN")
      sig_bet_size <- as.numeric(signal$bet_size %||% 0.02)
      sig_lead <- as.integer(abs(signal$lead %||% 0))
      sig_period <- as.integer(signal$period %||% 0)
      sig_edge <- as.numeric(signal$actual_edge %||% signal$edge %||% 0)
      ctx <- list(
        ticker = chr1(signal$ticker), home_score = as.integer(num1(signal$home_score)),
        away_score = as.integer(num1(signal$away_score)), clock = chr1(signal$clock),
        model_prob = num1(signal$model_win_prob), bid = num1(signal$side_bid),
        ask = num1(signal$side_ask), spread_source = chr1(signal$spread_source)
      )

      new_position <- tryCatch({
        tibble(
          position_id = position_id,
          game_id = as.character(game_id),
          strategy = as.character(strategy),
          signal = sig_signal,
          entry_time = Sys.time(),
          entry_price = entry_price,
          bet_size = sig_bet_size,
          contracts = as.integer(contracts),
          cost_basis = as.numeric(cost_basis),
          fee = fee,
          lead_at_entry = sig_lead,
          period_at_entry = sig_period,
          edge_at_entry = sig_edge,
          ticker = ctx$ticker,
          home_score = ctx$home_score,
          away_score = ctx$away_score,
          clock = ctx$clock,
          model_prob = ctx$model_prob,
          bid = ctx$bid,
          ask = ctx$ask,
          spread_source = ctx$spread_source,
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
      if (is.null(new_position)) return(NULL)

      positions_before <- self$positions
      balance_before <- self$cash_balance
      ok <- tryCatch({
        self$positions <- bind_rows(self$positions, new_position)
        # The ledger row IS the balance change; if it fails, roll back the position
        self$log_trade(
          position_id = position_id,
          game_id = game_id,
          action = "OPEN",
          strategy = strategy,
          signal = sig_signal,
          price = entry_price,
          contracts = contracts,
          amount = -total_cost,
          notes = sprintf(paste0("Edge %.1f%% | lead %d Q%d %s | score %s-%s | model %.1f%% | ",
                                 "bid %.3f ask %.3f | fee $%.2f | %s %s"),
                          sig_edge * 100, sig_lead, sig_period, ctx$clock %||% "?",
                          ctx$home_score, ctx$away_score, ctx$model_prob * 100,
                          ctx$bid, ctx$ask, fee, ctx$ticker, ctx$spread_source)
        )
        TRUE
      }, error = function(e) {
        message(sprintf("[STATE] ERROR recording position, rolled back: %s", e$message))
        if (!is.null(self$logger)) {
          self$logger$error(sprintf("add_position rolled back: %s", e$message), game_id = game_id)
        }
        FALSE
      })
      if (!ok) {
        self$positions <- positions_before
        self$cash_balance <- self$ledger_balance()
        return(NULL)
      }

      if (!is.null(self$logger)) {
        self$logger$info(sprintf("BALANCE: $%.2f -> $%.2f (-%s %.2f incl. fee %.2f)",
                                balance_before, self$cash_balance, strategy, total_cost, fee),
                        game_id = game_id)
      }
      message(sprintf("[STATE] Balance: $%.2f -> $%.2f | Positions: %d -> %d (added %s)",
                     balance_before, self$cash_balance,
                     nrow(positions_before), nrow(self$positions), position_id))

      self$last_update <- Sys.time()
      position_id
    },

    #' Settle a position
    #' @param position_id Position to settle
    #' @param won Whether the position won
    #' @param kalshi_fee Only used for legacy positions opened before fees were
    #'   charged at entry (fee column NA): applies the old fee-on-profit model.
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

      entry_fee <- if ("fee" %in% names(pos)) pos$fee else NA_real_
      if (won) {
        payout <- pos$contracts * 1.0
        if (is.na(entry_fee)) {
          # Legacy position: fee was never charged at entry
          payout <- payout - kalshi_fee * max(0, payout - pos$cost_basis)
        }
        status <- "settled_win"
      } else {
        payout <- 0
        status <- "settled_loss"
      }
      pnl <- payout - pos$cost_basis - (if (is.na(entry_fee)) 0 else entry_fee)

      self$positions$status[idx] <- status
      self$positions$exit_time[idx] <- Sys.time()
      self$positions$exit_price[idx] <- if (won) 1 else 0
      self$positions$pnl[idx] <- pnl

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

    #' Void an open position (market cancelled/voided): refund cost basis and
    #' fee, P&L 0
    #' @param position_id Position to void
    #' @param reason Short note for the trade log
    #' @return 0 (P&L) invisibly, or NULL if not found / not open
    void_position = function(position_id, reason = "market voided") {
      idx <- which(self$positions$position_id == position_id)
      if (length(idx) == 0 || self$positions$status[idx] != "open") {
        if (!is.null(self$logger)) {
          self$logger$warn("Position not open for void", position_id = position_id)
        }
        return(NULL)
      }
      pos <- self$positions[idx, ]
      refund <- pos$cost_basis + (if ("fee" %in% names(pos) && !is.na(pos$fee)) pos$fee else 0)
      self$positions$status[idx] <- "void"
      self$positions$exit_time[idx] <- Sys.time()
      self$positions$exit_price[idx] <- pos$entry_price
      self$positions$pnl[idx] <- 0
      self$log_trade(
        position_id = position_id, game_id = pos$game_id, action = "VOID",
        strategy = pos$strategy, signal = pos$signal, price = pos$entry_price,
        contracts = pos$contracts, amount = refund,
        notes = sprintf("VOID (%s): refunded $%.2f", reason, refund)
      )
      self$last_update <- Sys.time()
      invisible(0)
    },

    #' Balance implied by the ledger: starting balance + every cash flow.
    #' This is the source of truth; cash_balance is a cache of it.
    ledger_balance = function() {
      amt <- self$trade_log$amount
      (self$paper_balance %||% 0) + sum(amt[!is.na(amt)])
    },

    #' Log a ledger entry and refresh cash_balance from the ledger.
    #' This is the only place the balance changes.
    log_trade = function(position_id, game_id, action, strategy, signal,
                         price, contracts, amount, notes = "") {
      trade_id <- paste0("T", format(Sys.time(), "%Y%m%d%H%M%S"),
                        sprintf("%04d", nrow(self$trade_log) + 1))

      balance_after <- self$ledger_balance() + amount

      new_log <- tibble(
        trade_id = trade_id,
        position_id = position_id,
        game_id = as.character(game_id),
        timestamp = Sys.time(),
        action = action,
        strategy = strategy,
        signal = signal,
        price = as.numeric(price),
        contracts = as.integer(contracts),
        amount = as.numeric(amount),
        balance_after = balance_after,
        notes = notes
      )

      self$trade_log <- bind_rows(self$trade_log, new_log)
      self$cash_balance <- self$ledger_balance()
    },

    #' Record a signal that a guard blocked, with its full entry context
    log_skipped_entry = function(game_id, strategy, signal, reason, ctx = list()) {
      num1 <- function(x) { x <- suppressWarnings(as.numeric(x %||% NA)); if (length(x) == 1) x else NA_real_ }
      chr1 <- function(x) { x <- as.character(x %||% NA); if (length(x) == 1) x else NA_character_ }
      row <- tibble(
        timestamp = Sys.time(), game_id = as.character(game_id),
        strategy = as.character(strategy), signal = chr1(signal), reason = as.character(reason),
        ticker = chr1(ctx$ticker), home_score = as.integer(num1(ctx$home_score)),
        away_score = as.integer(num1(ctx$away_score)), period = as.integer(num1(ctx$period)),
        clock = chr1(ctx$clock), model_prob = num1(ctx$model_prob),
        bid = num1(ctx$bid), ask = num1(ctx$ask), fee_per_contract = num1(ctx$fee_per_contract)
      )
      self$skipped_entries <- bind_rows(self$skipped_entries %||% empty_skipped_tbl(), row)
      if (!is.null(self$logger)) {
        self$logger$info(sprintf("SKIP %s %s: %s", strategy, row$signal, reason),
                        game_id = game_id, ticker = row$ticker,
                        score = sprintf("%s-%s", row$home_score, row$away_score),
                        period = row$period, clock = row$clock,
                        model_prob = round(row$model_prob, 4),
                        bid = row$bid, ask = row$ask, fee = row$fee_per_contract)
      }
      invisible(row)
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

    #' Resolve the state file path. There is exactly one state file: the
    #' absolute STATE_FILE defined in global.R. No relative-path fallback, so an
    #' ad-hoc script run from another working directory can't silently read or
    #' write a different copy.
    resolve_state_path = function(path = NULL) {
      if (is.null(path)) {
        if (!exists("STATE_FILE")) {
          stop("No state path given and STATE_FILE is not defined (source global.R first)")
        }
        path <- get("STATE_FILE")
      }
      normalizePath(path, mustWork = FALSE)
    },

    #' Save state to file
    #' @param path File path (defaults to STATE_FILE)
    save_state = function(path = NULL) {
      path <- self$resolve_state_path(path)
      state <- list(
        mode = self$mode,
        paper_balance = self$paper_balance,
        cash_balance = self$ledger_balance(),
        positions = self$positions,
        trade_log = self$trade_log,
        skipped_entries = self$skipped_entries,
        market_prices = self$market_prices,
        saved_at = Sys.time()
      )

      # Write-then-rename so a crash mid-save can't truncate the only copy
      tmp <- paste0(path, ".tmp")
      saveRDS(state, tmp)
      if (!file.rename(tmp, path)) {
        file.copy(tmp, path, overwrite = TRUE)
        unlink(tmp)
      }

      if (!is.null(self$logger)) {
        self$logger$info("State saved", path = path)
      }
    },

    #' Load state from file. The balance is rebuilt from the trade ledger; the
    #' stored cash_balance is only compared against it.
    #' @param path File path (defaults to STATE_FILE)
    load_state = function(path = NULL) {
      path <- self$resolve_state_path(path)
      if (!file.exists(path)) {
        if (!is.null(self$logger)) {
          self$logger$warn("State file not found", path = path)
        }
        return(FALSE)
      }

      state <- readRDS(path)

      self$mode <- state$mode
      self$paper_balance <- state$paper_balance
      # bind_rows onto the empty schema adds any newer columns (fee, ticker, ...) as NA
      self$positions <- bind_rows(empty_positions_tbl(), state$positions)
      self$trade_log <- bind_rows(empty_trade_log_tbl(), state$trade_log)
      self$skipped_entries <- bind_rows(empty_skipped_tbl(), state$skipped_entries %||% empty_skipped_tbl())
      # Game states are never restored: anything saved is stale by definition,
      # and stale states are what let last season's games trade on Nov 2.
      self$game_states <- list()
      self$market_prices <- state$market_prices %||% list()

      self$cash_balance <- self$ledger_balance()
      stored <- as.numeric(state$cash_balance %||% NA)
      if (!is.na(stored) && abs(stored - self$cash_balance) > 0.005) {
        msg <- sprintf("Stored balance $%.2f disagrees with ledger $%.2f (diff $%.2f); using ledger",
                       stored, self$cash_balance, stored - self$cash_balance)
        message(sprintf("[STATE] WARNING: %s", msg))
        if (!is.null(self$logger)) self$logger$warn(msg, path = path)
      }

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

      self$positions <- empty_positions_tbl()
      self$trade_log <- empty_trade_log_tbl()
      self$skipped_entries <- empty_skipped_tbl()

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
