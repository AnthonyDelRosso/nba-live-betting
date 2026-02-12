# Automated Trader
# Main trading loop that polls ESPN, generates signals, and executes trades

library(R6)
library(later)
library(tidyverse)

#' AutomatedTrader R6 Class
#' Manages the automated trading loop
AutomatedTrader <- R6::R6Class(
  "AutomatedTrader",

  public = list(
    # Dependencies
    state = NULL,             # TradingState object
    predictor = NULL,         # KalshiPredictor object
    paper_engine = NULL,      # PaperTradingEngine for paper mode
    kalshi_client = NULL,     # KalshiClient for live mode
    logger = NULL,            # Logger object

    # Configuration
    poll_interval = NULL,     # Seconds between polls
    mode = NULL,              # "paper" or "live"
    team_ratings = NULL,      # Team net ratings for predictions

    # State
    is_running = NULL,        # Whether the loop is active
    last_poll = NULL,         # Last poll timestamp
    games_today = NULL,       # Current day's games
    scheduled_task = NULL,    # later() task handle

    # In-game adjustment tracking
    price_history = NULL,     # List of price histories by game_id
    price_history_minutes = NULL,  # Minutes of history to keep

    #' Initialize the automated trader
    #' @param state TradingState object
    #' @param predictor KalshiPredictor object
    #' @param paper_engine PaperTradingEngine (for paper mode)
    #' @param kalshi_client KalshiClient (for live mode)
    #' @param logger Logger object
    #' @param poll_interval Seconds between polls (default 30)
    #' @param team_ratings Tibble with team net ratings
    initialize = function(state, predictor, paper_engine = NULL,
                         kalshi_client = NULL, logger = NULL,
                         poll_interval = 30, team_ratings = NULL) {
      self$state <- state
      self$predictor <- predictor
      self$paper_engine <- paper_engine
      self$kalshi_client <- kalshi_client
      self$logger <- logger
      self$poll_interval <- poll_interval
      self$team_ratings <- team_ratings
      self$mode <- state$mode

      self$is_running <- FALSE
      self$games_today <- list()

      # Initialize price history tracking for in-game adjustments
      self$price_history <- list()
      self$price_history_minutes <- 5  # Track 5 minutes of price history

      if (!is.null(self$logger)) {
        self$logger$info("AutomatedTrader initialized",
                        mode = self$mode,
                        poll_interval = poll_interval)
      }
    },

    #' Store price for in-game velocity calculations
    #' @param game_id Game identifier
    #' @param price Current market price (trailer's price)
    #' @param timestamp Current timestamp
    store_price_history = function(game_id, price, timestamp = Sys.time()) {
      if (is.null(self$price_history[[game_id]])) {
        self$price_history[[game_id]] <- list()
      }

      # Add current price
      self$price_history[[game_id]][[length(self$price_history[[game_id]]) + 1]] <- list(
        price = price,
        timestamp = timestamp
      )

      # Prune old entries (keep last 10 minutes)
      cutoff <- timestamp - (10 * 60)  # 10 minutes ago
      self$price_history[[game_id]] <- Filter(function(x) {
        x$timestamp >= cutoff
      }, self$price_history[[game_id]])
    },

    #' Get price from N minutes ago for velocity calculation
    #' @param game_id Game identifier
    #' @param minutes_ago How many minutes back to look
    #' @return Price from that time, or NULL if not available
    get_price_minutes_ago = function(game_id, minutes_ago = 5) {
      if (is.null(self$price_history[[game_id]]) ||
          length(self$price_history[[game_id]]) == 0) {
        return(NULL)
      }

      target_time <- Sys.time() - (minutes_ago * 60)
      history <- self$price_history[[game_id]]

      # Find the entry closest to target_time
      best_entry <- NULL
      best_diff <- Inf

      for (entry in history) {
        diff <- abs(as.numeric(difftime(entry$timestamp, target_time, units = "secs")))
        if (diff < best_diff && entry$timestamp <= target_time + 60) {  # Within 1 minute tolerance
          best_diff <- diff
          best_entry <- entry
        }
      }

      if (!is.null(best_entry) && best_diff < 120) {  # Must be within 2 minutes
        return(best_entry$price)
      }

      NULL
    },

    #' Start the trading loop
    start = function() {
      if (self$is_running) {
        if (!is.null(self$logger)) {
          self$logger$warn("Trading loop already running")
        }
        return(invisible(FALSE))
      }

      self$is_running <- TRUE
      self$state$is_running <- TRUE

      if (!is.null(self$logger)) {
        self$logger$info("Trading loop started")
      }

      # Run first poll immediately
      self$poll()

      invisible(TRUE)
    },

    #' Stop the trading loop
    stop = function() {
      if (!self$is_running) {
        return(invisible(FALSE))
      }

      self$is_running <- FALSE
      self$state$is_running <- FALSE

      # Cancel scheduled task
      if (!is.null(self$scheduled_task)) {
        # later doesn't have a cancel mechanism, but we check is_running in poll
        self$scheduled_task <- NULL
      }

      if (!is.null(self$logger)) {
        self$logger$info("Trading loop stopped")
      }

      invisible(TRUE)
    },

    #' Main poll function - called every poll_interval seconds
    poll = function() {
      if (!self$is_running) {
        return(invisible(NULL))
      }

      self$last_poll <- Sys.time()

      # 1. Fetch ESPN scores
      tryCatch({
        self$poll_espn_scores()
      }, error = function(e) {
        if (!is.null(self$logger)) {
          self$logger$error(sprintf("ESPN poll error: %s", e$message))
        }
      })

      # 2. Check for settlements (ended games)
      tryCatch({
        self$check_settlements()
      }, error = function(e) {
        if (!is.null(self$logger)) {
          self$logger$error(sprintf("Settlement error: %s", e$message))
        }
      })

      # 3. Process live games for signals
      tryCatch({
        self$process_live_games()
      }, error = function(e) {
        if (!is.null(self$logger)) {
          # Get more details about the error
          err_call <- deparse(e$call)[1]
          self$logger$error(sprintf("Process games error: %s (in: %s)", e$message, err_call))
        }
      })

      # 4. Save state periodically
      tryCatch({
        if (runif(1) < 0.1) {  # Save ~10% of polls
          self$state$save_state()
        }
      }, error = function(e) {
        # Ignore save errors
      })

      # Schedule next poll
      if (self$is_running) {
        self$scheduled_task <- later::later(
          function() self$poll(),
          delay = self$poll_interval
        )
      }

      invisible(TRUE)
    },

    #' Fetch ESPN scores and update game states
    poll_espn_scores = function() {
      # Get live game states from ESPN
      live_states <- tryCatch({
        get_live_game_states(
          date = Sys.Date(),
          team_ratings = self$team_ratings
        )
      }, error = function(e) {
        if (!is.null(self$logger)) {
          self$logger$warn(sprintf("Failed to get live states: %s", e$message))
        }
        list()
      })

      # Safety check
      if (!is.list(live_states)) {
        live_states <- list()
      }

      # Update game states in trading state
      for (game_id in names(live_states)) {
        if (is.list(live_states[[game_id]])) {
          self$state$update_game_state(game_id, live_states[[game_id]])
        }
      }

      # Also get full scoreboard for context
      scoreboard <- tryCatch({
        get_nba_scoreboard(Sys.Date())
      }, error = function(e) {
        list(games = list())
      })

      self$games_today <- scoreboard$games %||% list()

      if (!is.null(self$logger)) {
        n_live <- length(live_states)
        n_total <- length(self$games_today)
        self$logger$debug(sprintf("ESPN poll: %d live / %d total games", n_live, n_total))
      }

      # Log balance status periodically (every poll)
      if (!is.null(self$state)) {
        balance <- self$state$cash_balance %||% 0
        open_pos <- tryCatch(nrow(self$state$get_open_positions()), error = function(e) 0)
        total_pos <- tryCatch(nrow(self$state$positions), error = function(e) 0)
        message(sprintf("[%s] Balance: $%.2f | Positions: %d open / %d total | Live games: %d",
                       format(Sys.time(), "%H:%M:%S"),
                       balance, open_pos, total_pos, length(live_states)))
      }

      # Also log predictor position counts
      if (!is.null(self$predictor)) {
        pred_pos <- tryCatch(nrow(self$predictor$positions), error = function(e) 0)
        pred_games <- tryCatch(length(self$predictor$game_positions), error = function(e) 0)
        message(sprintf("[%s] Predictor tracking: %d positions across %d games",
                       format(Sys.time(), "%H:%M:%S"), pred_pos, pred_games))
      }

      live_states
    },

    #' Check for ended games and settle positions
    check_settlements = function() {
      # Get final games with error handling
      final_games <- tryCatch({
        get_final_games(Sys.Date())
      }, error = function(e) {
        list()
      })

      # Safety check - ensure final_games is a list
      if (!is.list(final_games) || length(final_games) == 0) {
        return(invisible(NULL))
      }

      for (game in final_games) {
        tryCatch({
          # Safety check - ensure game is a list with required fields
          if (!is.list(game) || is.null(game$game_id)) {
            next
          }

          game_id <- as.character(game$game_id)

          # Check if we have open positions for this game
          open_positions <- tryCatch({
            self$state$get_game_positions(game_id, status = "open")
          }, error = function(e) {
            data.frame()
          })

          if (!is.data.frame(open_positions) || nrow(open_positions) == 0) {
            next
          }

          home_won <- isTRUE(game$home_won)

          if (self$mode == "paper" && !is.null(self$paper_engine)) {
            tryCatch({
              self$paper_engine$settle_game(game_id, home_won)
            }, error = function(e) {
              if (!is.null(self$logger)) {
                self$logger$debug(sprintf("Paper settle error: %s", e$message))
              }
            })
          } else {
            # For live mode, settlements happen automatically on Kalshi
            # But we still need to update our local state
            for (i in 1:nrow(open_positions)) {
              tryCatch({
                pos <- open_positions[i, , drop = FALSE]
                signal_val <- as.character(pos$signal)
                position_won <- (signal_val == "BUY_YES" && home_won) ||
                                (signal_val == "BUY_NO" && !home_won)
                self$state$settle_position(as.character(pos$position_id), position_won)
              }, error = function(e) {
                # Ignore individual position settle errors
              })
            }
          }

          if (!is.null(self$logger)) {
            winner_name <- if (home_won) {
              as.character(game$home_team %||% "Home")
            } else {
              as.character(game$away_team %||% "Away")
            }
            self$logger$info(sprintf("Settled game %s: %s won", game_id, winner_name))
          }
        }, error = function(e) {
          if (!is.null(self$logger)) {
            self$logger$debug(sprintf("Settlement error for game: %s", e$message))
          }
        })
      }
    },

    #' Process live games for trading signals
    process_live_games = function() {
      game_states <- self$state$game_states

      # Safety check - ensure game_states is a proper list
      if (!is.list(game_states) || length(game_states) == 0) {
        return(invisible(NULL))
      }

      # Safety check - ensure predictor exists and is valid
      if (is.null(self$predictor)) {
        if (!is.null(self$logger)) {
          self$logger$warn("Predictor is NULL, skipping game processing")
        }
        return(invisible(NULL))
      }

      # Get game IDs safely
      game_ids <- tryCatch({
        names(game_states)
      }, error = function(e) {
        character(0)
      })

      if (length(game_ids) == 0) {
        return(invisible(NULL))
      }

      for (game_id in game_ids) {
        tryCatch({
          # Safely extract game state
          raw_game_state <- tryCatch({
            game_states[[game_id]]
          }, error = function(e) {
            NULL
          })

          # Skip if extraction failed
          if (is.null(raw_game_state) || !is.list(raw_game_state)) {
            next
          }

          # Validate game state using global helper (check globalenv explicitly)
          game_state <- tryCatch({
            if (exists("validate_game_state", envir = globalenv())) {
              get("validate_game_state", envir = globalenv())(raw_game_state)
            } else {
              # Inline validation fallback
              if (!is.list(raw_game_state)) return(NULL)
              list(
                home_score = as.numeric(raw_game_state$home_score %||% 0),
                away_score = as.numeric(raw_game_state$away_score %||% 0),
                home_margin = as.numeric(raw_game_state$home_margin %||% 0),
                seconds_remaining = as.numeric(raw_game_state$seconds_remaining %||% 2880),
                minutes_elapsed = as.numeric(raw_game_state$minutes_elapsed %||% 0),
                period = as.integer(raw_game_state$period %||% 1),
                clock_minutes = as.numeric(raw_game_state$clock_minutes %||% 0),
                clock_seconds = as.numeric(raw_game_state$clock_seconds %||% 0),
                home_net_rating = as.numeric(raw_game_state$home_net_rating %||% 0),
                away_net_rating = as.numeric(raw_game_state$away_net_rating %||% 0),
                home_rating_advantage = as.numeric(raw_game_state$home_rating_advantage %||% 0),
                rest_advantage = as.numeric(raw_game_state$rest_advantage %||% 0),
                net_rating_advantage = as.numeric(raw_game_state$net_rating_advantage %||% 0)
              )
            }
          }, error = function(e) {
            NULL
          })

          if (is.null(game_state)) {
            next
          }

          # Skip if game is final
          if (isTRUE(raw_game_state$is_final)) {
            next
          }

          # Get market price safely
          market_price <- tryCatch({
            self$get_market_price(game_id, game_state)
          }, error = function(e) {
            0.5  # Default
          })

          if (!is.numeric(market_price) || length(market_price) != 1 || is.na(market_price)) {
            market_price <- 0.5
          }

          # Ensure market price is in valid range
          market_price <- max(0.01, min(0.99, market_price))

          # Update market price in state
          tryCatch({
            self$state$update_market_price(game_id, market_price)
          }, error = function(e) {
            # Ignore state update errors
          })

          # Calculate trailer price for in-game adjustments
          home_leading <- (game_state$home_margin %||% 0) > 0
          trailer_price <- if (home_leading) (1 - market_price) else market_price

          # Store price history for velocity calculation
          tryCatch({
            self$store_price_history(game_id, trailer_price)
          }, error = function(e) {
            # Ignore price history errors
          })

          # Build live_data for in-game adjustments
          live_data <- NULL
          tryCatch({
            price_5min_ago <- self$get_price_minutes_ago(game_id, 5)

            # Get run differential from play-by-play data
            recent_scoring <- tryCatch({
              get_recent_scoring(game_id, minutes_back = 5)
            }, error = function(e) {
              NULL
            })

            # Build live_data with both velocity and run differential
            trailer_pts <- NULL
            leader_pts <- NULL
            if (!is.null(recent_scoring)) {
              trailer_pts <- recent_scoring$trailer_pts
              leader_pts <- recent_scoring$leader_pts
            }

            # Only create live_data if we have at least one data source
            if (!is.null(price_5min_ago) || !is.null(trailer_pts)) {
              live_data <- list(
                trailer_price_5min_ago = price_5min_ago,
                trailer_pts_last_5 = trailer_pts,
                leader_pts_last_5 = leader_pts
              )

              # Log in-game adjustment data
              velocity_str <- if (!is.null(price_5min_ago)) {
                sprintf("velocity=%.2fc/min", (trailer_price - price_5min_ago) / 5 * 100)
              } else {
                "velocity=N/A"
              }

              run_str <- if (!is.null(trailer_pts) && !is.null(leader_pts)) {
                sprintf("run_diff=%+d (trailer %d, leader %d)",
                       trailer_pts - leader_pts, trailer_pts, leader_pts)
              } else {
                "run_diff=N/A"
              }

              message(sprintf("[INGAME] Game %s: %s, %s", game_id, velocity_str, run_str))
            }
          }, error = function(e) {
            # Ignore live_data errors
          })

          # Check current position counts BEFORE getting signal
          wl_count <- 0
          fade_count <- 0
          if (!is.null(self$predictor)) {
            tryCatch({
              wl_count <- self$predictor$get_position_count(game_id, "with_leader")
              fade_count <- self$predictor$get_position_count(game_id, "fade")
            }, error = function(e) {
              # Ignore
            })
          }
          message(sprintf("[SIGNAL] Game %s: existing positions WL=%d, Fade=%d",
                         game_id, wl_count, fade_count))

          # Get signal from predictor with live_data for in-game adjustments
          signal <- tryCatch({
            # Direct call with live_data for in-game adjustments
            self$predictor$get_signal(
              game_id = as.character(game_id),
              game_state = game_state,
              market_price = market_price,
              live_data = live_data  # Pass in-game adjustment data
            )
          }, error = function(e) {
            if (!is.null(self$logger)) {
              self$logger$debug(sprintf("Signal error for %s: %s", game_id, e$message))
            }
            NULL
          })

          # Validate signal structure
          if (is.null(signal) || !is.list(signal)) {
            next
          }

          # Process with_leader signal (with explicit type checks)
          wl_signal_data <- signal$with_leader
          if (is.list(wl_signal_data) && !is.null(wl_signal_data$signal)) {
            wl_signal <- as.character(wl_signal_data$signal)
            if (length(wl_signal) == 1 && !is.na(wl_signal) && wl_signal != "HOLD") {
              self$execute_signal(game_id, "with_leader", wl_signal_data, game_state)
            }
          }

          # Process fade signal (with explicit type checks)
          fade_signal_data <- signal$fade
          if (is.list(fade_signal_data) && !is.null(fade_signal_data$signal)) {
            fade_signal <- as.character(fade_signal_data$signal)
            if (length(fade_signal) == 1 && !is.na(fade_signal) && fade_signal != "HOLD") {
              self$execute_signal(game_id, "fade", fade_signal_data, game_state)
            }
          }

        }, error = function(e) {
          if (!is.null(self$logger)) {
            self$logger$debug(sprintf("Error processing game %s: %s", game_id, e$message))
          }
        })
      }
    },

    #' Get market price for a game
    #' @param game_id Game identifier
    #' @param game_state Current game state
    #' @return Market price (home team win probability)
    get_market_price = function(game_id, game_state) {
      # Validate game_state is a list
      if (!is.list(game_state)) {
        return(0.5)
      }

      tryCatch({
        if (self$mode == "paper") {
          # In paper mode, use real Kalshi prices if available, else estimate from model
          if (!is.null(self$paper_engine)) {
            price <- tryCatch({
              self$paper_engine$estimate_market_price(
                game_state, self$predictor,
                include_spread = FALSE,
                game_id = as.character(game_id)
              )
            }, error = function(e) {
              NA
            })

            if (is.numeric(price) && length(price) == 1 && !is.na(price)) {
              return(max(0.05, min(0.95, price)))
            }
          }
          return(0.5)
        }

        # In live mode, get from Kalshi
        if (!is.null(self$kalshi_client)) {
          # Find market ticker
          ticker <- tryCatch({
            self$kalshi_client$find_game_market(
              game_id = as.character(game_id),
              home_team = as.character(game_state$home_team_abbrev %||% ""),
              away_team = as.character(game_state$away_team_abbrev %||% ""),
              game_date = Sys.Date()
            )
          }, error = function(e) {
            NULL
          })

          if (!is.null(ticker)) {
            best_price <- tryCatch({
              self$kalshi_client$get_best_price(ticker, "yes")
            }, error = function(e) {
              NULL
            })

            if (is.list(best_price) && !is.null(best_price$price)) {
              price <- as.numeric(best_price$price)
              if (!is.na(price)) {
                return(max(0.05, min(0.95, price)))
              }
            }
          }
        }

        # Fallback to model estimate
        if (!is.null(self$paper_engine)) {
          price <- tryCatch({
            self$paper_engine$estimate_market_price(game_state, self$predictor)
          }, error = function(e) {
            NA
          })

          if (is.numeric(price) && length(price) == 1 && !is.na(price)) {
            return(max(0.05, min(0.95, price)))
          }
        }

        0.5  # Default
      }, error = function(e) {
        0.5  # Default on error
      })
    },

    #' Execute a trading signal
    #' @param game_id Game identifier
    #' @param strategy "with_leader" or "fade"
    #' @param signal Signal details
    #' @param game_state Current game state
    execute_signal = function(game_id, strategy, signal, game_state) {
      message(sprintf("[EXEC] execute_signal called: game=%s, strategy=%s", game_id, strategy))

      tryCatch({
        # Validate signal is a proper list
        if (!is.list(signal)) {
          message(sprintf("[EXEC] REJECTED: signal is not a list, type=%s", typeof(signal)))
          return(invisible(NULL))
        }

        # Validate required signal fields exist
        if (is.null(signal$signal) || is.null(signal$entry_price)) {
          message(sprintf("[EXEC] REJECTED: missing required fields (signal=%s, entry_price=%s)",
                         is.null(signal$signal), is.null(signal$entry_price)))
          return(invisible(NULL))
        }

        message(sprintf("[EXEC] Signal valid: %s at price %.2f", signal$signal, signal$entry_price))

        # Create a copy of signal to avoid modifying the original
        signal_copy <- as.list(signal)

        # Add game state info to signal for record-keeping (safely)
        if (is.list(game_state)) {
          signal_copy$lead <- as.numeric(game_state$home_margin %||% 0)
          signal_copy$period <- as.integer(game_state$period %||% 1)
        } else {
          signal_copy$lead <- 0
          signal_copy$period <- 1
        }

        if (self$mode == "paper") {
          message(sprintf("[EXEC] Calling paper_engine$execute_trade..."))

          # Check paper_engine exists
          if (is.null(self$paper_engine)) {
            message("[EXEC] ERROR: paper_engine is NULL!")
            return(invisible(NULL))
          }

          # Paper trade - pass game_state for spread simulation
          position_id <- tryCatch({
            self$paper_engine$execute_trade(game_id, strategy, signal_copy, game_state)
          }, error = function(e) {
            message(sprintf("[EXEC] Paper trade error: %s", e$message))
            NULL
          })

          if (!is.null(position_id)) {
            message(sprintf("[EXEC] SUCCESS: position_id=%s", position_id))

            # Record in predictor for position tracking - THIS IS CRITICAL
            # Without this, predictor will keep generating same signal
            tryCatch({
              if (!is.null(self$predictor)) {
                message(sprintf("[EXEC] Recording position in predictor for %s/%s...", game_id, strategy))

                # Ensure signal_copy has all required fields for record_position
                signal_for_predictor <- list(
                  signal = as.character(signal_copy$signal %||% "UNKNOWN"),
                  entry_price = as.numeric(signal_copy$entry_price %||% 0.5),
                  bet_size = as.numeric(signal_copy$bet_size %||% 0.02),
                  lead = as.integer(abs(signal_copy$lead %||% 0)),
                  period = as.integer(signal_copy$period %||% 1),
                  edge = as.numeric(signal_copy$edge %||% 0)
                )

                message(sprintf("[EXEC] Signal details: price=%.2f, size=%.2f, lead=%d, period=%d",
                               signal_for_predictor$entry_price,
                               signal_for_predictor$bet_size,
                               signal_for_predictor$lead,
                               signal_for_predictor$period))

                self$predictor$record_position(game_id, strategy, signal_for_predictor)

                # Verify it was recorded
                new_count <- self$predictor$get_position_count(game_id, strategy)
                message(sprintf("[EXEC] Predictor now has %d %s positions for game %s",
                               new_count, strategy, game_id))
              } else {
                message("[EXEC] WARNING: predictor is NULL, cannot track position!")
              }
            }, error = function(e) {
              message(sprintf("[EXEC] ERROR recording position in predictor: %s", e$message))
              message(sprintf("[EXEC] Error details: %s", paste(capture.output(print(e)), collapse = "\n")))
            })

            if (!is.null(self$logger)) {
              self$logger$log_trade(
                game_id = game_id,
                strategy = strategy,
                signal = as.character(signal_copy$signal %||% "UNKNOWN"),
                price = as.numeric(signal_copy$entry_price %||% 0),
                size = as.numeric(signal_copy$bet_size %||% 0)
              )
            }
          } else {
            message("[EXEC] execute_trade returned NULL")
          }
        } else {
          # Live trade via Kalshi
          message("[EXEC] Live mode - calling execute_live_trade")
          self$execute_live_trade(game_id, strategy, signal_copy, game_state)
        }
      }, error = function(e) {
        message(sprintf("[EXEC] ERROR: %s", e$message))
        if (!is.null(self$logger)) {
          self$logger$error(sprintf("Execute signal error: %s", e$message))
        }
      })

      invisible(NULL)
    },

    #' Execute a live trade on Kalshi
    execute_live_trade = function(game_id, strategy, signal, game_state) {
      if (is.null(self$kalshi_client)) {
        if (!is.null(self$logger)) {
          self$logger$error("No Kalshi client configured for live trading")
        }
        return(NULL)
      }

      # Find market
      ticker <- self$kalshi_client$find_game_market(
        game_id = game_id,
        home_team = game_state$home_team_abbrev,
        away_team = game_state$away_team_abbrev,
        game_date = Sys.Date()
      )

      if (is.null(ticker)) {
        if (!is.null(self$logger)) {
          self$logger$warn("Could not find Kalshi market",
                          game_id = game_id)
        }
        return(NULL)
      }

      # Determine order parameters
      side <- if (signal$signal == "BUY_YES") "yes" else "no"

      # Calculate contracts
      balance <- self$kalshi_client$get_balance()
      if (is.null(balance)) {
        return(NULL)
      }

      bet_amount <- balance * signal$bet_size
      contracts <- floor(bet_amount / signal$entry_price)

      if (contracts < 1) {
        if (!is.null(self$logger)) {
          self$logger$warn("Insufficient balance for live trade",
                          game_id = game_id,
                          required = signal$entry_price)
        }
        return(NULL)
      }

      # Place order
      response <- self$kalshi_client$place_order(
        ticker = ticker,
        side = side,
        action = "buy",
        count = contracts,
        price = signal$entry_price
      )

      if (!is.null(response)) {
        # Record position locally
        self$state$add_position(
          game_id = game_id,
          strategy = strategy,
          signal = signal,
          contracts = contracts
        )

        self$predictor$record_position(game_id, strategy, signal)
      }

      response
    },

    #' Get current status
    get_status = function() {
      list(
        is_running = self$is_running,
        mode = self$mode,
        last_poll = self$last_poll,
        games_today = length(self$games_today),
        live_games = length(self$state$game_states),
        open_positions = nrow(self$state$get_open_positions()),
        balance = self$state$cash_balance,
        total_balance = self$state$get_total_balance()
      )
    },

    #' Update team ratings
    #' @param ratings Tibble with team net ratings
    update_team_ratings = function(ratings) {
      self$team_ratings <- ratings
      if (!is.null(self$logger)) {
        self$logger$info("Team ratings updated",
                        n_teams = nrow(ratings))
      }
    }
  )
)

#' Create an automated trader
#' @param state TradingState
#' @param predictor KalshiPredictor
#' @param logger Logger
#' @param mode "paper" or "live"
#' @param poll_interval Seconds between polls
#' @param use_real_prices Use real Kalshi prices in paper mode (default TRUE)
#' @return AutomatedTrader instance
create_automated_trader <- function(state, predictor, logger = NULL,
                                     mode = "paper", poll_interval = 30,
                                     use_real_prices = TRUE) {
  # Create paper engine if in paper mode
  paper_engine <- NULL
  kalshi_client <- NULL

  if (mode == "paper") {
    # Create kalshi client for real price data (read-only)
    price_client <- NULL
    if (use_real_prices) {
      price_client <- tryCatch({
        client <- create_kalshi_client(logger = logger)
        if (client$is_authenticated()) {
          if (!is.null(logger)) {
            logger$info("Kalshi client connected for real price data")
          }
          client
        } else {
          if (!is.null(logger)) {
            logger$warn("Kalshi API key not configured, using simulated prices")
          }
          NULL
        }
      }, error = function(e) {
        if (!is.null(logger)) {
          logger$warn(sprintf("Could not create Kalshi client: %s", e$message))
        }
        NULL
      })
    }

    paper_engine <- PaperTradingEngine$new(
      state = state,
      logger = logger,
      kalshi_client = price_client
    )
  } else {
    kalshi_client <- create_kalshi_client(logger = logger)
  }

  AutomatedTrader$new(
    state = state,
    predictor = predictor,
    paper_engine = paper_engine,
    kalshi_client = kalshi_client,
    logger = logger,
    poll_interval = poll_interval
  )
}
