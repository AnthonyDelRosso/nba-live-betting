# Pregame Big-Favorite Spread Strategy (FROZEN RULE — paper only)
#
# Forward test of the sub-pattern found in analyze_favorite_deep.R
# (2026-11-15). The rule is frozen so the forward sample is an honest
# out-of-sample test — do NOT tune these constants on forward results.
#
#   Market:  KXNBASPREAD (full-game spread ladder)
#   When:    once per game, first poll at <= 60 min before scheduled tip
#            (backtest snapshot was the quote nearest tip-60min). If the
#            first look is already < 45 min out, the game is logged as
#            "missed_window" and skipped rather than entered off-rule.
#   Quotes:  only strikes with a genuine two-sided quote: bid > 0, ask < 1,
#            ask - bid <= 0.15 (same filter as research_kalshi_calibration_v2)
#   Favorite: the team with strikes whose YES ask is in [0.60, 0.92]
#   Filter:  market-implied median favorite margin >= 8 pts, where the
#            implied median is where the favorite's (monotone-enforced) mid
#            survival curve P(margin > k) crosses 0.50
#   Leg:     buy YES on the favorite strike whose ask is closest to 0.70
#   Size:    fixed contract count (CONFIG$pregame_fav_contracts, default 10)
#   Settle:  on the Kalshi market's own result (NOT the moneyline winner)
#
# Every evaluated game is recorded (entered or not, with the reason) in
# data/pregame_fav_log.rds so the forward test can be scored per game.

library(R6)
library(tidyverse)

# Strategies whose positions must NOT be settled by the moneyline winner.
SPREAD_SETTLED_STRATEGIES <- c("pregame_big_fav")

PREGAME_FAV_RULE <- list(
  strategy_name   = "pregame_big_fav",
  series          = "KXNBASPREAD",
  entry_max_min   = 60,     # enter on first poll at or inside 60 min to tip
  entry_min_min   = 45,     # ...but not if we first see it inside 45 min
  price_lo        = 0.60,
  price_hi        = 0.92,
  target_price    = 0.70,
  min_implied_margin = 8,
  max_spread      = 0.15,
  settle_check_sec = 300    # check open-position settlement every 5 min
)

PregameFavStrategy <- R6::R6Class(
  "PregameFavStrategy",

  public = list(
    state = NULL,
    client = NULL,           # KalshiClient (public reads only)
    logger = NULL,
    contracts = NULL,
    kalshi_fee = NULL,
    log_path = NULL,
    decisions = NULL,        # one row per evaluated game
    last_settle_check = NULL,

    initialize = function(state, client, logger = NULL, contracts = 10,
                          kalshi_fee = 0.07, log_path = NULL) {
      self$state <- state
      self$client <- client
      self$logger <- logger
      self$contracts <- as.integer(contracts)
      self$kalshi_fee <- kalshi_fee
      self$log_path <- log_path %||% if (exists("DATA_DIR")) {
        file.path(DATA_DIR, "pregame_fav_log.rds")
      } else "data/pregame_fav_log.rds"
      self$decisions <- if (file.exists(self$log_path)) readRDS(self$log_path) else
        private$empty_decisions()
      self$last_settle_check <- as.POSIXct(0, origin = "1970-01-01")
    },

    #' Called once per trader poll with today's ESPN scoreboard games
    run = function(games) {
      if (is.null(self$client)) return(invisible(NULL))
      if (!identical(self$state$mode, "paper")) return(invisible(NULL))  # paper only

      for (g in games %||% list()) {
        tryCatch(self$evaluate_game(g), error = function(e) {
          private$log("warn", sprintf("[PREGAME_FAV] evaluate error for %s: %s",
                                      g$game_id %||% "?", e$message))
        })
      }

      if (as.numeric(difftime(Sys.time(), self$last_settle_check, units = "secs")) >=
          PREGAME_FAV_RULE$settle_check_sec) {
        self$last_settle_check <- Sys.time()
        tryCatch(self$check_settlements(), error = function(e) {
          private$log("warn", sprintf("[PREGAME_FAV] settlement error: %s", e$message))
        })
      }
      invisible(NULL)
    },

    evaluate_game = function(g) {
      game_id <- as.character(g$game_id %||% NA)
      if (is.na(game_id) || game_id %in% self$decisions$game_id) return(invisible(NULL))
      if (!isTRUE(g$is_scheduled)) return(invisible(NULL))

      tip <- private$parse_espn_ts(g$start_time)
      if (is.na(tip)) return(invisible(NULL))
      mins_to_tip <- as.numeric(difftime(tip, Sys.time(), units = "mins"))
      if (mins_to_tip > PREGAME_FAV_RULE$entry_max_min) return(invisible(NULL))  # too early

      away <- normalize_team_abbrev(g$away_team_abbrev %||% "")
      home <- normalize_team_abbrev(g$home_team_abbrev %||% "")
      base <- list(game_id = game_id, game_date = format(tip, "%Y-%m-%d", tz = "America/New_York"),
                   away_team = away, home_team = home, tipoff = tip,
                   eval_time = Sys.time(), mins_to_tip = mins_to_tip)

      if (mins_to_tip < PREGAME_FAV_RULE$entry_min_min) {
        # Distinguish "we polled inside the window but Kalshi never listed a
        # quotable ladder" from "the loop never looked at this game in time".
        base$event_ticker <- private$event_ticker(tip, away, home)
        decision <- if (game_id %in% private$no_ladder_seen) "no_market_by_window" else "missed_window"
        private$no_ladder_seen <- setdiff(private$no_ladder_seen, game_id)
        return(private$record(base, decision = decision))
      }

      event_ticker <- private$event_ticker(tip, away, home)
      ladder <- self$fetch_ladder(event_ticker)
      base$event_ticker <- event_ticker
      if (is.null(ladder) || nrow(ladder) == 0) {
        # Market may not be listed yet — retry on later polls while still in window.
        private$no_ladder_seen <- union(private$no_ladder_seen, game_id)
        private$log("info", sprintf("[PREGAME_FAV] %s: no quotable ladder yet", event_ticker))
        return(invisible(NULL))
      }

      a <- self$assess(ladder)
      base$fav_team <- a$fav_team
      base$implied_median <- a$implied_median
      if (a$decision != "qualifies") return(private$record(base, decision = a$decision))
      leg <- a$leg

      signal <- list(signal = "BUY_YES", entry_price = leg$ask, bet_size = 0,
                     edge = 0, lead = 0, period = 0)
      position_id <- self$state$add_position(game_id, PREGAME_FAV_RULE$strategy_name,
                                             signal, self$contracts)
      if (is.null(position_id)) {
        return(private$record(base, decision = "entry_rejected", leg = leg))
      }
      private$log("info", sprintf(
        "[PREGAME_FAV] ENTER %s: BUY YES %s (%s > %.1f) x%d @ %.2f | implied fav margin %.1f | %.0f min to tip",
        event_ticker, leg$ticker, a$fav_team, leg$threshold, self$contracts, leg$ask,
        base$implied_median, mins_to_tip))
      private$record(base, decision = "entered", leg = leg, position_id = position_id)
    },

    #' Apply the frozen rule to a quotable ladder. Single source of truth for
    #' both live entries (evaluate_game) and the dashboard preview.
    #' @return list(decision = qualifies|no_fav_legs|ambiguous_fav|below_margin_filter,
    #'              fav_team, implied_median, leg)
    assess = function(ladder) {
      band <- ladder %>% filter(ask >= PREGAME_FAV_RULE$price_lo, ask <= PREGAME_FAV_RULE$price_hi)
      fav_teams <- unique(band$team)
      if (length(fav_teams) != 1) {
        return(list(decision = if (length(fav_teams) == 0) "no_fav_legs" else "ambiguous_fav",
                    fav_team = NULL, implied_median = NULL, leg = NULL))
      }
      fav <- fav_teams
      imp <- private$implied_median(filter(ladder, team == fav))
      if (is.na(imp) || imp < PREGAME_FAV_RULE$min_implied_margin) {
        return(list(decision = "below_margin_filter", fav_team = fav, implied_median = imp, leg = NULL))
      }
      leg <- band %>% filter(team == fav) %>% arrange(threshold) %>%
        slice(which.min(abs(ask - PREGAME_FAV_RULE$target_price)))
      list(decision = "qualifies", fav_team = fav, implied_median = imp, leg = leg)
    },

    #' Dashboard view of a slate: recorded decisions for evaluated games, and
    #' for not-yet-evaluated scheduled games a live read of where the rule
    #' currently stands (cached; quotes can still move before the window).
    preview = function(games, ttl_sec = 300) {
      if (!is.null(private$preview_cache) &&
          as.numeric(difftime(Sys.time(), private$preview_time, units = "secs")) < ttl_sec &&
          identical(private$preview_ids, sort(vapply(games, function(g) as.character(g$game_id %||% ""), "")))) {
        live <- private$preview_cache
      } else {
        live <- map_dfr(games, function(g) {
          gid <- as.character(g$game_id %||% NA)
          if (is.na(gid) || gid %in% self$decisions$game_id || !isTRUE(g$is_scheduled)) return(NULL)
          tip <- private$parse_espn_ts(g$start_time)
          if (is.na(tip)) return(NULL)
          lad <- tryCatch(self$fetch_ladder(private$event_ticker(
            tip, normalize_team_abbrev(g$away_team_abbrev %||% ""),
            normalize_team_abbrev(g$home_team_abbrev %||% ""))), error = function(e) NULL)
          if (is.null(lad) || nrow(lad) == 0) return(tibble(game_id = gid, now_decision = "no_market_yet"))
          a <- self$assess(lad)
          tibble(game_id = gid, now_decision = a$decision, now_fav = a$fav_team %||% NA_character_,
                 now_implied = a$implied_median %||% NA_real_,
                 now_leg = if (!is.null(a$leg)) sprintf("%s >%.1f @ %.2f", a$leg$team, a$leg$threshold, a$leg$ask) else NA_character_)
        })
        private$preview_cache <- live
        private$preview_time <- Sys.time()
        private$preview_ids <- sort(vapply(games, function(g) as.character(g$game_id %||% ""), ""))
      }
      live
    },

    #' Pull the event's strike ladder with two-sided quotes
    fetch_ladder = function(event_ticker) {
      resp <- self$client$public_request(sprintf("/markets?event_ticker=%s&limit=200", event_ticker))
      mk <- resp$markets %||% list()
      if (length(mk) == 0) return(NULL)
      map_dfr(mk, function(m) {
        tibble(
          ticker = m$ticker %||% NA_character_,
          status = m$status %||% NA_character_,
          team = str_extract(m$ticker %||% "", "(?<=-)[A-Z]+(?=\\d+$)"),
          threshold = suppressWarnings(as.numeric(regmatches(m$title %||% "",
                                                  regexpr("[0-9]+\\.[0-9]+", m$title %||% "")))[1]),
          bid = extract_kalshi_price(m, "yes_bid_dollars", "yes_bid"),
          ask = extract_kalshi_price(m, "yes_ask_dollars", "yes_ask"),
          ask_size = suppressWarnings(as.numeric(m$yes_ask_size_fp %||% NA)),
          volume = suppressWarnings(as.numeric(m$volume_fp %||% m$volume %||% NA))
        )
      }) %>%
        filter(status %in% c("active", "open"), !is.na(team), !is.na(threshold),
               !is.na(bid), !is.na(ask), bid > 0, ask < 1,
               ask - bid <= PREGAME_FAV_RULE$max_spread) %>%
        mutate(mid = (bid + ask) / 2)
    },

    #' Settle open positions from each market's own Kalshi result
    check_settlements = function() {
      open_rows <- which(self$decisions$decision == "entered" & is.na(self$decisions$result))
      for (i in open_rows) {
        tk <- self$decisions$leg_ticker[i]
        m <- self$client$get_market(tk)
        m <- m$market %||% m
        res <- tolower(m$result %||% "")
        status <- tolower(m$status %||% "")
        pid <- self$decisions$position_id[i]
        # Voided / cancelled market (e.g. postponed game): Kalshi resolves it
        # with no yes/no result. Refund at cost instead of leaving it open forever.
        if (res %in% c("void", "voided", "cancelled", "canceled") ||
            (status %in% c("settled", "finalized") && !res %in% c("yes", "no"))) {
          self$state$void_position(pid, reason = sprintf("Kalshi result '%s', status '%s'", res, status))
          self$decisions$result[i] <- "void"
          self$decisions$pnl[i] <- 0
          private$log("info", sprintf("[PREGAME_FAV] VOID %s: result '%s' status '%s', refunded at cost",
                                      tk, res, status))
          next
        }
        if (!res %in% c("yes", "no")) next
        pnl <- self$state$settle_position(pid, won = res == "yes", kalshi_fee = self$kalshi_fee)
        self$decisions$result[i] <- res
        self$decisions$pnl[i] <- as.numeric(pnl %||% NA)
        private$log("info", sprintf("[PREGAME_FAV] SETTLE %s: %s, P&L $%.2f", tk, toupper(res),
                                    as.numeric(pnl %||% NA)))
      }
      if (length(open_rows) > 0) private$save()
    },

    #' Forward-test scorecard (one row per entered game)
    summary = function() {
      d <- self$decisions %>% filter(decision == "entered", result %in% c("yes", "no"))
      list(evaluated = nrow(self$decisions),
           entered = sum(self$decisions$decision == "entered"),
           settled = nrow(d),
           voided = sum(self$decisions$decision == "entered" & self$decisions$result %in% "void"),
           win_rate = if (nrow(d)) mean(d$result == "yes") else NA,
           mean_entry = if (nrow(d)) mean(d$leg_ask) else NA,
           total_pnl = sum(d$pnl, na.rm = TRUE),
           return_pct = if (nrow(d)) 100 * sum(d$pnl) / sum(d$leg_ask * d$contracts) else NA)
    }
  ),

  private = list(
    preview_cache = NULL, preview_time = NULL, preview_ids = NULL,
    no_ladder_seen = character(),  # game_ids polled in-window with no quotable ladder

    event_ticker = function(tip, away, home) {
      sprintf("%s-%s%s%s", PREGAME_FAV_RULE$series,
              toupper(format(tip, "%y%b%d", tz = "America/New_York")), away, home)
    },

    empty_decisions = function() tibble(
      game_id = character(), game_date = character(), away_team = character(),
      home_team = character(), tipoff = as.POSIXct(character()), eval_time = as.POSIXct(character()),
      mins_to_tip = numeric(), event_ticker = character(), decision = character(),
      fav_team = character(), implied_median = numeric(), leg_ticker = character(),
      leg_threshold = numeric(), leg_bid = numeric(), leg_ask = numeric(),
      leg_ask_size = numeric(), contracts = integer(), position_id = character(),
      result = character(), pnl = numeric()),

    record = function(base, decision, leg = NULL, position_id = NA_character_) {
      row <- tibble(
        game_id = base$game_id, game_date = base$game_date, away_team = base$away_team,
        home_team = base$home_team, tipoff = base$tipoff, eval_time = base$eval_time,
        mins_to_tip = base$mins_to_tip, event_ticker = base$event_ticker %||% NA_character_,
        decision = decision, fav_team = base$fav_team %||% NA_character_,
        implied_median = base$implied_median %||% NA_real_,
        leg_ticker = leg$ticker %||% NA_character_, leg_threshold = leg$threshold %||% NA_real_,
        leg_bid = leg$bid %||% NA_real_, leg_ask = leg$ask %||% NA_real_,
        leg_ask_size = leg$ask_size %||% NA_real_,
        contracts = if (decision == "entered") self$contracts else NA_integer_,
        position_id = position_id, result = NA_character_, pnl = NA_real_)
      self$decisions <- bind_rows(self$decisions, row)
      if (decision != "entered") {
        private$log("info", sprintf("[PREGAME_FAV] %s %s@%s: %s%s", base$game_date, base$away_team,
                                    base$home_team, decision,
                                    if (!is.null(base$implied_median) && !is.na(base$implied_median))
                                      sprintf(" (implied fav margin %.1f)", base$implied_median) else ""))
      }
      private$save()
      invisible(row)
    },

    # Same construction as analyze_favorite_deep.R: monotone-enforced mid
    # survival curve, linear interpolation at 0.5, NA if it never crosses.
    implied_median = function(team_ladder) {
      team_ladder <- arrange(team_ladder, threshold)
      p <- cummin(team_ladder$mid); k <- team_ladder$threshold
      if (length(p) < 2 || max(p) < 0.5 || min(p) > 0.5) return(NA_real_)
      approx(p, k, xout = 0.5, ties = mean)$y
    },

    parse_espn_ts = function(ts) {
      if (is.null(ts) || is.na(ts) || !nzchar(ts)) return(as.POSIXct(NA))
      v <- as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      if (is.na(v)) v <- as.POSIXct(ts, format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
      v
    },

    save = function() tryCatch(saveRDS(self$decisions, self$log_path), error = function(e) NULL),

    log = function(level, msg) {
      message(msg)
      if (!is.null(self$logger)) self$logger[[level]](msg)
    }
  )
)
