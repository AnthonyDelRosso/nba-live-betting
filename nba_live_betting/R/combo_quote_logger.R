# Combo RFQ Quote Logger (paper only, QUOTE DISCOVERY — NEVER TRADES)
#
# Question it answers: when 2+ games on a slate pass the frozen pregame
# big-favorite rule (R/pregame_fav_strategy.R), what do Kalshi combo market
# makers charge for the parlay of those legs, relative to the product of the
# single-leg prices at the same moment? That markup decides whether combos
# can ever beat placing the singles side by side (see the 2026-11-15 combo
# analysis: at the in-sample edge a small combo allocation helps; at any
# smaller edge the growth-optimal combo stake is zero, and markup makes it worse).
#
# Flow, per trader poll:
#   1. Eligible legs = the rule's qualifying legs (decision entered or
#      entry_rejected) on the same game_date whose games tip > min_mins_to_tip
#      from now. Every not-yet-quoted 2..max_legs subset becomes a candidate.
#   2. Re-read each leg's live bid/ask (the leg was chosen up to an hour ago),
#      create/fetch the combo market in the multi-game collection, send an RFQ
#      for `contracts` contracts with rest_remainder = FALSE.
#   3. On a later poll (>= collect_after_sec), read every quote received,
#      log it, then DELETE the RFQ. Nothing is ever accepted or confirmed —
#      KalshiClient has no method that could do so.
#
# Pricing convention: a maker's quote is (yes_bid, no_bid). Buying the combo
# YES means trading against a maker's NO bid, so our YES ask = 1 - best no_bid
# (same as the order book, where yes ask = 1 - best no bid).
#
# Output: data/combo_quote_log.rds = list(rfqs = one row per combo attempt,
#                                         quotes = one row per maker quote)
#
# This does not touch the frozen rule: it only reads its decisions.

library(R6)
library(tidyverse)

COMBO_LOGGER_CFG <- list(
  max_legs          = 3,
  contracts         = 10,
  min_mins_to_tip   = 5,     # don't quote combos whose earliest leg tips sooner
  collect_after_sec = 20,    # wait at least this long for quotes before reading
  max_rfqs_per_date = 15,    # cap combo attempts per slate
  max_pending       = 5,     # cap concurrently open RFQs
  max_leg_spread    = 0.15,  # same two-sided filter as the rule
  collection_ttl_sec = 600,
  preferred_collection = "MULTIGAME"
)

combo_fee <- function(price, contracts) {
  # Kalshi taker schedule ceil(0.07 * C * P * (1 - P)) in cents, as $/contract
  ceiling(0.07 * contracts * price * (1 - price) * 100) / 100 / contracts
}

ComboQuoteLogger <- R6::R6Class(
  "ComboQuoteLogger",

  public = list(
    client = NULL,
    strategy = NULL,          # PregameFavStrategy (read-only use of $decisions)
    logger = NULL,
    cfg = NULL,
    log_path = NULL,
    rfqs = NULL,
    quotes = NULL,
    disabled_reason = NULL,

    initialize = function(client, strategy, logger = NULL, cfg = list(), log_path = NULL) {
      self$client <- client
      self$strategy <- strategy
      self$logger <- logger
      self$cfg <- modifyList(COMBO_LOGGER_CFG, cfg)
      self$log_path <- log_path %||% if (exists("DATA_DIR")) {
        file.path(DATA_DIR, "combo_quote_log.rds")
      } else "data/combo_quote_log.rds"
      saved <- if (file.exists(self$log_path)) tryCatch(readRDS(self$log_path), error = function(e) NULL)
      self$rfqs <- saved$rfqs %||% private$empty_rfqs()
      self$quotes <- saved$quotes %||% private$empty_quotes()
      if (!isTRUE(tryCatch(client$is_authenticated(), error = function(e) FALSE))) {
        self$disabled_reason <- "Kalshi client not authenticated (RFQs need API credentials)"
        private$log("warn", sprintf("[COMBO_RFQ] disabled: %s", self$disabled_reason))
      }
    },

    run = function() {
      if (!is.null(self$disabled_reason)) return(invisible(NULL))
      tryCatch(self$collect_pending(), error = function(e)
        private$log("warn", sprintf("[COMBO_RFQ] collect error: %s", e$message)))
      tryCatch(self$request_new(), error = function(e)
        private$log("warn", sprintf("[COMBO_RFQ] request error: %s", e$message)))
      invisible(NULL)
    },

    #' Qualifying rule legs that can still be combined (not yet tipped)
    eligible_legs = function(now = Sys.time()) {
      d <- self$strategy$decisions
      if (is.null(d) || nrow(d) == 0) return(d)
      d %>%
        filter(decision %in% c("entered", "entry_rejected"), !is.na(leg_ticker),
               as.numeric(difftime(tipoff, now, units = "mins")) > self$cfg$min_mins_to_tip) %>%
        distinct(leg_ticker, .keep_all = TRUE) %>%
        arrange(tipoff)
    },

    #' Unquoted leg subsets, grouped by slate
    candidates = function(now = Sys.time()) {
      legs <- self$eligible_legs(now)
      if (is.null(legs) || nrow(legs) < 2) return(list())
      out <- list()
      for (gd in unique(legs$game_date)) {
        L <- legs[legs$game_date == gd, ]
        if (nrow(L) < 2) next
        for (k in 2:min(self$cfg$max_legs, nrow(L))) {
          for (idx in asplit(combn(nrow(L), k), 2)) {
            key <- paste(sort(L$leg_ticker[idx]), collapse = "|")
            if (key %in% self$rfqs$rfq_key) next
            out[[length(out) + 1]] <- list(key = key, game_date = gd, legs = L[idx, ])
          }
        }
      }
      out
    },

    request_new = function(now = Sys.time()) {
      if (!is.null(private$blocked_until) && now < private$blocked_until) return(invisible(NULL))
      cands <- self$candidates(now)
      for (cand in cands) {
        if (!is.null(private$blocked_until) && Sys.time() < private$blocked_until) break
        n_pending <- sum(self$rfqs$status == "pending")
        n_today <- sum(self$rfqs$game_date == cand$game_date)
        if (n_pending >= self$cfg$max_pending || n_today >= self$cfg$max_rfqs_per_date) break
        private$request_one(cand)
      }
    },

    collect_pending = function(now = Sys.time()) {
      for (i in which(self$rfqs$status == "pending")) {
        age <- as.numeric(difftime(now, self$rfqs$created_at[i], units = "secs"))
        if (age < self$cfg$collect_after_sec) next
        rfq_id <- self$rfqs$rfq_id[i]
        qs <- self$client$get_rfq_quotes(rfq_id)
        if (isTRUE(qs$.error)) {
          # Give up after the RFQ would have auto-closed anyway (~3h)
          if (age > 3 * 3600) { self$rfqs$status[i] <- "collect_failed"; self$rfqs$error[i] <- qs$code }
          next
        }
        qdf <- private$parse_quotes(qs, rfq_id, self$rfqs$created_at[i])
        self$quotes <- bind_rows(self$quotes, qdf)

        yes_bids <- qdf$yes_bid[qdf$yes_bid > 0]
        no_bids <- qdf$no_bid[qdf$no_bid > 0]
        self$rfqs$collected_at[i] <- now
        self$rfqs$n_quotes[i] <- nrow(qdf)
        self$rfqs$n_makers[i] <- n_distinct(qdf$creator_id)
        self$rfqs$best_yes_bid[i] <- if (length(yes_bids)) max(yes_bids) else NA_real_
        self$rfqs$best_no_bid[i] <- if (length(no_bids)) max(no_bids) else NA_real_
        self$rfqs$combo_yes_ask[i] <- if (length(no_bids)) 1 - max(no_bids) else NA_real_
        self$rfqs$first_quote_ms[i] <- if (nrow(qdf)) min(qdf$latency_ms, na.rm = TRUE) else NA_real_
        self$rfqs$markup_vs_ask[i] <- self$rfqs$combo_yes_ask[i] / self$rfqs$prod_ask[i] - 1
        self$rfqs$markup_vs_mid[i] <- self$rfqs$combo_yes_ask[i] / self$rfqs$prod_mid[i] - 1
        self$rfqs$status[i] <- if (nrow(qdf)) "collected" else "no_quotes"

        del <- self$client$delete_rfq(rfq_id)
        self$rfqs$deleted[i] <- !isTRUE(del$.error)

        private$log("info", sprintf(
          "[COMBO_RFQ] %s %d-leg: %d quotes / %d makers | combo YES ask %s vs prod ask %.3f (mid %.3f) | markup %s",
          self$rfqs$game_date[i], self$rfqs$n_legs[i], nrow(qdf), self$rfqs$n_makers[i],
          private$fmt(self$rfqs$combo_yes_ask[i]), self$rfqs$prod_ask[i], self$rfqs$prod_mid[i],
          ifelse(is.na(self$rfqs$markup_vs_ask[i]), "n/a",
                 sprintf("%+.1f%% vs ask, %+.1f%% vs mid", 100 * self$rfqs$markup_vs_ask[i],
                         100 * self$rfqs$markup_vs_mid[i]))))
      }
      private$save()
    },

    #' Combo attempts joined to leg results (from the rule's own settlement)
    scored = function() {
      if (nrow(self$rfqs) == 0) return(self$rfqs)
      res <- self$strategy$decisions %>% filter(!is.na(leg_ticker)) %>%
        distinct(leg_ticker, .keep_all = TRUE) %>% select(leg_ticker, result)
      self$rfqs %>%
        mutate(leg_results = map_chr(strsplit(leg_tickers, ","), function(tk) {
          r <- res$result[match(tk, res$leg_ticker)]
          paste(ifelse(is.na(r), "open", r), collapse = ",")
        }),
        combo_result = map_chr(strsplit(leg_results, ","), function(r) {
          if (any(r == "no")) "no" else if (any(r == "void")) "void"
          else if (all(r == "yes")) "yes" else "open"
        }),
        fee_pc = ifelse(is.na(combo_yes_ask), NA_real_, combo_fee(combo_yes_ask, contracts)),
        # Hypothetical P&L per contract had we bought the combo at its quoted ask
        combo_pnl_pc = case_when(combo_result == "yes" ~ 1 - combo_yes_ask - fee_pc,
                                 combo_result == "no"  ~ -combo_yes_ask - fee_pc,
                                 TRUE ~ NA_real_))
    },

    summary = function() {
      s <- self$scored()
      q <- s %>% filter(status %in% c("collected", "no_quotes"))
      p <- q %>% filter(!is.na(combo_yes_ask))
      st <- p %>% filter(combo_result %in% c("yes", "no"))
      list(attempts = nrow(s),
           failures = sum(!s$status %in% c("collected", "no_quotes", "pending")),
           rfqs_collected = nrow(q),
           quote_rate = if (nrow(q)) nrow(p) / nrow(q) else NA,
           by_legs = p %>% group_by(n_legs) %>%
             summarise(n = n(), median_markup_vs_ask = median(markup_vs_ask),
                       median_markup_vs_mid = median(markup_vs_mid),
                       share_below_prod_ask = mean(combo_yes_ask < prod_ask),
                       median_makers = median(n_makers), .groups = "drop"),
           settled = nrow(st),
           settled_hit_rate = if (nrow(st)) mean(st$combo_result == "yes") else NA,
           settled_mean_price = if (nrow(st)) mean(st$combo_yes_ask) else NA,
           settled_roi = if (nrow(st)) sum(st$combo_pnl_pc) / sum(st$combo_yes_ask) else NA)
    }
  ),

  private = list(
    collections = NULL, collections_time = NULL,
    blocked_until = NULL,

    # 401/403 (bad creds, expired Kalshi location attestation) are account
    # problems, not combo outcomes: pause and retry later instead of recording
    # the candidate as attempted.
    auth_blocked = function(resp) {
      if (!isTRUE(resp$.error) || !isTRUE(resp$status %in% c(401L, 403L))) return(FALSE)
      private$blocked_until <- Sys.time() + 15 * 60
      private$log("warn", sprintf("[COMBO_RFQ] HTTP %d, pausing 15 min: %s", resp$status,
                                  resp$message %||% resp$code %||% ""))
      TRUE
    },

    request_one = function(cand) {
      legs <- cand$legs
      row <- list(rfq_key = cand$key, game_date = cand$game_date, n_legs = nrow(legs),
                  leg_tickers = paste(legs$leg_ticker, collapse = ","),
                  contracts = as.integer(self$cfg$contracts), created_at = Sys.time())

      # Fresh single-leg quotes at RFQ time: the benchmark the combo is priced against
      px <- map_dfr(legs$leg_ticker, function(tk) {
        m <- self$client$get_market(tk); m <- m$market %||% m
        tibble(ticker = tk, status = m$status %||% NA_character_,
               bid = extract_kalshi_price(m, "yes_bid_dollars", "yes_bid"),
               ask = extract_kalshi_price(m, "yes_ask_dollars", "yes_ask"))
      })
      row$leg_bids <- paste(sprintf("%.2f", px$bid), collapse = ",")
      row$leg_asks <- paste(sprintf("%.2f", px$ask), collapse = ",")
      ok <- px$status %in% c("active", "open") & !is.na(px$bid) & !is.na(px$ask) &
        px$bid > 0 & px$ask < 1 & (px$ask - px$bid) <= self$cfg$max_leg_spread
      if (!all(ok)) {
        return(private$add_rfq(row, status = "leg_not_quotable",
                               error = paste(px$ticker[!ok], collapse = ",")))
      }
      row$prod_ask <- prod(px$ask); row$prod_bid <- prod(px$bid)
      row$prod_mid <- prod((px$bid + px$ask) / 2)

      coll <- private$find_collection(legs$event_ticker)
      if (is.null(coll)) return(private$add_rfq(row, status = "no_collection"))
      row$collection <- coll

      sel <- map2(legs$leg_ticker, legs$event_ticker,
                  ~ list(market_ticker = .x, event_ticker = .y, side = "yes"))
      mk <- self$client$create_combo_market(coll, sel)
      if (private$auth_blocked(mk)) return(invisible(NULL))
      if (is.null(mk) || isTRUE(mk$.error) || is.null(mk$market_ticker)) {
        return(private$add_rfq(row, status = "create_market_failed", error = mk$code %||% "null response"))
      }
      row$combo_ticker <- mk$market_ticker

      rfq <- self$client$create_rfq(mk$market_ticker, self$cfg$contracts)
      if (private$auth_blocked(rfq)) return(invisible(NULL))
      if (is.null(rfq) || isTRUE(rfq$.error) || is.null(rfq$id)) {
        return(private$add_rfq(row, status = "rfq_failed", error = rfq$code %||% "null response"))
      }
      row$rfq_id <- rfq$id
      row$created_at <- Sys.time()
      private$log("info", sprintf("[COMBO_RFQ] RFQ %s: %d-leg %s | single asks %s (prod %.3f)",
                                  rfq$id, nrow(legs), row$leg_tickers, row$leg_asks, row$prod_ask))
      private$add_rfq(row, status = "pending")
    },

    find_collection = function(event_tickers) {
      stale <- is.null(private$collections) ||
        as.numeric(difftime(Sys.time(), private$collections_time, units = "secs")) > self$cfg$collection_ttl_sec
      if (stale) {
        private$collections <- self$client$get_mve_collections()
        private$collections_time <- Sys.time()
      }
      fits <- keep(private$collections, function(c)
        all(event_tickers %in% unlist(c$associated_event_tickers)))
      if (length(fits) == 0) {
        private$collections <- NULL  # event may have just been added; refetch next time
        return(NULL)
      }
      tk <- map_chr(fits, ~ .x$collection_ticker %||% NA_character_)
      pref <- grepl(self$cfg$preferred_collection, tk)
      if (any(pref)) tk[pref][1] else tk[1]
    },

    parse_quotes = function(qs, rfq_id, created_at) {
      if (length(qs) == 0) return(private$empty_quotes())
      map_dfr(qs, function(q) {
        cts <- as.POSIXct(q$created_ts %||% NA_character_, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
        tibble(rfq_id = rfq_id, quote_id = q$id %||% NA_character_,
               creator_id = q$creator_id %||% NA_character_,
               yes_bid = suppressWarnings(as.numeric(q$yes_bid_dollars %||% NA)),
               no_bid = suppressWarnings(as.numeric(q$no_bid_dollars %||% NA)),
               status = q$status %||% NA_character_,
               cancellation_reason = q$cancellation_reason %||% NA_character_,
               created_ts = cts,
               latency_ms = as.numeric(difftime(cts, created_at, units = "secs")) * 1000)
      }) %>% mutate(yes_bid = coalesce(yes_bid, 0), no_bid = coalesce(no_bid, 0))
    },

    add_rfq = function(row, status, error = NA_character_) {
      row$status <- status; row$error <- error
      self$rfqs <- bind_rows(self$rfqs, as_tibble(row))
      if (status != "pending") {
        private$log("info", sprintf("[COMBO_RFQ] %s: %s%s", row$leg_tickers, status,
                                    if (!is.na(error)) sprintf(" (%s)", error) else ""))
      }
      private$save()
      invisible(NULL)
    },

    empty_rfqs = function() tibble(
      rfq_key = character(), game_date = character(), n_legs = integer(),
      leg_tickers = character(), leg_bids = character(), leg_asks = character(),
      prod_ask = numeric(), prod_bid = numeric(), prod_mid = numeric(),
      collection = character(), combo_ticker = character(), rfq_id = character(),
      contracts = integer(), created_at = as.POSIXct(character()),
      collected_at = as.POSIXct(character()), status = character(), error = character(),
      n_quotes = integer(), n_makers = integer(), best_yes_bid = numeric(),
      best_no_bid = numeric(), combo_yes_ask = numeric(), first_quote_ms = numeric(),
      markup_vs_ask = numeric(), markup_vs_mid = numeric(), deleted = logical()),

    empty_quotes = function() tibble(
      rfq_id = character(), quote_id = character(), creator_id = character(),
      yes_bid = numeric(), no_bid = numeric(), status = character(),
      cancellation_reason = character(), created_ts = as.POSIXct(character()),
      latency_ms = numeric()),

    fmt = function(x) if (is.na(x)) "none" else sprintf("%.3f", x),

    save = function() tryCatch(saveRDS(list(rfqs = self$rfqs, quotes = self$quotes), self$log_path),
                               error = function(e) NULL),

    log = function(level, msg) {
      message(msg)
      if (!is.null(self$logger)) self$logger[[level]](msg)
    }
  )
)
