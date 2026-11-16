# Tests for R/combo_quote_logger.R with a mock Kalshi client (no network).
# Run from nba_live_betting/:  Rscript tests/test_combo_quote_logger.R
suppressMessages({ library(tidyverse); library(R6); library(httr2); library(jsonlite) })
source("R/kalshi_api.R"); source("R/combo_quote_logger.R")
fails <- 0
check <- function(ok, msg) { cat(sprintf("[%s] %s\n", if (isTRUE(ok)) "PASS" else "FAIL", msg)); if (!isTRUE(ok)) fails <<- fails + 1 }

# No accept/confirm capability anywhere in the client
check(!any(grepl("accept|confirm", names(KalshiClient$public_methods), ignore.case = TRUE)),
      "KalshiClient exposes no accept/confirm methods")

now <- Sys.time()
mk_dec <- function(gid, ev, tk, tip_min, decision = "entered", result = NA_character_)
  tibble(game_id = gid, game_date = "2026-11-15", tipoff = now + tip_min * 60,
         event_ticker = ev, decision = decision, leg_ticker = tk, result = result)
strategy <- list(decisions = bind_rows(
  mk_dec("1", "KXNBASPREAD-26NOV15AAABBB", "KXNBASPREAD-26NOV15AAABBB-AAA6", 50),
  mk_dec("2", "KXNBASPREAD-26NOV15CCCDDD", "KXNBASPREAD-26NOV15CCCDDD-DDD9", 55),
  mk_dec("3", "KXNBASPREAD-26NOV15EEEFFF", "KXNBASPREAD-26NOV15EEEFFF-EEE5", 58, "entry_rejected"),
  mk_dec("4", "KXNBASPREAD-26NOV15GGGHHH", "KXNBASPREAD-26NOV15GGGHHH-GGG7", 2),           # tips too soon
  mk_dec("5", "KXNBASPREAD-26NOV15IIIJJJ", NA, 50, "below_margin_filter")))                # not a leg

calls <- list(create_market = 0, rfq = 0, delete = 0)
asks <- c(0.70, 0.72, 0.68)
mock <- list(
  is_authenticated = function() TRUE,
  get_market = function(tk) {
    i <- match(tk, strategy$decisions$leg_ticker)
    list(market = list(ticker = tk, status = "active",
                       yes_bid_dollars = sprintf("%.4f", asks[i] - 0.01),
                       yes_ask_dollars = sprintf("%.4f", asks[i])))
  },
  get_mve_collections = function() list(
    list(collection_ticker = "KXMVECROSSCATEGORY-R", associated_event_tickers = as.list(strategy$decisions$event_ticker)),
    list(collection_ticker = "KXMVESPORTSMULTIGAMEEXTENDED-R", associated_event_tickers = as.list(strategy$decisions$event_ticker))),
  create_combo_market = function(coll, legs) {
    calls$create_market <<- calls$create_market + 1
    stopifnot(all(map_chr(legs, "side") == "yes"))
    list(market_ticker = paste0("KXMVE-", calls$create_market), event_ticker = "KXMVE")
  },
  create_rfq = function(tk, n) { calls$rfq <<- calls$rfq + 1; list(id = sprintf("rfq-%d", calls$rfq)) },
  get_rfq_quotes = function(id) {
    ts <- format(now + 0.15, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
    if (id == "rfq-2") return(list())  # one combo gets no quotes
    list(list(id = "q1", creator_id = "mmA", yes_bid_dollars = "0.4500", no_bid_dollars = "0.5000", status = "open", created_ts = ts),
         list(id = "q2", creator_id = "mmB", yes_bid_dollars = "0.4400", no_bid_dollars = "0.5100", status = "open", created_ts = ts),
         list(id = "q3", creator_id = "mmC", yes_bid_dollars = "0.4600", no_bid_dollars = "0", status = "open", created_ts = ts))
  },
  delete_rfq = function(id) { calls$delete <<- calls$delete + 1; list(status = 204L) }
)

tmp <- tempfile(fileext = ".rds")
lg <- ComboQuoteLogger$new(mock, strategy, log_path = tmp)
check(is.null(lg$disabled_reason), "enabled with authenticated client")

cands <- lg$candidates(now)
check(length(cands) == 4, sprintf("3 eligible legs -> 3 pairs + 1 triple (got %d)", length(cands)))
check(!any(grepl("GGGHHH|IIIJJJ", map_chr(cands, "key"))), "excludes legs tipping too soon and non-legs")

lg$request_new(now)
check(sum(lg$rfqs$status == "pending") == 4 && calls$rfq == 4, "4 RFQs sent and pending")
check(all(lg$rfqs$collection == "KXMVESPORTSMULTIGAMEEXTENDED-R"), "prefers the multi-game collection")
lg$request_new(now)
check(calls$rfq == 4, "no duplicate RFQs for already-attempted combos")

lg$collect_pending(now + 5)
check(sum(lg$rfqs$status == "pending") == 4, "does not collect before collect_after_sec")
lg$collect_pending(now + 30)
r1 <- lg$rfqs %>% filter(rfq_id == "rfq-1")
check(r1$status == "collected" && r1$n_quotes == 3 && r1$n_makers == 3, "collects all quotes and makers")
check(abs(r1$combo_yes_ask - 0.49) < 1e-9, "combo YES ask = 1 - best NO bid (0.49)")
check(abs(r1$prod_ask - 0.70 * 0.72) < 1e-9, "benchmark = product of live single asks")
check(abs(r1$markup_vs_ask - (0.49 / 0.504 - 1)) < 1e-9, "markup vs product of asks")
check(lg$rfqs$status[lg$rfqs$rfq_id == "rfq-2"] == "no_quotes", "records RFQs with no quotes")
check(calls$delete == 4 && all(lg$rfqs$deleted), "every collected RFQ is deleted")
check(nrow(lg$quotes) == 9, "raw quote rows logged")

# Persistence: a restart reloads and does not re-request
lg2 <- ComboQuoteLogger$new(mock, strategy, log_path = tmp)
check(nrow(lg2$rfqs) == 4 && length(lg2$candidates(now)) == 0, "state reloads from disk; nothing re-requested")

# Failure paths
strategy$decisions$leg_ticker[2] <- "KXNBASPREAD-26NOV15CCCDDD-DDD10"  # new leg on a game (new key)
asks[2] <- 1.00                                                       # no YES offer: not two-sided
lg2$strategy <- strategy; lg2$request_new(now)
check(any(lg2$rfqs$status == "leg_not_quotable"), "skips combos whose legs aren't two-sided")
mock$create_combo_market <- function(coll, legs) list(.error = TRUE, code = "invalid_market_combination")
asks[2] <- 0.72; strategy$decisions$leg_ticker[2] <- "KXNBASPREAD-26NOV15CCCDDD-DDD11"
lg3 <- ComboQuoteLogger$new(mock, strategy, log_path = tempfile())
lg3$request_new(now)
check(all(lg3$rfqs$status == "create_market_failed") && all(lg3$rfqs$error == "invalid_market_combination"),
      "records Kalshi error codes when combo creation is rejected")
mock$create_combo_market <- function(coll, legs) list(market_ticker = "KXMVE-X", event_ticker = "KXMVE")
mock$create_rfq <- function(tk, n) { calls$rfq <<- calls$rfq + 1
  list(.error = TRUE, status = 403L, code = "x", message = "location attestation expired") }
lg4 <- ComboQuoteLogger$new(mock, strategy, log_path = tempfile())
n0 <- calls$rfq; lg4$request_new(now)
check(nrow(lg4$rfqs) == 0 && calls$rfq == n0 + 1, "403 pauses after one RFQ attempt without consuming candidates")
lg4$request_new(now + 60)
check(calls$rfq == n0 + 1, "stays paused during backoff")
check(length(lg4$candidates(now)) == 4, "candidates still available for retry")
mock$is_authenticated <- function() FALSE
check(!is.null(ComboQuoteLogger$new(mock, strategy, log_path = tempfile())$disabled_reason),
      "disables itself without credentials")

# Scoring from the rule's settled leg results
strategy$decisions <- strategy$decisions %>% mutate(leg_ticker = c(
  "KXNBASPREAD-26NOV15AAABBB-AAA6", "KXNBASPREAD-26NOV15CCCDDD-DDD9", "KXNBASPREAD-26NOV15EEEFFF-EEE5",
  "KXNBASPREAD-26NOV15GGGHHH-GGG7", NA), result = c("yes", "yes", "no", NA, NA))
lg$strategy <- strategy
sc <- lg$scored()
ab <- sc %>% filter(rfq_key == paste(sort(strategy$decisions$leg_ticker[1:2]), collapse = "|"))
check(ab$combo_result == "yes" && abs(ab$combo_pnl_pc - (1 - 0.49 - combo_fee(0.49, 10))) < 1e-9,
      "winning combo P&L at quoted ask, net of fee")
check(all(sc$combo_result[grepl("EEE5", sc$leg_tickers)] == "no"), "any losing leg loses the combo")
s <- lg$summary()
check(s$rfqs_collected == 4 && abs(s$quote_rate - 0.75) < 1e-9, "summary: 4 collected, 3 of 4 quoted")

cat(sprintf("\n%s (%d failure%s)\n", if (fails == 0) "ALL PASS" else "FAILURES", fails, if (fails == 1) "" else "s"))
quit(status = if (fails == 0) 0 else 1)
