# Live smoke test: sends ONE real combo RFQ (2 of today's KXNBASPREAD legs), reads quotes, deletes it. Never accepts.
# Run from nba_live_betting/:  Rscript tests/smoke_combo_rfq.R
suppressMessages({ library(tidyverse); library(R6); library(httr2); library(jsonlite) })
source("R/kalshi_api.R")
cl <- create_kalshi_client("config/config.yml")
cat("authenticated:", cl$is_authenticated(), "\n")
colls <- cl$get_mve_collections()
coll <- keep(colls, ~ grepl("MULTIGAME", .x$collection_ticker))[[1]]
evs <- sort(grep("^KXNBASPREAD", unlist(coll$associated_event_tickers), value = TRUE))
# pick, for two events, the strike with ask closest to 0.70 among two-sided quotes on open markets
pick <- map_dfr(evs, function(ev) {
  mk <- cl$public_request(sprintf("/markets?event_ticker=%s&limit=200", ev))$markets
  map_dfr(mk, ~ tibble(ev = ev, tk = .x$ticker, st = .x$status, close = .x$close_time %||% NA,
     bid = extract_kalshi_price(.x, "yes_bid_dollars", "yes_bid"), ask = extract_kalshi_price(.x, "yes_ask_dollars", "yes_ask")))
}) %>% filter(st %in% c("active","open"), bid > 0, ask < 1, ask - bid <= 0.05) %>%
  group_by(ev) %>% slice_min(abs(ask - 0.70), n = 1, with_ties = FALSE) %>% ungroup()
print(pick)
legs <- head(pick, 2)
mk <- cl$create_combo_market(coll$collection_ticker,
  map2(legs$tk, legs$ev, ~ list(market_ticker = .x, event_ticker = .y, side = "yes")))
str(mk)
if (is.null(mk$market_ticker)) quit(status = 1)
rfq <- cl$create_rfq(mk$market_ticker, 10); str(rfq)
if (is.null(rfq$id)) quit(status = 1)
on.exit_delete <- function() { d <- cl$delete_rfq(rfq$id); cat("delete:", d$status %||% d$code, "\n") }
tryCatch({
  Sys.sleep(20)
  qs <- cl$get_rfq_quotes(rfq$id)
  if (isTRUE(qs$.error)) stop(sprintf("get quotes failed: %s", qs$message))
  q <- map_dfr(qs, ~ tibble(maker = substr(.x$creator_id, 1, 8), status = .x$status,
         yes_bid = as.numeric(.x$yes_bid_dollars %||% 0), no_bid = as.numeric(.x$no_bid_dollars %||% 0)))
  print(q, n = 50)
  cat(sprintf("\n%d quotes, %d makers | best YES bid %.3f | combo YES ask (1 - best NO bid) %.3f\n",
      nrow(q), n_distinct(q$maker), max(q$yes_bid), 1 - max(q$no_bid)))
  cat(sprintf("prod single asks %.4f | prod mids %.4f | markup vs ask %+.1f%%, vs mid %+.1f%%\n",
      prod(legs$ask), prod((legs$bid + legs$ask)/2),
      100 * ((1 - max(q$no_bid)) / prod(legs$ask) - 1), 100 * ((1 - max(q$no_bid)) / prod((legs$bid + legs$ask)/2) - 1)))
}, finally = on.exit_delete())
r <- cl$api_request(sprintf("/communications/rfqs/%s", rfq$id)); cat("rfq status after delete:", r$rfq$status, r$rfq$cancellation_reason %||% "", "\n")
