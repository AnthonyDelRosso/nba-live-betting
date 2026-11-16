# Limit-order fill-rate backtest: instead of crossing the spread (buying NO
# at the ask = 1-yes_bid), rest a passive limit order somewhere between the
# current yes_bid and yes_ask (equivalently: a resting sell-YES order) and
# check whether subsequent trading ever reaches that level before the
# market closes. A resting sell-YES @ P fills iff some later YES trade
# prints at or above P (sufficient buying pressure reached that level).
# Unfilled legs contribute zero P&L and zero risk — not a loss, just no trade.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) setwd(args[1])

library(httr2); library(jsonlite); library(tidyverse)

SERIES_TICKER <- if (length(args) >= 2) args[2] else "KXNBA1QSPREAD"
OUT_FILE <- sprintf("limit_fills_%s.rds", SERIES_TICKER)

parse_kalshi_ts <- function(ts) {
  if (is.null(ts) || is.na(ts) || !nzchar(ts)) return(NA_integer_)
  as.integer(as.POSIXct(sub("\\.[0-9]+Z$", "Z", ts), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
}

fetch_candlesticks_retry <- function(ticker, open_ts, close_ts, tries = 4) {
  for (attempt in seq_len(tries)) {
    resp <- tryCatch(
      request(sprintf("https://api.elections.kalshi.com/trade-api/v2/series/%s/markets/%s/candlesticks", SERIES_TICKER, ticker)) %>%
        req_url_query(start_ts = open_ts, end_ts = close_ts, period_interval = 1) %>%
        req_perform() %>% resp_body_json(),
      error = function(e) NULL)
    if (!is.null(resp)) return(resp)
    Sys.sleep(0.5 * attempt)
  }
  NULL
}

# Scope: the same wing legs already analyzed (threshold >= 10, pregame_fixed
# snapshot available) from the existing corrected pull.
base <- readRDS(sprintf("calibration_v2_%s.rds", SERIES_TICKER)) %>%
  filter(!is.na(snapshot_price), !is.na(result), threshold >= 10,
         snapshot_method == "pregame_fixed", !is.na(minutes_from_open))
cat(sprintf("Testing limit fills for %d legs...\n", nrow(base)))

results <- vector("list", nrow(base))
for (i in seq_len(nrow(base))) {
  row <- base[i, ]
  m <- tryCatch(
    request(sprintf("https://api.elections.kalshi.com/trade-api/v2/markets/%s", row$ticker)) %>%
      req_perform() %>% resp_body_json(),
    error = function(e) NULL)
  if (is.null(m)) { results[[i]] <- NULL; next }
  m <- m$market %||% m
  open_ts <- parse_kalshi_ts(m$open_time)
  close_ts <- parse_kalshi_ts(m$close_time)
  if (is.na(open_ts) || is.na(close_ts) || close_ts <= open_ts) { results[[i]] <- NULL; next }

  cs <- fetch_candlesticks_retry(row$ticker, open_ts, close_ts)
  Sys.sleep(0.12)
  if (is.null(cs)) { results[[i]] <- NULL; next }
  cands <- cs$candlesticks %||% list()
  if (length(cands) == 0) { results[[i]] <- NULL; next }

  snapshot_ts <- open_ts + round(row$minutes_from_open * 60)
  future_cands <- Filter(function(c) (c$end_period_ts %||% 0) > snapshot_ts, cands)

  yes_bid <- 2 * row$snapshot_mid - row$snapshot_price  # recover from stored snapshot
  yes_ask <- row$snapshot_price
  spread <- yes_ask - yes_bid

  # Candidate resting sell-YES levels between current bid and ask.
  targets <- list(at_bid = yes_bid, quarter_in = yes_bid + 0.25 * spread,
                   at_mid = yes_bid + 0.50 * spread, three_quarter = yes_bid + 0.75 * spread,
                   at_ask = yes_ask)

  max_future_price <- if (length(future_cands) > 0) {
    prices <- sapply(future_cands, function(c) {
      if (length(c$price) == 0) return(NA_real_)
      suppressWarnings(as.numeric(c$price$high_dollars %||% c$price$close_dollars %||% NA))
    })
    suppressWarnings(max(prices, na.rm = TRUE))
  } else NA_real_

  fills <- sapply(targets, function(t) !is.na(max_future_price) && max_future_price >= t)

  results[[i]] <- tibble(
    ticker = row$ticker, event_ticker = row$event_ticker, threshold = row$threshold,
    result = row$result, yes_bid = yes_bid, yes_ask = yes_ask, spread = spread,
    max_future_yes_price = max_future_price,
    fill_at_bid = fills["at_bid"], fill_quarter_in = fills["quarter_in"],
    fill_at_mid = fills["at_mid"], fill_three_quarter = fills["three_quarter"],
    fill_at_ask = fills["at_ask"]
  )

  if (i %% 200 == 0) {
    cat(sprintf("  [%d/%d] checkpointing...\n", i, nrow(base)))
    saveRDS(bind_rows(results[1:i]), OUT_FILE)
  }
}

final_df <- bind_rows(results)
saveRDS(final_df, OUT_FILE)
cat(sprintf("\nDone. %d legs with usable data.\n", nrow(final_df)))
