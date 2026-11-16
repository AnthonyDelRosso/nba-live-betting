# Step 2 of the spread-straddle research: is Kalshi's KXNBASPREAD pricing
# actually calibrated, especially in the tails?
#
# Inspecting a full per-minute candle sequence showed the first several
# quoted asks after a strike opens are noisy/unreliable (e.g. 0.98 -> 0.35 ->
# 0.98 -> 0.98 before settling into a real trend once genuine trading starts)
# — almost every market's "first ask" sampled near 0.95-0.99 regardless of
# outcome, which is pure seed-quote noise, not price discovery. The
# economically meaningful, defensible snapshot is the first candle with
# actual trade VOLUME: a real transacted price, not a resting/un-crossed
# quote. Use that candle's traded price (and, separately, the best ask in
# that same window) and test whether it's calibrated to the eventual result.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) setwd(args[1])

library(httr2); library(jsonlite); library(tidyverse)

OUT_FILE <- "calibration_data.rds"
CHECKPOINT_EVERY <- 200

cat("Fetching all settled KXNBASPREAD markets...\n")
all_markets <- list()
cursor <- NULL
repeat {
  req <- request("https://api.elections.kalshi.com/trade-api/v2/markets") %>%
    req_url_query(series_ticker = "KXNBASPREAD", status = "settled", limit = 1000,
                  !!!(if (!is.null(cursor) && nzchar(cursor)) list(cursor = cursor) else list()))
  resp <- req %>% req_perform() %>% resp_body_json()
  batch <- resp$markets %||% list()
  all_markets <- c(all_markets, batch)
  cursor <- resp$cursor %||% ""
  if (!nzchar(cursor) || length(batch) == 0) break
}
cat(sprintf("Total settled markets: %d\n", length(all_markets)))

# Parse the point threshold out of the title (e.g. "... wins by over 8.5 points")
extract_threshold <- function(title) {
  m <- regmatches(title, regexpr("[0-9]+\\.[0-9]+", title))
  if (length(m) == 0 || !nzchar(m)) return(NA_real_)
  as.numeric(m)
}
regmatches <- regmatches  # base R

thresholds <- sapply(all_markets, function(m) extract_threshold(m$title %||% ""))
cat(sprintf("Threshold range: %.1f to %.1f\n", min(thresholds, na.rm=TRUE), max(thresholds, na.rm=TRUE)))
cat(sprintf("Markets with threshold >= 15: %d\n", sum(thresholds >= 15, na.rm = TRUE)))

# Prioritize the tail (>=15 pts, where the thesis lives) and keep a random
# baseline sample of near-the-money strikes for comparison.
idx_tail <- which(thresholds >= 15)
idx_near <- which(thresholds < 15)
set.seed(42)
idx_near_sample <- sample(idx_near, min(800, length(idx_near)))
idx_to_fetch <- c(idx_tail, idx_near_sample)
cat(sprintf("Fetching candlesticks for %d markets (%d tail + %d near-the-money sample)...\n",
            length(idx_to_fetch), length(idx_tail), length(idx_near_sample)))

# Kalshi timestamps sometimes include fractional seconds
# (e.g. "2026-11-07T02:57:30.772583Z") and sometimes don't — strip them
# before parsing so a fixed format string doesn't silently yield NA.
parse_kalshi_ts <- function(ts) {
  if (is.null(ts) || is.na(ts) || !nzchar(ts)) return(NA_integer_)
  ts_clean <- sub("\\.[0-9]+Z$", "Z", ts)
  as.integer(as.POSIXct(ts_clean, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
}

# Wrapped in a real function (not evaluated at script top level) so that
# return() inside the loop actually exits with a value instead of just
# behaving like break and falling through to whatever's next in the block.
fetch_first_trade <- function(m) {
  tryCatch({
    open_ts <- parse_kalshi_ts(m$open_time)
    close_ts <- parse_kalshi_ts(m$close_time)
    if (is.na(open_ts) || is.na(close_ts) || close_ts <= open_ts) {
      return(list(price = NA_real_, ask_at_trade = NA_real_, minutes_to_trade = NA_real_))
    }

    cs <- request(sprintf("https://api.elections.kalshi.com/trade-api/v2/series/KXNBASPREAD/markets/%s/candlesticks", m$ticker)) %>%
      req_url_query(start_ts = open_ts, end_ts = close_ts, period_interval = 1) %>%
      req_perform() %>% resp_body_json()
    cands <- cs$candlesticks %||% list()
    if (length(cands) == 0) {
      return(list(price = NA_real_, ask_at_trade = NA_real_, minutes_to_trade = NA_real_))
    }

    for (i in seq_along(cands)) {
      cnd <- cands[[i]]
      vol <- suppressWarnings(as.numeric(cnd$volume_fp %||% 0))
      if (!is.na(vol) && vol > 0 && length(cnd$price) > 0) {
        trade_price <- suppressWarnings(as.numeric(cnd$price$close_dollars %||% cnd$price$open_dollars %||% NA))
        ask <- suppressWarnings(as.numeric(cnd$yes_ask$close_dollars %||% cnd$yes_ask$open_dollars %||% NA))
        if (!is.na(trade_price)) {
          return(list(price = trade_price, ask_at_trade = ask, minutes_to_trade = i))
        }
      }
    }
    list(price = NA_real_, ask_at_trade = NA_real_, minutes_to_trade = NA_real_)
  }, error = function(e) list(price = NA_real_, ask_at_trade = NA_real_, minutes_to_trade = NA_real_))
}

results <- vector("list", length(idx_to_fetch))
for (i in seq_along(idx_to_fetch)) {
  m <- all_markets[[idx_to_fetch[i]]]
  ft <- fetch_first_trade(m)

  results[[i]] <- tibble(
    ticker = m$ticker,
    event_ticker = m$event_ticker %||% NA,
    threshold = thresholds[idx_to_fetch[i]],
    result = m$result %||% NA,
    volume_fp = as.numeric(m$volume_fp %||% 0),
    trade_price = ft$price,
    ask_at_trade = ft$ask_at_trade,
    minutes_to_trade = ft$minutes_to_trade
  )

  if (i %% CHECKPOINT_EVERY == 0) {
    cat(sprintf("  [%d/%d] checkpointing...\n", i, length(idx_to_fetch)))
    saveRDS(bind_rows(results[1:i]), OUT_FILE)
  }
}

final_df <- bind_rows(results)
saveRDS(final_df, OUT_FILE)
cat(sprintf("\nDone. Saved %d rows to %s (%d with usable trade_price)\n",
            nrow(final_df), OUT_FILE, sum(!is.na(final_df$trade_price))))
