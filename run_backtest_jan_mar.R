# Fetch Feb + Mar 2026 Kalshi data, merge with Jan, and re-run spread backtest
# Kalshi API ignores min/max_close_ts filters, so we paginate all settled markets
# and filter by the date encoded in each ticker.

PROJECT_DIR <- "C:/Users/Anthony/OneDrive/Documents/High Dimensional Data Analytics/testing"
setwd(PROJECT_DIR)

library(tidyverse)
library(httr2)

KALSHI_BASE_URL <- "https://api.elections.kalshi.com/trade-api/v2"

# ---- Helper: fetch one page of settled KXNBAGAME markets ----
fetch_markets_page <- function(cursor = NULL) {
  params <- list(limit = 200, status = "settled", series_ticker = "KXNBAGAME")
  if (!is.null(cursor) && nzchar(cursor)) params$cursor <- cursor

  tryCatch({
    req <- request(paste0(KALSHI_BASE_URL, "/markets"))
    req <- do.call(req_url_query, c(list(req), params))
    resp <- req_perform(req)
    resp_body_json(resp)
  }, error = function(e) {
    message("API error: ", e$message)
    NULL
  })
}

# ---- Helper: fetch candlesticks for a single ticker ----
fetch_candlesticks <- function(ticker, game_date) {
  start_ts <- as.integer(as.POSIXct(paste(game_date, "12:00:00"), tz = "America/New_York"))
  end_ts   <- as.integer(as.POSIXct(paste(game_date + 1, "06:00:00"), tz = "America/New_York"))

  result <- tryCatch({
    req <- request(sprintf("%s/series/KXNBAGAME/markets/%s/candlesticks", KALSHI_BASE_URL, ticker))
    req <- req_url_query(req, start_ts = start_ts, end_ts = end_ts, period_interval = 1)
    resp_body_json(req_perform(req))
  }, error = function(e) NULL)

  if (is.null(result) || length(result$candlesticks) == 0) return(NULL)

  rows <- lapply(result$candlesticks, function(c) {
    ts    <- as.POSIXct(c$end_period_ts, origin = "1970-01-01", tz = "UTC")
    trade <- c$price$close
    bid   <- c$yes_bid$close
    ask   <- c$yes_ask$close
    price <- if (!is.null(trade)) trade / 100 else
             if (!is.null(bid) && !is.null(ask)) (bid + ask) / 2 / 100 else NA_real_
    data.frame(
      timestamp = ts,
      price     = price,
      yes_bid   = if (!is.null(bid)) bid / 100 else NA_real_,
      yes_ask   = if (!is.null(ask)) ask / 100 else NA_real_,
      volume    = c$volume %||% 0L,
      stringsAsFactors = FALSE
    )
  })
  bind_rows(rows)
}

# ---- Step 1: Paginate all settled markets, keep Feb-Mar 2026 ----
message("\n", strrep("=", 60))
message("  FETCHING ALL SETTLED KXNBAGAME MARKETS (paginating)")
message(strrep("=", 60))

TARGET_START <- as.Date("2026-02-01")
TARGET_END   <- as.Date("2026-03-31")

all_markets <- list()
cursor <- NULL
page <- 1

repeat {
  message(sprintf("  Page %d (cursor: %s)...", page, if (is.null(cursor)) "start" else substr(cursor, 1, 20)))
  resp <- fetch_markets_page(cursor)
  if (is.null(resp) || length(resp$markets) == 0) break

  all_markets <- c(all_markets, resp$markets)
  cursor <- resp$cursor %||% ""
  page <- page + 1
  if (!nzchar(cursor)) break
  Sys.sleep(0.2)
}

message(sprintf("Total settled KXNBAGAME markets found: %d", length(all_markets)))

# Parse tickers and filter by date
# Ticker format: KXNBAGAME-{YYMMMDD}{AWAY}{HOME}-{TEAM}
markets_df <- map_dfr(all_markets, function(m) {
  ticker <- m$ticker %||% ""
  date_str <- regmatches(ticker, regexpr("\\d{2}[A-Z]{3}\\d{2}", ticker))
  game_date <- if (length(date_str) == 1) as.Date(date_str, format = "%y%b%d") else NA_Date_
  teams_part <- regmatches(ticker, regexpr("(?<=\\d{2}[A-Z]{3}\\d{2})[A-Z]{6}", ticker, perl = TRUE))
  tibble(
    ticker      = ticker,
    result      = m$result %||% NA_character_,
    game_date   = game_date,
    away_team   = if (length(teams_part) == 1) substr(teams_part, 1, 3) else NA_character_,
    home_team   = if (length(teams_part) == 1) substr(teams_part, 4, 6) else NA_character_,
    market_team = regmatches(ticker, regexpr("[A-Z]{3}$", ticker)) %||% NA_character_
  )
}) %>%
  filter(!is.na(game_date), game_date >= TARGET_START, game_date <= TARGET_END,
         market_team == home_team)   # home team markets only

message(sprintf("Markets in Feb-Mar 2026 (home-team only): %d", nrow(markets_df)))

# ---- Step 2: Fetch candlesticks for each Feb-Mar market ----
message("\n", strrep("=", 60))
message("  FETCHING CANDLESTICK DATA")
message(strrep("=", 60))

candles_list <- list()
for (i in seq_len(nrow(markets_df))) {
  m <- markets_df[i, ]
  message(sprintf("[%d/%d] %s (%s @ %s)...", i, nrow(markets_df), m$ticker, m$away_team, m$home_team))
  candles <- fetch_candlesticks(m$ticker, m$game_date)
  if (!is.null(candles) && nrow(candles) > 0) {
    candles$ticker      <- m$ticker
    candles$game_date   <- m$game_date
    candles$home_team   <- m$home_team
    candles$away_team   <- m$away_team
    candles$market_team <- m$market_team
    candles$result      <- m$result
    candles_list[[length(candles_list) + 1]] <- candles
    message(sprintf("  %d candles", nrow(candles)))
  } else {
    message("  no data")
  }
  Sys.sleep(0.3)
}

if (length(candles_list) == 0) stop("No candlestick data retrieved for any Feb-Mar market")

new_data <- bind_rows(candles_list)
message(sprintf("\nFeb-Mar rows: %d across %d games", nrow(new_data), length(candles_list)))
message(sprintf("Date range: %s to %s", min(new_data$game_date), max(new_data$game_date)))

# ---- Step 3: Merge with January data ----
message("\nLoading existing January data...")
jan_data <- readRDS("kalshi_nba_full_data.rds")
message(sprintf("  Jan rows: %d", nrow(jan_data)))

# Back up original before overwriting
file.copy("kalshi_nba_full_data.rds", "kalshi_nba_full_data_jan_only_backup.rds", overwrite = TRUE)

combined <- bind_rows(jan_data, new_data)
message(sprintf("  Combined rows: %d", nrow(combined)))
message(sprintf("  Full date range: %s to %s",
                min(combined$game_date, na.rm = TRUE),
                max(combined$game_date, na.rm = TRUE)))
saveRDS(combined, "kalshi_nba_full_data.rds")
message("Saved combined dataset.")

# ---- Step 4: Run the backtest ----
message("\n\n")
source("backtest_with_spread_2026.R")
