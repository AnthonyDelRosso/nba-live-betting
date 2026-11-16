# Fetch Feb-Mar 2026 Kalshi candlestick data using historical endpoints
# Strategy:
#   1. Check /historical/cutoff to see what's been archived
#   2. Try /historical/markets for settled KXNBAGAME markets in Feb-Mar
#   3. Fall back to /markets for any still in the live window
#   4. Fetch candlesticks (historical or live endpoint) for each home-team market
#   5. Merge with Jan data and save combined dataset

PROJECT_DIR <- "C:/Users/Anthony/OneDrive/Documents/High Dimensional Data Analytics/testing"
setwd(PROJECT_DIR)

library(tidyverse)
library(httr2)

KALSHI_BASE_URL  <- "https://api.elections.kalshi.com/trade-api/v2"
TARGET_START     <- as.Date("2026-02-01")
TARGET_END       <- as.Date("2026-03-31")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

parse_ticker_date <- function(ticker) {
  date_str <- regmatches(ticker, regexpr("\\d{2}[A-Z]{3}\\d{2}", ticker))
  if (length(date_str) == 1) as.Date(date_str, format = "%y%b%d") else NA_Date_
}

parse_ticker_teams <- function(ticker) {
  teams_part <- regmatches(ticker, regexpr("(?<=\\d{2}[A-Z]{3}\\d{2})[A-Z]{6}", ticker, perl = TRUE))
  market_team <- regmatches(ticker, regexpr("[A-Z]{3}$", ticker))
  list(
    away_team   = if (length(teams_part) == 1) substr(teams_part, 1, 3) else NA_character_,
    home_team   = if (length(teams_part) == 1) substr(teams_part, 4, 6) else NA_character_,
    market_team = if (length(market_team) == 1) market_team else NA_character_
  )
}

fetch_page <- function(base_url, params) {
  tryCatch({
    req <- request(base_url)
    req <- do.call(req_url_query, c(list(req), params))
    resp_body_json(req_perform(req))
  }, error = function(e) {
    message("  API error: ", conditionMessage(e))
    NULL
  })
}

paginate_markets <- function(endpoint) {
  all_markets <- list()
  cursor <- NULL
  page <- 1
  repeat {
    params <- list(limit = 200, status = "settled", series_ticker = "KXNBAGAME")
    if (!is.null(cursor) && nzchar(cursor)) params$cursor <- cursor
    message(sprintf("  Page %d...", page))
    resp <- fetch_page(endpoint, params)
    if (is.null(resp) || length(resp$markets) == 0) break
    all_markets <- c(all_markets, resp$markets)
    cursor <- resp$cursor %||% ""
    page   <- page + 1
    if (!nzchar(cursor)) break
    Sys.sleep(0.3)
  }
  all_markets
}

fetch_candlesticks <- function(ticker, game_date, use_historical = FALSE) {
  start_ts <- as.integer(as.POSIXct(paste(game_date, "12:00:00"), tz = "America/New_York"))
  end_ts   <- as.integer(as.POSIXct(paste(game_date + 1, "06:00:00"), tz = "America/New_York"))

  if (use_historical) {
    url <- sprintf("%s/historical/series/KXNBAGAME/markets/%s/candlesticks", KALSHI_BASE_URL, ticker)
  } else {
    url <- sprintf("%s/series/KXNBAGAME/markets/%s/candlesticks", KALSHI_BASE_URL, ticker)
  }

  result <- tryCatch({
    req <- request(url)
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

markets_to_df <- function(markets_list) {
  map_dfr(markets_list, function(m) {
    ticker <- m$ticker %||% ""
    teams  <- parse_ticker_teams(ticker)
    tibble(
      ticker      = ticker,
      result      = m$result %||% NA_character_,
      game_date   = parse_ticker_date(ticker),
      away_team   = teams$away_team,
      home_team   = teams$home_team,
      market_team = teams$market_team
    )
  })
}

# ---------------------------------------------------------------------------
# Step 1: Check cutoff
# ---------------------------------------------------------------------------
message("\n", strrep("=", 60))
message("  CHECKING HISTORICAL CUTOFF")
message(strrep("=", 60))

cutoff_resp <- fetch_page(paste0(KALSHI_BASE_URL, "/historical/cutoff"), list())
if (!is.null(cutoff_resp)) {
  message("Cutoff timestamps:")
  for (nm in names(cutoff_resp)) {
    ts <- cutoff_resp[[nm]]
    if (!is.null(ts) && is.numeric(ts)) {
      message(sprintf("  %-45s %s", nm, as.POSIXct(ts, origin = "1970-01-01", tz = "UTC")))
    } else {
      message(sprintf("  %-45s %s", nm, ts))
    }
  }
} else {
  message("Could not fetch cutoff (endpoint may require auth or not exist)")
}

# ---------------------------------------------------------------------------
# Step 2: Paginate historical markets endpoint
# ---------------------------------------------------------------------------
message("\n", strrep("=", 60))
message("  FETCHING HISTORICAL KXNBAGAME MARKETS")
message(strrep("=", 60))

hist_markets_raw <- paginate_markets(paste0(KALSHI_BASE_URL, "/historical/markets"))
message(sprintf("Historical endpoint returned: %d markets", length(hist_markets_raw)))

# ---------------------------------------------------------------------------
# Step 3: Paginate live markets endpoint (catches anything not yet archived)
# ---------------------------------------------------------------------------
message("\n", strrep("=", 60))
message("  FETCHING LIVE (SETTLED) KXNBAGAME MARKETS")
message(strrep("=", 60))

live_markets_raw <- paginate_markets(paste0(KALSHI_BASE_URL, "/markets"))
message(sprintf("Live endpoint returned: %d markets", length(live_markets_raw)))

# ---------------------------------------------------------------------------
# Step 4: Combine, parse, filter Feb-Mar home-team markets
# ---------------------------------------------------------------------------
hist_df <- if (length(hist_markets_raw) > 0) markets_to_df(hist_markets_raw) else tibble()
live_df <- if (length(live_markets_raw) > 0) markets_to_df(live_markets_raw) else tibble()

all_markets_df <- bind_rows(hist_df, live_df) %>%
  distinct(ticker, .keep_all = TRUE) %>%
  filter(!is.na(game_date),
         game_date >= TARGET_START,
         game_date <= TARGET_END,
         market_team == home_team)

message(sprintf("\nFeb-Mar 2026 home-team markets found: %d", nrow(all_markets_df)))
if (nrow(all_markets_df) > 0) {
  message(sprintf("Date range: %s to %s", min(all_markets_df$game_date), max(all_markets_df$game_date)))
  print(all_markets_df %>% arrange(game_date) %>% select(game_date, away_team, home_team, ticker))
}

if (nrow(all_markets_df) == 0) {
  stop("No Feb-Mar markets found from either endpoint. Cannot continue.")
}

# Determine which markets are historical vs live based on cutoff
market_settled_cutoff <- if (!is.null(cutoff_resp$market_settled_ts)) {
  as.POSIXct(cutoff_resp$market_settled_ts, origin = "1970-01-01", tz = "UTC")
} else {
  Sys.time() - 90 * 24 * 3600  # fallback: 90 days ago
}

# ---------------------------------------------------------------------------
# Step 5: Fetch candlesticks for each market
# ---------------------------------------------------------------------------
message("\n", strrep("=", 60))
message("  FETCHING CANDLESTICK DATA")
message(strrep("=", 60))

candles_list <- list()
for (i in seq_len(nrow(all_markets_df))) {
  m   <- all_markets_df[i, ]
  # Try live endpoint first; if no data, try historical
  message(sprintf("[%d/%d] %s (%s @ %s)...",
                  i, nrow(all_markets_df), m$ticker, m$away_team, m$home_team))

  candles <- fetch_candlesticks(m$ticker, m$game_date, use_historical = FALSE)
  if (is.null(candles) || nrow(candles) == 0) {
    message("  Live endpoint empty, trying historical...")
    candles <- fetch_candlesticks(m$ticker, m$game_date, use_historical = TRUE)
  }

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
    message("  no data from either endpoint")
  }
  Sys.sleep(0.3)
}

if (length(candles_list) == 0) {
  stop("No candlestick data retrieved for any Feb-Mar market.")
}

new_data <- bind_rows(candles_list)
message(sprintf("\nFeb-Mar rows: %d across %d games",
                nrow(new_data), length(candles_list)))
message(sprintf("Date range: %s to %s",
                min(new_data$game_date), max(new_data$game_date)))

# ---------------------------------------------------------------------------
# Step 6: Merge with January data
# ---------------------------------------------------------------------------
message("\nLoading existing January data...")
jan_data <- readRDS("kalshi_nba_full_data.rds")
message(sprintf("  Jan rows: %d", nrow(jan_data)))

# Back up before overwriting
file.copy("kalshi_nba_full_data.rds", "kalshi_nba_full_data_jan_only_backup.rds", overwrite = TRUE)
message("  Jan backup saved.")

combined <- bind_rows(jan_data, new_data)
message(sprintf("  Combined rows: %d", nrow(combined)))
message(sprintf("  Full date range: %s to %s",
                min(combined$game_date, na.rm = TRUE),
                max(combined$game_date, na.rm = TRUE)))

saveRDS(combined, "kalshi_nba_full_data.rds")
message("Saved combined dataset to kalshi_nba_full_data.rds")

# ---------------------------------------------------------------------------
# Step 7: Run the backtest
# ---------------------------------------------------------------------------
message("\n\n")
source("backtest_with_spread_2026.R")
