# Fetch Full Kalshi NBA Game Data
# Uses Kalshi API to get complete candlestick data from game start to finish
# No authentication required for read endpoints

library(tidyverse)
library(httr2)
library(jsonlite)

# ------------------------------------------------------------------------------
# Kalshi API Configuration
# ------------------------------------------------------------------------------

KALSHI_BASE_URL <- "https://api.elections.kalshi.com/trade-api/v2"

# ------------------------------------------------------------------------------
# API Helper Functions
# ------------------------------------------------------------------------------

#' Make request to Kalshi API (no auth needed for reads)
kalshi_request <- function(endpoint, params = list()) {
  url <- paste0(KALSHI_BASE_URL, endpoint)

  req <- request(url) %>%
    req_headers("Content-Type" = "application/json")

  if (length(params) > 0) {
    req <- req %>% req_url_query(!!!params)
  }

  tryCatch({
    resp <- req %>% req_perform()
    resp %>% resp_body_json()
  }, error = function(e) {
    message(sprintf("API Error: %s", e$message))
    NULL
  })
}

#' Get all NBA game markets for a date range
get_nba_markets <- function(start_date, end_date) {
  message(sprintf("Fetching NBA markets from %s to %s...", start_date, end_date))

  start_ts <- as.integer(as.POSIXct(start_date, tz = "UTC"))
  end_ts <- as.integer(as.POSIXct(as.Date(end_date) + 1, tz = "UTC"))

  markets <- list()
  cursor <- NULL

  repeat {
    params <- list(
      limit = 200,
      status = "settled",
      series_ticker = "KXNBAGAME",
      min_close_ts = start_ts,
      max_close_ts = end_ts
    )
    if (!is.null(cursor) && cursor != "") {
      params$cursor <- cursor
    }

    result <- kalshi_request("/markets", params)

    if (is.null(result) || length(result$markets) == 0) {
      break
    }

    markets <- c(markets, result$markets)
    message(sprintf("  Fetched %d markets so far...", length(markets)))

    cursor <- result$cursor
    if (is.null(cursor) || cursor == "") {
      break
    }

    Sys.sleep(0.2)
  }

  message(sprintf("Found %d total NBA markets", length(markets)))

  if (length(markets) == 0) {
    return(tibble())
  }

  # Convert to dataframe
  markets_df <- map_dfr(markets, function(m) {
    tibble(
      ticker = m$ticker,
      title = m$title %||% "",
      subtitle = m$subtitle %||% "",
      status = m$status,
      result = m$result %||% NA,
      open_time = m$open_time %||% NA,
      close_time = m$close_time %||% NA
    )
  })

  # Parse game date and teams from ticker
  # Ticker format: KXNBAGAME-{YYMMMDD}{AWAY}{HOME}-{OUTCOME_TEAM}
  # Example: KXNBAGAME-26JAN30DETGSW-GSW = DET @ GSW, market for GSW
  markets_df <- markets_df %>%
    mutate(
      game_date = as.Date(str_extract(ticker, "\\d{2}[A-Z]{3}\\d{2}"), format = "%y%b%d"),
      # Extract the 6-char team portion after the date
      teams_portion = str_extract(ticker, "(?<=\\d{2}[A-Z]{3}\\d{2})[A-Z]{6}"),
      away_team = str_sub(teams_portion, 1, 3),
      home_team = str_sub(teams_portion, 4, 6),
      market_team = str_extract(ticker, "[A-Z]{3}$")
    ) %>%
    select(-teams_portion) %>%
    filter(game_date >= as.Date(start_date), game_date <= as.Date(end_date))

  message(sprintf("Filtered to %d markets in date range", nrow(markets_df)))
  markets_df
}

#' Get candlestick data for a single market
get_market_candlesticks <- function(ticker, game_date, period_minutes = 1) {
  # Set time range to cover full game day (noon to 6am next day ET)
  start_ts <- as.integer(as.POSIXct(paste(game_date, "12:00:00"), tz = "America/New_York"))
  end_ts <- as.integer(as.POSIXct(paste(game_date + 1, "06:00:00"), tz = "America/New_York"))

  result <- kalshi_request(
    sprintf("/series/KXNBAGAME/markets/%s/candlesticks", ticker),
    list(
      start_ts = start_ts,
      end_ts = end_ts,
      period_interval = period_minutes
    )
  )

  if (is.null(result) || length(result$candlesticks) == 0) {
    return(NULL)
  }

  # Parse candlesticks - price is nested in the response
  candles_list <- lapply(result$candlesticks, function(c) {
    # Get timestamp
    ts <- as.POSIXct(c$end_period_ts, origin = "1970-01-01", tz = "UTC")

    # Get price - use trade price if available, otherwise midpoint of bid/ask
    trade_price <- c$price$close  # Actual trade price
    yes_bid <- c$yes_bid$close    # Bid price
    yes_ask <- c$yes_ask$close    # Ask price

    if (!is.null(trade_price)) {
      price <- trade_price / 100
    } else if (!is.null(yes_bid) && !is.null(yes_ask)) {
      price <- (yes_bid + yes_ask) / 2 / 100  # Midpoint
    } else if (!is.null(c$price$previous)) {
      price <- c$price$previous / 100  # Last known price
    } else {
      price <- NA_real_
    }

    # Get OHLC from trade prices if available
    open_p <- if (!is.null(c$price$open)) c$price$open / 100 else price
    high_p <- if (!is.null(c$price$high)) c$price$high / 100 else price
    low_p <- if (!is.null(c$price$low)) c$price$low / 100 else price
    close_p <- if (!is.null(c$price$close)) c$price$close / 100 else price

    data.frame(
      timestamp = ts,
      price = price,
      open = open_p,
      high = high_p,
      low = low_p,
      close = close_p,
      yes_bid = if (!is.null(yes_bid)) yes_bid / 100 else NA_real_,
      yes_ask = if (!is.null(yes_ask)) yes_ask / 100 else NA_real_,
      volume = c$volume %||% 0L,
      stringsAsFactors = FALSE
    )
  })

  candles_df <- bind_rows(candles_list)
  candles_df$ticker <- ticker
  candles_df
}

# ------------------------------------------------------------------------------
# Main Data Collection Function
# ------------------------------------------------------------------------------

#' Fetch complete NBA game data from Kalshi
#'
#' @param start_date Start date (YYYY-MM-DD)
#' @param end_date End date (YYYY-MM-DD)
#' @param output_file Path to save the RDS file
fetch_nba_games <- function(start_date = "2026-01-01",
                            end_date = "2026-01-31",
                            output_file = "kalshi_nba_full_data.rds") {

  message("\n", strrep("=", 60))
  message("  KALSHI NBA DATA COLLECTION")
  message(strrep("=", 60), "\n")

  # Get list of markets
  markets <- get_nba_markets(start_date, end_date)

  if (nrow(markets) == 0) {
    stop("No markets found in date range")
  }

  # Fetch candlestick data for each market
  all_data <- list()
  success_count <- 0

  for (i in 1:nrow(markets)) {
    m <- markets[i, ]
    message(sprintf("[%d/%d] %s (%s @ %s)...",
                    i, nrow(markets), m$ticker, m$away_team, m$home_team))

    candles <- get_market_candlesticks(m$ticker, m$game_date)

    if (!is.null(candles) && nrow(candles) > 0) {
      candles$game_date <- m$game_date
      candles$home_team <- m$home_team
      candles$away_team <- m$away_team
      candles$market_team <- m$market_team
      candles$result <- m$result

      duration <- as.numeric(difftime(max(candles$timestamp),
                                       min(candles$timestamp), units = "mins"))
      message(sprintf("  Got %d candles (%.0f min coverage)", nrow(candles), duration))

      all_data[[i]] <- candles
      success_count <- success_count + 1
    } else {
      message("  No data")
    }

    Sys.sleep(0.3)  # Rate limiting
  }

  # Combine all data
  if (length(all_data) == 0) {
    stop("No candlestick data retrieved for any market")
  }

  full_data <- bind_rows(all_data)

  message("\n", strrep("=", 60))
  message("  COLLECTION COMPLETE")
  message(strrep("=", 60))
  message(sprintf("\nTotal observations: %d", nrow(full_data)))
  message(sprintf("Games with data: %d / %d", success_count, nrow(markets)))
  message(sprintf("Date range: %s to %s", min(full_data$game_date), max(full_data$game_date)))

  # Calculate coverage stats
  coverage <- full_data %>%
    group_by(ticker) %>%
    summarise(
      duration_mins = as.numeric(difftime(max(timestamp), min(timestamp), units = "mins")),
      n_candles = n(),
      .groups = "drop"
    )

  message(sprintf("\nAverage coverage per game: %.0f mins (%.0f candles)",
                  mean(coverage$duration_mins), mean(coverage$n_candles)))

  # Save to file
  saveRDS(full_data, output_file)
  message(sprintf("\nSaved to: %s", output_file))

  invisible(full_data)
}

# ------------------------------------------------------------------------------
# Run if sourced
# ------------------------------------------------------------------------------

message("\n", strrep("=", 60))
message("  KALSHI DATA FETCHER LOADED")
message(strrep("=", 60))
message("
To fetch full NBA game data for January 2026:

  data <- fetch_nba_games(
    start_date = '2026-01-01',
    end_date = '2026-01-31',
    output_file = 'kalshi_nba_full_data.rds'
  )

No API key required - read endpoints are public.
")
