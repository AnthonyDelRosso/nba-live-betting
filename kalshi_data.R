# Kalshi API Data Fetcher
# Fetches historical NBA market data for backtesting
#
# API Docs: https://docs.kalshi.com

library(tidyverse)
library(httr2)
library(jsonlite)

# ------------------------------------------------------------------------------
# API Configuration
# ------------------------------------------------------------------------------

KALSHI_BASE_URL <- "https://api.elections.kalshi.com/trade-api/v2"

# ------------------------------------------------------------------------------
# API Helper Functions
# ------------------------------------------------------------------------------

#' Make a GET request to Kalshi API with retry logic
#' @param endpoint API endpoint (without base URL)
#' @param params List of query parameters
#' @param max_retries Maximum number of retry attempts
#' @param base_delay Base delay in seconds for exponential backoff
#' @return Parsed JSON response
kalshi_get <- function(endpoint, params = list(), max_retries = 5, base_delay = 2) {
  url <- paste0(KALSHI_BASE_URL, endpoint)

  for (attempt in 1:max_retries) {
    result <- tryCatch({
      resp <- request(url) %>%
        req_url_query(!!!params) %>%
        req_headers(
          "Accept" = "application/json",
          "User-Agent" = "R-KalshiBacktest/1.0"
        ) %>%
        req_perform()

      return(resp %>% resp_body_json())
    }, error = function(e) {
      # Check if it's a rate limit or server error (5xx)
      if (grepl("503|502|500|429|rate", e$message, ignore.case = TRUE)) {
        if (attempt < max_retries) {
          delay <- base_delay * (2 ^ (attempt - 1)) + runif(1, 0, 1)
          message(sprintf("  Rate limited (attempt %d/%d), waiting %.1fs...",
                          attempt, max_retries, delay))
          Sys.sleep(delay)
          return(NULL)  # Signal to retry
        }
      }
      stop(e)
    })

    if (!is.null(result)) return(result)
  }

  stop("Max retries exceeded for ", endpoint)
}

# ------------------------------------------------------------------------------
# Series & Market Discovery
# ------------------------------------------------------------------------------

#' Get list of all series (to find NBA series tickers)
#' @param status Filter by status (e.g., "open", "closed", "all")
#' @return Data frame of series
get_series_list <- function(status = NULL) {
  params <- list()
  if (!is.null(status)) params$status <- status

  result <- kalshi_get("/series", params)

  if (length(result$series) == 0) {
    return(tibble())
  }

  map_dfr(result$series, function(s) {
    tibble(
      ticker = s$ticker %||% NA_character_,
      title = s$title %||% NA_character_,
      category = s$category %||% NA_character_,
      tags = paste(s$tags %||% character(0), collapse = ", ")
    )
  })
}

#' Search for NBA-related series
#' @return Data frame of NBA series
find_nba_series <- function() {
  all_series <- get_series_list()

  all_series %>%
    filter(
      str_detect(str_to_lower(title), "nba|basketball") |
      str_detect(str_to_lower(category), "sports|basketball") |
      str_detect(str_to_lower(tags), "nba|basketball")
    )
}

#' Get markets for a series
#' @param series_ticker Series ticker
#' @param status Market status filter
#' @param limit Max markets to return
#' @return Data frame of markets
get_markets <- function(series_ticker = NULL, status = NULL, limit = 1000) {
  params <- list(limit = limit)
  if (!is.null(series_ticker)) params$series_ticker <- series_ticker
  if (!is.null(status)) params$status <- status

  result <- kalshi_get("/markets", params)

  if (length(result$markets) == 0) {
    return(tibble())
  }

  map_dfr(result$markets, function(m) {
    tibble(
      ticker = m$ticker %||% NA_character_,
      event_ticker = m$event_ticker %||% NA_character_,
      series_ticker = m$series_ticker %||% NA_character_,
      title = m$title %||% NA_character_,
      subtitle = m$subtitle %||% NA_character_,
      status = m$status %||% NA_character_,
      yes_bid = m$yes_bid %||% NA_real_,
      yes_ask = m$yes_ask %||% NA_real_,
      last_price = m$last_price %||% NA_real_,
      volume = m$volume %||% NA_integer_,
      open_interest = m$open_interest %||% NA_integer_,
      created_time = m$created_time %||% NA_character_,
      close_time = m$close_time %||% NA_character_,
      result = m$result %||% NA_character_
    )
  })
}

#' Search markets by keyword
#' @param keyword Search term (e.g., "Lakers", "NBA")
#' @param status Market status
#' @return Data frame of matching markets
search_markets <- function(keyword, status = NULL) {
  all_markets <- get_markets(status = status)

  all_markets %>%
    filter(
      str_detect(str_to_lower(title), str_to_lower(keyword)) |
      str_detect(str_to_lower(subtitle), str_to_lower(keyword))
    )
}

# ------------------------------------------------------------------------------
# Historical Data
# ------------------------------------------------------------------------------

#' Get candlestick data for a market
#' @param market_ticker Market ticker
#' @param period_interval Candle period: 1 (minute), 60 (hour), 1440 (day)
#' @param start_ts Start timestamp (Unix seconds)
#' @param end_ts End timestamp (Unix seconds)
#' @return Data frame of candlesticks
get_market_candlesticks <- function(market_ticker,
                                     period_interval = 60,
                                     start_ts = NULL,
                                     end_ts = NULL) {

  params <- list(
    period_interval = period_interval
  )
  if (!is.null(start_ts)) params$start_ts <- start_ts
  if (!is.null(end_ts)) params$end_ts <- end_ts

  endpoint <- sprintf("/markets/%s/candlesticks", market_ticker)

  result <- tryCatch({
    kalshi_get(endpoint, params)
  }, error = function(e) {
    message("Error fetching candlesticks for ", market_ticker, ": ", e$message)
    return(list(candlesticks = list()))
  })

  if (length(result$candlesticks) == 0) {
    return(tibble())
  }

  map_dfr(result$candlesticks, function(c) {
    tibble(
      market_ticker = market_ticker,
      timestamp = c$end_period_ts %||% NA_integer_,
      open = c$open %||% NA_real_,
      high = c$high %||% NA_real_,
      low = c$low %||% NA_real_,
      close = c$close %||% NA_real_,
      volume = c$volume %||% NA_integer_,
      yes_price = (c$yes_bid %||% 0 + c$yes_ask %||% 0) / 2
    )
  }) %>%
    mutate(
      datetime = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")
    )
}

#' Get historical trades for a market
#' @param market_ticker Market ticker
#' @param limit Max trades to return
#' @return Data frame of trades
get_market_trades <- function(market_ticker, limit = 1000) {
  params <- list(
    ticker = market_ticker,
    limit = limit
  )

  result <- tryCatch({
    kalshi_get("/markets/trades", params)
  }, error = function(e) {
    message("Error fetching trades for ", market_ticker, ": ", e$message)
    return(list(trades = list()))
  })

  if (length(result$trades) == 0) {
    return(tibble())
  }

  map_dfr(result$trades, function(t) {
    tibble(
      market_ticker = market_ticker,
      trade_id = t$trade_id %||% NA_character_,
      timestamp = t$created_time %||% NA_character_,
      price = t$yes_price %||% NA_real_,
      count = t$count %||% NA_integer_
    )
  })
}

#' Get event details including markets
#' @param event_ticker Event ticker
#' @return List with event data
get_event <- function(event_ticker) {
  tryCatch({
    kalshi_get(sprintf("/events/%s", event_ticker))
  }, error = function(e) {
    message("Error fetching event ", event_ticker, ": ", e$message)
    return(list())
  })
}

#' Get market history/details
#' @param market_ticker Market ticker
#' @return Market details
get_market <- function(market_ticker) {
  tryCatch({
    kalshi_get(sprintf("/markets/%s", market_ticker))
  }, error = function(e) {
    message("Error fetching market ", market_ticker, ": ", e$message)
    return(list())
  })
}

# ------------------------------------------------------------------------------
# NBA-Specific Functions
# ------------------------------------------------------------------------------

#' Get all NBA game markets (settled)
#' @param days_back Number of days of history to fetch
#' @return Data frame of NBA game markets
get_nba_game_markets <- function(days_back = 30) {
  message("Searching for NBA markets...")

  # Try different search approaches
  nba_markets <- bind_rows(
    search_markets("NBA", status = "settled"),
    search_markets("basketball", status = "settled"),
    search_markets("Lakers", status = "settled"),
    search_markets("Celtics", status = "settled"),
    search_markets("Warriors", status = "settled")
  ) %>%
    distinct(ticker, .keep_all = TRUE)

  if (nrow(nba_markets) == 0) {
    message("No NBA markets found. Trying to list all settled markets...")
    nba_markets <- get_markets(status = "settled") %>%
      filter(str_detect(str_to_lower(title), "nba|basketball|lakers|celtics|warriors|heat|bucks"))
  }

  message(sprintf("Found %d NBA markets", nrow(nba_markets)))
  nba_markets
}

#' Build historical odds dataset for NBA games
#' @param markets Data frame of markets (from get_nba_game_markets)
#' @param period_interval Candle period
#' @return Data frame with historical odds
build_nba_odds_history <- function(markets, period_interval = 60) {

  if (nrow(markets) == 0) {
    message("No markets to process")
    return(tibble())
  }

  message(sprintf("Fetching candlesticks for %d markets...", nrow(markets)))

  # Fetch candlesticks for each market
  all_candles <- map_dfr(markets$ticker, function(ticker) {
    message("  Fetching: ", ticker)
    Sys.sleep(0.5)  # Rate limiting
    get_market_candlesticks(ticker, period_interval = period_interval)
  })

  # Join with market metadata
  all_candles %>%
    left_join(
      markets %>% select(ticker, title, subtitle, result, close_time),
      by = c("market_ticker" = "ticker")
    )
}

# ------------------------------------------------------------------------------
# Example Usage
# ------------------------------------------------------------------------------

# # Explore available series
# series <- get_series_list()
# print(series %>% filter(str_detect(str_to_lower(category), "sport")))
#
# # Find NBA series
# nba_series <- find_nba_series()
# print(nba_series)
#
# # Get NBA game markets
# nba_markets <- get_nba_game_markets()
# print(nba_markets)
#
# # Get candlesticks for a specific market
# candles <- get_market_candlesticks("SOME_NBA_TICKER", period_interval = 60)
# print(candles)
#
# # Build full historical dataset
# odds_history <- build_nba_odds_history(nba_markets)
