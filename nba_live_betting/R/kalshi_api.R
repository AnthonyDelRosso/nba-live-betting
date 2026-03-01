# Kalshi API Client
# Handles public market data reads and RSA-PSS authenticated requests

library(R6)
library(httr2)
library(jsonlite)
library(tidyverse)

# Kalshi API endpoint — api.kalshi.com is deprecated, elections domain is current
KALSHI_API_BASE <- "https://api.elections.kalshi.com/trade-api/v2"

# NBA game winner markets use the KXNBAGAME series
KALSHI_NBA_SERIES <- "KXNBAGAME"

# ESPN → Kalshi team abbreviation mapping
# ESPN's API uses shortened abbreviations for some teams that differ from Kalshi tickers
ESPN_TO_KALSHI_ABBREV <- c(
  "GS"   = "GSW",   # Golden State Warriors
  "NY"   = "NYK",   # New York Knicks
  "WSH"  = "WAS",   # Washington Wizards
  "UTAH" = "UTA",   # Utah Jazz
  "SA"   = "SAS",   # San Antonio Spurs
  "NO"   = "NOP"    # New Orleans Pelicans
)

#' Normalize ESPN team abbreviation to Kalshi format
#' @param espn_abbrev ESPN team abbreviation
#' @return Kalshi-compatible abbreviation
normalize_team_abbrev <- function(espn_abbrev) {
  abbrev <- toupper(espn_abbrev)
  mapped <- ESPN_TO_KALSHI_ABBREV[abbrev]
  if (!is.na(mapped)) mapped else abbrev
}

#' Find the openssl CLI binary
#' Checks PATH first, then common Windows install locations (Git for Windows, etc.)
#' @return Path to openssl binary, or stops with an error
find_openssl_binary <- function() {
  # Check PATH first
  openssl_path <- Sys.which("openssl")
  if (nzchar(openssl_path)) return(as.character(openssl_path))

  # Common Windows locations (Git for Windows bundles openssl)
  candidates <- c(
    "C:/Program Files/Git/usr/bin/openssl.exe",
    "C:/Program Files (x86)/Git/usr/bin/openssl.exe",
    "C:/Program Files/OpenSSL-Win64/bin/openssl.exe",
    "C:/Program Files/OpenSSL/bin/openssl.exe",
    "C:/msys64/usr/bin/openssl.exe"
  )

  for (path in candidates) {
    if (file.exists(path)) return(path)
  }

  stop("openssl CLI not found. Install Git for Windows or OpenSSL for Windows and ensure it is on your PATH.")
}

# Cache the openssl path so we only search once
.openssl_bin <- NULL

#' Sign a message using RSA-PSS SHA-256 via the openssl CLI
#' @param message_string The message to sign (timestamp_ms + METHOD + path)
#' @param private_key_path Path to PEM private key file
#' @return Base64-encoded signature string
sign_rsa_pss <- function(message_string, private_key_path) {
  # Find openssl binary (cached after first lookup)
  if (is.null(.openssl_bin)) {
    .openssl_bin <<- find_openssl_binary()
    message(sprintf("[KALSHI] Using openssl at: %s", .openssl_bin))
    ver <- tryCatch(
      system2(.openssl_bin, "version", stdout = TRUE, stderr = TRUE),
      error = function(e) "unknown"
    )
    message(sprintf("[KALSHI] openssl version: %s", paste(ver, collapse = " ")))
  }

  msg_file <- tempfile(fileext = ".bin")
  hash_file <- tempfile(fileext = ".hash")
  sig_file <- tempfile(fileext = ".sig")
  on.exit(unlink(c(msg_file, hash_file, sig_file)), add = TRUE)

  writeBin(charToRaw(message_string), msg_file)

  # Step 1: Hash the message with SHA-256
  exit_hash <- system2(
    .openssl_bin, c(
      "dgst", "-sha256", "-binary",
      "-out", shQuote(hash_file),
      shQuote(msg_file)
    ),
    stdout = FALSE, stderr = TRUE
  )

  if (!is.null(attr(exit_hash, "status")) || !file.exists(hash_file)) {
    stderr_msg <- paste(exit_hash, collapse = "\n")
    stop("SHA-256 hash failed: ", stderr_msg)
  }

  # Step 2: Sign the hash with RSA-PSS using pkeyutl
  stderr_out <- system2(
    .openssl_bin, c(
      "pkeyutl", "-sign",
      "-inkey", shQuote(private_key_path),
      "-in", shQuote(hash_file),
      "-out", shQuote(sig_file),
      "-pkeyopt", "rsa_padding_mode:pss",
      "-pkeyopt", "rsa_pss_saltlen:-1",
      "-pkeyopt", "digest:sha256"
    ),
    stdout = FALSE, stderr = TRUE
  )

  exit_code <- attr(stderr_out, "status") %||% 0L
  if (exit_code != 0 || !file.exists(sig_file)) {
    stderr_msg <- paste(stderr_out, collapse = "\n")
    stop("openssl RSA-PSS signing failed (exit code ", exit_code, "): ", stderr_msg)
  }

  sig_bytes <- readBin(sig_file, "raw", file.info(sig_file)$size)
  openssl::base64_encode(sig_bytes)
}

#' KalshiClient R6 Class
#' Handles public market reads and RSA-PSS authenticated API calls
KalshiClient <- R6::R6Class(
  "KalshiClient",

  public = list(
    api_base = NULL,
    api_key_id = NULL,
    private_key_path = NULL,
    logger = NULL,

    # Cache of market listing data (refreshed by get_nba_markets)
    market_cache = NULL,
    market_cache_time = NULL,
    market_cache_ttl = 30,  # Seconds before cache expires

    #' Initialize the Kalshi client
    #' @param api_key_id Kalshi API key ID (optional, only needed for orders)
    #' @param private_key_path Path to RSA private key PEM file (optional)
    #' @param logger Logger instance
    initialize = function(api_key_id = NULL, private_key_path = NULL,
                         logger = NULL) {
      self$api_key_id <- api_key_id
      self$private_key_path <- private_key_path
      self$logger <- logger
      self$api_base <- KALSHI_API_BASE
      self$market_cache <- list()
      self$market_cache_time <- NULL

      if (!is.null(logger)) {
        logger$info("KalshiClient initialized",
                    api_base = self$api_base,
                    has_auth = self$is_authenticated())
      }
    },

    #' Check if client has auth credentials (for portfolio/order endpoints)
    is_authenticated = function() {
      !is.null(self$api_key_id) && nzchar(self$api_key_id) &&
        !is.null(self$private_key_path) && nzchar(self$private_key_path) &&
        file.exists(self$private_key_path)
    },

    #' Make a public (unauthenticated) GET request
    #' Market data endpoints on api.elections.kalshi.com don't require auth
    #' @param endpoint API endpoint (without base), may include query params
    #' @return Response data or NULL on error
    public_request = function(endpoint) {
      url <- paste0(self$api_base, endpoint)

      tryCatch({
        resp <- request(url) %>%
          req_headers(
            "Accept" = "application/json"
          ) %>%
          req_perform()
        resp_body_json(resp)
      }, error = function(e) {
        if (!is.null(self$logger)) {
          self$logger$error(sprintf("Public API request failed: %s", e$message),
                           endpoint = endpoint)
        }
        NULL
      })
    },

    #' Make authenticated API request (for portfolio/order endpoints)
    #' @param endpoint API endpoint (without base), may include query params
    #' @param method HTTP method
    #' @param body Request body (for POST/PUT)
    #' @return Response data
    api_request = function(endpoint, method = "GET", body = NULL) {
      if (!self$is_authenticated()) {
        if (!is.null(self$logger)) {
          self$logger$error("Kalshi client not configured (missing API key or private key file)")
        }
        return(NULL)
      }

      # Strip query params from path for signing
      path_no_query <- sub("\\?.*$", "", endpoint)

      # Timestamp in milliseconds
      timestamp_ms <- sprintf("%.0f", as.numeric(Sys.time()) * 1000)

      # Build message: timestamp_ms + METHOD + path (no query)
      message_string <- paste0(timestamp_ms, toupper(method), "/trade-api/v2", path_no_query)

      if (!is.null(self$logger)) {
        self$logger$debug(sprintf("[AUTH] Signing: %s...%s (ts=%s)",
                                  substr(message_string, 1, 20),
                                  substr(message_string, nchar(message_string) - 30, nchar(message_string)),
                                  timestamp_ms))
      }

      # Sign the message
      signature <- tryCatch({
        sign_rsa_pss(message_string, self$private_key_path)
      }, error = function(e) {
        if (!is.null(self$logger)) {
          self$logger$error(sprintf("RSA-PSS signing failed: %s", e$message))
        }
        return(NULL)
      })

      if (is.null(signature)) return(NULL)

      url <- paste0(self$api_base, endpoint)

      req <- request(url) %>%
        req_method(method) %>%
        req_headers(
          "KALSHI-ACCESS-KEY" = self$api_key_id,
          "KALSHI-ACCESS-TIMESTAMP" = timestamp_ms,
          "KALSHI-ACCESS-SIGNATURE" = signature,
          "Content-Type" = "application/json"
        )

      if (!is.null(body)) {
        req <- req %>% req_body_json(body)
      }

      tryCatch({
        resp <- req %>% req_perform()
        resp_body_json(resp)
      }, error = function(e) {
        if (!is.null(self$logger)) {
          self$logger$error(sprintf("API request failed: %s", e$message),
                           endpoint = endpoint)
        }
        NULL
      })
    },

    #' Test API connectivity with a lightweight public call
    #' @return TRUE if the API is reachable and returning data
    test_connectivity = function() {
      tryCatch({
        response <- self$public_request(
          sprintf("/markets?limit=1&series_ticker=%s", KALSHI_NBA_SERIES)
        )
        !is.null(response) && is.list(response$markets)
      }, error = function(e) {
        FALSE
      })
    },

    #' Get account balance (requires auth)
    #' @return Balance in dollars
    get_balance = function() {
      response <- self$api_request("/portfolio/balance")
      if (is.null(response)) return(NULL)
      response$balance / 100
    },

    #' Fetch NBA game winner markets (public, no auth needed)
    #' @param status Market status filter ("open", "settled", etc.)
    #' @return List of NBA game markets with bid/ask data
    get_nba_markets = function(status = "open") {
      # Return cache if still fresh
      if (!is.null(self$market_cache_time) &&
          as.numeric(difftime(Sys.time(), self$market_cache_time, units = "secs")) < self$market_cache_ttl &&
          length(self$market_cache) > 0) {
        return(self$market_cache)
      }

      all_markets <- list()
      cursor <- NULL

      repeat {
        endpoint <- sprintf("/markets?status=%s&series_ticker=%s&limit=200",
                           status, KALSHI_NBA_SERIES)
        if (!is.null(cursor) && nzchar(cursor)) {
          endpoint <- paste0(endpoint, "&cursor=", cursor)
        }

        response <- self$public_request(endpoint)

        if (is.null(response)) break

        page_markets <- response$markets %||% list()
        all_markets <- c(all_markets, page_markets)

        cursor <- response$cursor %||% ""
        if (!nzchar(cursor)) break
      }

      # Update cache
      self$market_cache <- all_markets
      self$market_cache_time <- Sys.time()

      if (!is.null(self$logger) && length(all_markets) > 0) {
        self$logger$info(sprintf("[MARKET] Fetched %d NBA game markets", length(all_markets)))
      }

      all_markets
    },

    #' Get specific market by ticker (public, no auth needed)
    #' @param ticker Market ticker
    #' @return Market details
    get_market = function(ticker) {
      self$public_request(sprintf("/markets/%s", ticker))
    },

    #' Get market orderbook (public, no auth needed)
    #' New API format: arrays of [price_cents, quantity] pairs
    #' @param ticker Market ticker
    #' @return Orderbook data
    get_orderbook = function(ticker) {
      self$public_request(sprintf("/markets/%s/orderbook", ticker))
    },

    #' Get orderbook depth near a price
    #' Calls get_orderbook() and sums available contracts within a price range.
    #' @param ticker Market ticker
    #' @param side "yes" or "no" — which side's book to read
    #' @param near_price Target price in dollars (0-1). If NULL, uses best price.
    #' @param range_cents How many cents around near_price to include (default 3)
    #' @return List with total_contracts, total_dollars, levels, or NULL on error
    get_orderbook_depth = function(ticker, side = "yes", near_price = NULL, range_cents = 3) {
      ob <- tryCatch({
        self$get_orderbook(ticker)
      }, error = function(e) {
        if (!is.null(self$logger)) {
          self$logger$debug(sprintf("Orderbook fetch failed for %s: %s", ticker, e$message))
        }
        NULL
      })

      if (is.null(ob) || !is.list(ob)) return(NULL)

      # The orderbook response has orderbook_fp with yes_dollars and no_dollars
      # Each is an array of [price_cents, quantity] pairs
      ob_data <- ob$orderbook_fp %||% ob$orderbook %||% ob
      book_key <- if (side == "yes") "yes_dollars" else "no_dollars"
      levels <- ob_data[[book_key]]

      if (is.null(levels) || length(levels) == 0) {
        return(list(total_contracts = 0, total_dollars = 0, levels = 0))
      }

      total_contracts <- 0
      total_dollars <- 0
      level_count <- 0

      for (level in levels) {
        if (!is.numeric(level) || length(level) < 2) next
        price_cents <- level[1]
        quantity <- level[2]
        price_dollars <- price_cents / 100

        # If near_price specified, only count levels within range
        if (!is.null(near_price)) {
          if (abs(price_dollars - near_price) > (range_cents / 100)) next
        }

        total_contracts <- total_contracts + quantity
        total_dollars <- total_dollars + (quantity * price_dollars)
        level_count <- level_count + 1
      }

      list(
        total_contracts = total_contracts,
        total_dollars = total_dollars,
        levels = level_count
      )
    },

    #' Get fresh spread data bypassing cache (for trade execution)
    #' Calls get_market(ticker) directly for a live /markets/{ticker} response.
    #' @param ticker Market ticker
    #' @return List with bid, ask, mid, spread info, source = "kalshi_fresh"
    get_fresh_spread_data = function(ticker) {
      resp <- self$get_market(ticker)
      if (is.null(resp) || !is.list(resp)) return(NULL)
      market_data <- resp$market %||% resp

      if (is.null(market_data) || !is.list(market_data)) return(NULL)

      # Extract bid/ask from market listing (already in cents)
      yes_bid <- market_data$yes_bid %||% NA
      yes_ask <- market_data$yes_ask %||% NA

      # Convert cents to dollars
      best_bid <- if (!is.null(yes_bid) && !is.na(yes_bid)) yes_bid / 100 else NA
      best_ask <- if (!is.null(yes_ask) && !is.na(yes_ask)) yes_ask / 100 else NA

      # Calculate mid price and spread
      mid_price <- if (!is.na(best_bid) && !is.na(best_ask)) {
        (best_bid + best_ask) / 2
      } else if (!is.na(best_bid)) {
        best_bid
      } else if (!is.na(best_ask)) {
        best_ask
      } else {
        lp <- market_data$last_price %||% 50
        lp / 100
      }

      spread <- if (!is.na(best_bid) && !is.na(best_ask)) {
        best_ask - best_bid
      } else {
        NA
      }

      spread_pct <- if (!is.na(spread) && mid_price > 0) {
        spread / mid_price * 100
      } else {
        NA
      }

      list(
        best_bid = best_bid,
        best_ask = best_ask,
        mid_price = mid_price,
        spread = spread,
        spread_pct = spread_pct,
        buy_yes_price = best_ask,
        buy_no_price = if (!is.na(best_bid)) 1 - best_bid else NA,
        yes_prob_mid = mid_price,
        no_prob_mid = 1 - mid_price,
        # Volume/liquidity fields (already in market response)
        volume_24h = market_data$volume_24h %||% NA,
        open_interest = market_data$open_interest %||% NA,
        yes_bid_size = as.numeric(market_data$yes_bid_size_fp %||% NA),
        yes_ask_size = as.numeric(market_data$yes_ask_size_fp %||% NA),
        source = "kalshi_fresh"
      )
    },

    #' Get spread data from market listing (no orderbook call needed)
    #' Uses the yes_bid/yes_ask fields already present in market data
    #' @param ticker Market ticker
    #' @return List with bid, ask, mid, spread, and depth info
    get_spread_data = function(ticker) {
      # First, try to find data in cached market listing
      market_data <- NULL
      for (m in self$market_cache) {
        if (!is.list(m)) next
        if (identical(m$ticker, ticker)) {
          market_data <- m
          break
        }
      }

      # If not in cache, fetch the individual market
      if (is.null(market_data)) {
        resp <- self$get_market(ticker)
        if (is.null(resp) || !is.list(resp)) return(NULL)
        market_data <- resp$market %||% resp
      }

      if (is.null(market_data) || !is.list(market_data)) {
        return(NULL)
      }

      # Extract bid/ask from market listing (already in cents)
      yes_bid <- market_data$yes_bid %||% NA
      yes_ask <- market_data$yes_ask %||% NA
      no_bid <- market_data$no_bid %||% NA
      no_ask <- market_data$no_ask %||% NA

      # Convert cents to dollars
      best_bid <- if (!is.null(yes_bid) && !is.na(yes_bid)) yes_bid / 100 else NA
      best_ask <- if (!is.null(yes_ask) && !is.na(yes_ask)) yes_ask / 100 else NA

      # Calculate mid price and spread
      mid_price <- if (!is.na(best_bid) && !is.na(best_ask)) {
        (best_bid + best_ask) / 2
      } else if (!is.na(best_bid)) {
        best_bid
      } else if (!is.na(best_ask)) {
        best_ask
      } else {
        # Try last_price as fallback
        lp <- market_data$last_price %||% 50
        lp / 100
      }

      spread <- if (!is.na(best_bid) && !is.na(best_ask)) {
        best_ask - best_bid
      } else {
        NA
      }

      spread_pct <- if (!is.na(spread) && mid_price > 0) {
        spread / mid_price * 100
      } else {
        NA
      }

      list(
        best_bid = best_bid,
        best_ask = best_ask,
        mid_price = mid_price,
        spread = spread,
        spread_pct = spread_pct,
        # Execution prices (what you'd actually pay)
        buy_yes_price = best_ask,         # To buy YES, you pay the ask
        buy_no_price = if (!is.na(best_bid)) 1 - best_bid else NA,  # To buy NO
        # Implied probabilities
        yes_prob_mid = mid_price,
        no_prob_mid = 1 - mid_price,
        # Volume/liquidity fields (already in market response)
        volume_24h = market_data$volume_24h %||% NA,
        open_interest = market_data$open_interest %||% NA,
        yes_bid_size = as.numeric(market_data$yes_bid_size_fp %||% NA),
        yes_ask_size = as.numeric(market_data$yes_ask_size_fp %||% NA),
        source = "kalshi"
      )
    },

    #' Get best available price for a side
    #' @param ticker Market ticker
    #' @param side "yes" or "no"
    #' @return Best price and quantity
    get_best_price = function(ticker, side = "yes") {
      spread_data <- self$get_spread_data(ticker)
      if (is.null(spread_data)) return(NULL)

      if (side == "yes") {
        if (!is.na(spread_data$best_ask)) {
          return(list(price = spread_data$best_ask, quantity = NA))
        }
      } else {
        if (!is.na(spread_data$buy_no_price)) {
          return(list(price = spread_data$buy_no_price, quantity = NA))
        }
      }

      NULL
    },

    #' Calculate actual edge accounting for bid-ask spread
    #' @param ticker Market ticker
    #' @param model_prob Model's probability for YES winning
    #' @param side "yes" or "no" (which side you're betting)
    #' @param kalshi_fee Kalshi fee rate on wins (default 7%)
    #' @return List with actual edge, execution price, and recommendation
    calculate_edge_with_spread = function(ticker, model_prob, side = "yes", kalshi_fee = 0.07) {
      spread_data <- self$get_spread_data(ticker)

      if (is.null(spread_data)) {
        return(list(
          edge = NA,
          execution_price = NA,
          mid_price = NA,
          spread = NA,
          recommendation = "NO_DATA"
        ))
      }

      if (side == "yes") {
        execution_price <- spread_data$buy_yes_price
        model_win_prob <- model_prob
      } else {
        execution_price <- spread_data$buy_no_price
        model_win_prob <- 1 - model_prob
      }

      if (is.na(execution_price)) {
        return(list(
          edge = NA,
          execution_price = NA,
          mid_price = spread_data$mid_price,
          spread = spread_data$spread,
          recommendation = "NO_LIQUIDITY"
        ))
      }

      payout_if_win <- 1 - kalshi_fee * (1 - execution_price)
      breakeven_prob <- execution_price / payout_if_win
      actual_edge <- model_win_prob - breakeven_prob

      mid_execution <- if (side == "yes") spread_data$mid_price else (1 - spread_data$mid_price)
      mid_payout <- 1 - kalshi_fee * (1 - mid_execution)
      mid_breakeven <- mid_execution / mid_payout
      mid_edge <- model_win_prob - mid_breakeven
      edge_lost_to_spread <- mid_edge - actual_edge

      recommendation <- if (actual_edge >= 0.05) {
        "STRONG_BET"
      } else if (actual_edge >= 0.02) {
        "MARGINAL_BET"
      } else if (actual_edge > 0) {
        "WEAK_EDGE"
      } else {
        "NO_EDGE"
      }

      list(
        edge = actual_edge,
        edge_mid = mid_edge,
        edge_lost_to_spread = edge_lost_to_spread,
        execution_price = execution_price,
        mid_price = spread_data$mid_price,
        spread = spread_data$spread,
        spread_pct = spread_data$spread_pct,
        breakeven_prob = breakeven_prob,
        model_prob = model_win_prob,
        recommendation = recommendation
      )
    },

    #' Place an order (requires auth)
    #' @param ticker Market ticker
    #' @param side "yes" or "no"
    #' @param action "buy" or "sell"
    #' @param count Number of contracts
    #' @param price Limit price (in dollars, 0-1)
    #' @return Order response
    place_order = function(ticker, side, action, count, price) {
      price_cents <- round(price * 100)

      body <- list(
        ticker = ticker,
        action = action,
        side = side,
        count = count,
        type = "limit",
        yes_price = if (side == "yes") price_cents else NULL,
        no_price = if (side == "no") price_cents else NULL
      )

      body <- body[!sapply(body, is.null)]

      response <- self$api_request("/portfolio/orders", method = "POST", body = body)

      if (!is.null(self$logger) && !is.null(response)) {
        self$logger$info("Order placed",
                        ticker = ticker, side = side,
                        count = count, price = price)
      }

      response
    },

    #' Cancel an order (requires auth)
    cancel_order = function(order_id) {
      self$api_request(sprintf("/portfolio/orders/%s", order_id), method = "DELETE")
    },

    #' Get open orders (requires auth)
    get_open_orders = function() {
      response <- self$api_request("/portfolio/orders?status=resting")
      response$orders %||% list()
    },

    #' Get positions (requires auth)
    get_positions = function() {
      response <- self$api_request("/portfolio/positions")
      response$market_positions %||% list()
    },

    #' Get fills (requires auth)
    get_fills = function(ticker = NULL) {
      endpoint <- "/portfolio/fills"
      if (!is.null(ticker)) {
        endpoint <- sprintf("%s?ticker=%s", endpoint, ticker)
      }
      response <- self$api_request(endpoint)
      response$fills %||% list()
    },

    #' Find NBA game market tickers for a game
    #' Ticker format: KXNBAGAME-{YY}{MON}{DD}{AWAY}{HOME}-{TEAM}
    #' Returns the HOME team's ticker (YES = home wins)
    #' @param game_id ESPN game ID
    #' @param home_team Home team abbreviation (ESPN format)
    #' @param away_team Away team abbreviation (ESPN format)
    #' @param game_date Game date
    #' @return Market ticker or NULL
    find_game_market = function(game_id, home_team, away_team, game_date) {
      markets <- self$get_nba_markets()

      if (length(markets) == 0) {
        if (!is.null(self$logger)) {
          self$logger$info("[MARKET] No open NBA game markets found on Kalshi")
        }
        return(NULL)
      }

      # Normalize ESPN abbreviations to Kalshi format
      kalshi_home <- normalize_team_abbrev(home_team)
      kalshi_away <- normalize_team_abbrev(away_team)

      # Build expected date portion: YY + MON + DD (e.g., "26FEB22")
      game_dt <- as.Date(game_date)
      date_str <- toupper(format(game_dt, "%y%b%d"))  # e.g. "26FEB22"

      # Strategy 1: Build expected ticker directly
      # Format: KXNBAGAME-{date}{away}{home}-{home}
      expected_home_ticker <- sprintf("KXNBAGAME-%s%s%s-%s",
                                      date_str, kalshi_away, kalshi_home, kalshi_home)

      for (market in markets) {
        if (toupper(market$ticker %||% "") == expected_home_ticker) {
          if (!is.null(self$logger)) {
            self$logger$info(sprintf("[MARKET] Exact match: %s vs %s -> %s",
                                    away_team, home_team, market$ticker))
          }
          return(market$ticker)
        }
      }

      # Strategy 2: Fuzzy match — look for date AND both teams in ticker
      for (market in markets) {
        ticker <- toupper(market$ticker %||% "")
        has_date <- grepl(date_str, ticker)
        has_home <- grepl(kalshi_home, ticker)
        has_away <- grepl(kalshi_away, ticker)

        # Match: has date, has both teams, and ends with home team (home winner market)
        if (has_date && has_home && has_away && grepl(paste0(kalshi_home, "$"), ticker)) {
          if (!is.null(self$logger)) {
            self$logger$info(sprintf("[MARKET] Fuzzy match: %s vs %s -> %s",
                                    away_team, home_team, market$ticker))
          }
          return(market$ticker)
        }
      }

      # No Strategy 3 — matching on date+home alone produces false positives
      # (e.g., BOS@LAL matching ORL@LAL). Require both teams to match.

      if (!is.null(self$logger)) {
        self$logger$info(sprintf("[MARKET] No market found for %s @ %s on %s (searched %d markets, expected: %s)",
                                kalshi_away, kalshi_home, as.character(game_date),
                                length(markets), expected_home_ticker))
      }
      NULL
    }
  )
)

#' Create a Kalshi client from config
#' @param config_path Path to config file
#' @param logger Logger instance
#' @return KalshiClient instance
create_kalshi_client <- function(config_path = "config/config.yml",
                                  logger = NULL) {
  api_key_id <- NULL
  private_key_path <- NULL

  # Try config file first
  if (file.exists(config_path)) {
    config <- yaml::read_yaml(config_path)
    api_key_id <- config$kalshi$api_key_id
    private_key_path <- config$kalshi$private_key_path
  }

  # Fall back to env vars if config fields are empty
  if (is.null(api_key_id) || !nzchar(api_key_id)) {
    api_key_id <- Sys.getenv("KALSHI_API_KEY_ID", unset = "")
  }
  if (is.null(private_key_path) || !nzchar(private_key_path)) {
    private_key_path <- Sys.getenv("KALSHI_PRIVATE_KEY_PATH", unset = "")
  }

  # Empty strings -> NULL so is_authenticated() knows creds are missing
  if (!nzchar(api_key_id)) api_key_id <- NULL
  if (!nzchar(private_key_path)) private_key_path <- NULL

  KalshiClient$new(
    api_key_id = api_key_id,
    private_key_path = private_key_path,
    logger = logger
  )
}
