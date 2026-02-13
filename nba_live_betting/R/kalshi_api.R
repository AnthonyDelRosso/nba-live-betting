# Kalshi API Client
# Handles RSA-PSS API key authentication and API calls to Kalshi

library(R6)
library(httr2)
library(jsonlite)
library(tidyverse)

# Kalshi API endpoints
KALSHI_API_BASE <- "https://trading-api.kalshi.com/trade-api/v2"

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
#' The R openssl package only supports PKCS#1 v1.5, so we shell out.
#' @param message_string The message to sign (timestamp_ms + METHOD + path)
#' @param private_key_path Path to PEM private key file
#' @return Base64-encoded signature string
sign_rsa_pss <- function(message_string, private_key_path) {
  # Find openssl binary (cached after first lookup)
  if (is.null(.openssl_bin)) {
    .openssl_bin <<- find_openssl_binary()
    message(sprintf("[KALSHI] Using openssl at: %s", .openssl_bin))
  }

  msg_file <- tempfile(fileext = ".bin")
  sig_file <- tempfile(fileext = ".sig")
  on.exit(unlink(c(msg_file, sig_file)), add = TRUE)

  writeBin(charToRaw(message_string), msg_file)

  exit_code <- system2(
    .openssl_bin, c(
      "dgst", "-sha256",
      "-sigopt", "rsa_padding_mode:pss",
      "-sigopt", "rsa_pss_saltlen:32",
      "-sign", shQuote(private_key_path),
      "-out", shQuote(sig_file),
      shQuote(msg_file)
    ),
    stdout = FALSE, stderr = FALSE
  )

  if (exit_code != 0 || !file.exists(sig_file)) {
    stop("openssl RSA-PSS signing failed (exit code ", exit_code, ")")
  }

  sig_bytes <- readBin(sig_file, "raw", file.info(sig_file)$size)
  openssl::base64_encode(sig_bytes)
}

#' KalshiClient R6 Class
#' Handles RSA-PSS per-request signing and API calls to Kalshi
KalshiClient <- R6::R6Class(
  "KalshiClient",

  public = list(
    api_base = NULL,
    api_key_id = NULL,
    private_key_path = NULL,
    logger = NULL,

    #' Initialize the Kalshi client
    #' @param api_key_id Kalshi API key ID
    #' @param private_key_path Path to RSA private key PEM file
    #' @param logger Logger instance
    initialize = function(api_key_id = NULL, private_key_path = NULL,
                         logger = NULL) {
      self$api_key_id <- api_key_id
      self$private_key_path <- private_key_path
      self$logger <- logger
      self$api_base <- KALSHI_API_BASE

      if (!is.null(logger)) {
        logger$info("KalshiClient initialized (RSA-PSS auth)")
      }
    },

    #' Check if client is ready (API key and private key file exist)
    is_authenticated = function() {
      !is.null(self$api_key_id) && nzchar(self$api_key_id) &&
        !is.null(self$private_key_path) && nzchar(self$private_key_path) &&
        file.exists(self$private_key_path)
    },

    #' Make authenticated API request (each request is individually signed)
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
      timestamp_ms <- as.character(as.integer(as.numeric(Sys.time()) * 1000))

      # Build message: timestamp_ms + METHOD + path (no query)
      message_string <- paste0(timestamp_ms, toupper(method), "/trade-api/v2", path_no_query)

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

    #' Get account balance
    #' @return Balance in dollars
    get_balance = function() {
      response <- self$api_request("/portfolio/balance")

      if (is.null(response)) {
        return(NULL)
      }

      # Balance is in cents
      response$balance / 100
    },

    #' Search for NBA markets (handles pagination)
    #' @param status Market status filter ("open", "closed", etc.)
    #' @return List of NBA markets
    get_nba_markets = function(status = "open") {
      all_markets <- list()
      cursor <- NULL

      repeat {
        endpoint <- sprintf("/markets?status=%s&series_ticker=NBA&limit=200", status)
        if (!is.null(cursor) && nzchar(cursor)) {
          endpoint <- paste0(endpoint, "&cursor=", cursor)
        }

        response <- self$api_request(endpoint)

        if (is.null(response)) break

        page_markets <- response$markets %||% list()
        all_markets <- c(all_markets, page_markets)

        # Check for next page
        cursor <- response$cursor %||% ""
        if (!nzchar(cursor)) break
      }

      all_markets
    },

    #' Get specific market by ticker
    #' @param ticker Market ticker
    #' @return Market details
    get_market = function(ticker) {
      self$api_request(sprintf("/markets/%s", ticker))
    },

    #' Get market orderbook
    #' @param ticker Market ticker
    #' @return Orderbook with bids and asks
    get_orderbook = function(ticker) {
      self$api_request(sprintf("/markets/%s/orderbook", ticker))
    },

    #' Get best available price for a side
    #' @param ticker Market ticker
    #' @param side "yes" or "no"
    #' @return Best price and quantity
    get_best_price = function(ticker, side = "yes") {
      orderbook <- self$get_orderbook(ticker)

      if (is.null(orderbook)) {
        return(NULL)
      }

      if (side == "yes") {
        # Best ask for buying YES
        asks <- orderbook$asks %||% list()
        if (length(asks) > 0) {
          # Price is in cents
          return(list(
            price = asks[[1]]$price / 100,
            quantity = asks[[1]]$quantity
          ))
        }
      } else {
        # Best bid for selling YES (buying NO)
        bids <- orderbook$bids %||% list()
        if (length(bids) > 0) {
          return(list(
            price = (100 - bids[[1]]$price) / 100,  # NO price
            quantity = bids[[1]]$quantity
          ))
        }
      }

      NULL
    },

    #' Get full bid-ask spread data
    #' @param ticker Market ticker
    #' @return List with bid, ask, mid, spread, and depth info
    get_spread_data = function(ticker) {
      orderbook <- self$get_orderbook(ticker)

      if (is.null(orderbook)) {
        return(NULL)
      }

      bids <- orderbook$bids %||% list()
      asks <- orderbook$asks %||% list()

      # Best bid (highest buy order for YES)
      best_bid <- if (length(bids) > 0) bids[[1]]$price / 100 else NA
      best_bid_qty <- if (length(bids) > 0) bids[[1]]$quantity else 0

      # Best ask (lowest sell order for YES)
      best_ask <- if (length(asks) > 0) asks[[1]]$price / 100 else NA
      best_ask_qty <- if (length(asks) > 0) asks[[1]]$quantity else 0

      # Calculate mid price and spread
      mid_price <- if (!is.na(best_bid) && !is.na(best_ask)) {
        (best_bid + best_ask) / 2
      } else if (!is.na(best_bid)) {
        best_bid
      } else if (!is.na(best_ask)) {
        best_ask
      } else {
        0.5  # Default
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

      # Calculate total depth at top 3 levels
      bid_depth <- sum(sapply(head(bids, 3), function(x) x$quantity %||% 0))
      ask_depth <- sum(sapply(head(asks, 3), function(x) x$quantity %||% 0))

      list(
        best_bid = best_bid,
        best_ask = best_ask,
        best_bid_qty = best_bid_qty,
        best_ask_qty = best_ask_qty,
        mid_price = mid_price,
        spread = spread,
        spread_pct = spread_pct,
        bid_depth = bid_depth,
        ask_depth = ask_depth,
        # Execution prices (what you'd actually pay)
        buy_yes_price = best_ask,      # To buy YES, you pay the ask
        buy_no_price = 1 - best_bid,   # To buy NO, you pay 1 - bid
        # Implied probabilities
        yes_prob_mid = mid_price,
        no_prob_mid = 1 - mid_price
      )
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

      # Get actual execution price (what you'd pay)
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

      # Calculate breakeven probability accounting for fee
      # You pay execution_price, receive $1 * (1 - fee) if you win
      # Breakeven: execution_price = win_prob * (1 - fee)
      # So: breakeven_prob = execution_price / (1 - fee)
      breakeven_prob <- execution_price / (1 - kalshi_fee)

      # Actual edge = model probability - breakeven probability
      actual_edge <- model_win_prob - breakeven_prob

      # Compare to edge if we used mid price
      mid_execution <- if (side == "yes") spread_data$mid_price else (1 - spread_data$mid_price)
      mid_breakeven <- mid_execution / (1 - kalshi_fee)
      mid_edge <- model_win_prob - mid_breakeven

      # Edge lost to spread
      edge_lost_to_spread <- mid_edge - actual_edge

      # Recommendation
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
        bid_depth = spread_data$bid_depth,
        ask_depth = spread_data$ask_depth,
        recommendation = recommendation
      )
    },

    #' Place an order
    #' @param ticker Market ticker
    #' @param side "yes" or "no"
    #' @param action "buy" or "sell"
    #' @param count Number of contracts
    #' @param price Limit price (in dollars, 0-1)
    #' @return Order response
    place_order = function(ticker, side, action, count, price) {
      # Convert price to cents
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

      # Remove NULL fields
      body <- body[!sapply(body, is.null)]

      response <- self$api_request("/portfolio/orders", method = "POST", body = body)

      if (!is.null(self$logger) && !is.null(response)) {
        self$logger$info("Order placed",
                        ticker = ticker,
                        side = side,
                        count = count,
                        price = price)
      }

      response
    },

    #' Cancel an order
    #' @param order_id Order ID to cancel
    #' @return Cancellation response
    cancel_order = function(order_id) {
      self$api_request(sprintf("/portfolio/orders/%s", order_id), method = "DELETE")
    },

    #' Get open orders
    #' @return List of open orders
    get_open_orders = function() {
      response <- self$api_request("/portfolio/orders?status=resting")
      response$orders %||% list()
    },

    #' Get positions
    #' @return List of positions
    get_positions = function() {
      response <- self$api_request("/portfolio/positions")
      response$market_positions %||% list()
    },

    #' Get fills (executed trades)
    #' @param ticker Optional market ticker filter
    #' @return List of fills
    get_fills = function(ticker = NULL) {
      endpoint <- "/portfolio/fills"
      if (!is.null(ticker)) {
        endpoint <- sprintf("%s?ticker=%s", endpoint, ticker)
      }
      response <- self$api_request(endpoint)
      response$fills %||% list()
    },

    #' Find NBA game market ticker
    #' Maps game_id to Kalshi market ticker
    #' @param game_id ESPN game ID
    #' @param home_team Home team abbreviation
    #' @param away_team Away team abbreviation
    #' @param game_date Game date
    #' @return Market ticker or NULL
    find_game_market = function(game_id, home_team, away_team, game_date) {
      markets <- self$get_nba_markets()

      if (length(markets) == 0) {
        if (!is.null(self$logger)) {
          self$logger$info("[MARKET] No open NBA markets found on Kalshi")
        }
        return(NULL)
      }

      # Kalshi tickers typically look like: KXNBA-26JAN15-DET or NBA-DET-26JAN15
      # Build date patterns to match against ticker
      game_dt <- as.Date(game_date)
      date_str_upper <- toupper(format(game_dt, "%d%b%y"))   # e.g. "15JAN26"
      date_str_ymd <- format(game_dt, "%y%b%d")              # e.g. "26JAN15"
      date_iso <- format(game_dt, "%Y-%m-%d")                # for title matching

      # First pass: match team AND date (strongest match)
      for (market in markets) {
        ticker <- toupper(market$ticker %||% "")
        title <- market$title %||% ""

        has_team <- grepl(toupper(home_team), ticker) || grepl(toupper(away_team), ticker)
        has_date <- grepl(date_str_upper, ticker) || grepl(date_str_ymd, ticker)

        if (has_team && has_date) {
          if (!is.null(self$logger)) {
            self$logger$info(sprintf("[MARKET] Matched %s vs %s -> %s (team+date)",
                                    away_team, home_team, market$ticker))
          }
          return(market$ticker)
        }
      }

      # Second pass: match team in ticker + date in title/subtitle
      for (market in markets) {
        ticker <- toupper(market$ticker %||% "")
        title <- market$title %||% ""
        subtitle <- market$subtitle %||% ""

        has_team <- grepl(toupper(home_team), ticker) || grepl(toupper(away_team), ticker)
        has_date_title <- grepl(date_iso, title) || grepl(date_iso, subtitle) ||
                          grepl(format(game_dt, "%b %d"), title, ignore.case = TRUE)

        if (has_team && has_date_title) {
          if (!is.null(self$logger)) {
            self$logger$info(sprintf("[MARKET] Matched %s vs %s -> %s (team+title date)",
                                    away_team, home_team, market$ticker))
          }
          return(market$ticker)
        }
      }

      # Third pass: team-only fallback (less reliable, may match wrong date)
      for (market in markets) {
        ticker <- toupper(market$ticker %||% "")

        if (grepl(toupper(home_team), ticker) || grepl(toupper(away_team), ticker)) {
          if (!is.null(self$logger)) {
            self$logger$warn(sprintf("[MARKET] Weak match %s vs %s -> %s (team only, no date confirm)",
                                     away_team, home_team, market$ticker))
          }
          return(market$ticker)
        }
      }

      if (!is.null(self$logger)) {
        self$logger$info(sprintf("[MARKET] No market found for %s vs %s on %s",
                                away_team, home_team, as.character(game_date)))
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
