# Kalshi API Endpoint Diagnostic
# Read-only connectivity/sanity check for the endpoints the trading app relies on.
# Does NOT place or cancel orders (those mutate real account/market state).

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) setwd(args[1])

library(httr2)
library(jsonlite)
library(R6)
library(yaml)

source("R/logging.R")
source("R/kalshi_api.R")

results <- list()
record <- function(name, ok, detail = "") {
  results[[length(results) + 1]] <<- list(name = name, ok = ok, detail = detail)
  cat(sprintf("[%s] %s%s\n", if (ok) "PASS" else "FAIL", name,
              if (nzchar(detail)) paste0(" — ", detail) else ""))
}

cat("=== Kalshi API Endpoint Diagnostic ===\n\n")

logger <- create_logger(log_dir = "logs", level = "INFO")
client <- tryCatch(
  create_kalshi_client(config_path = "config/config.yml", logger = logger),
  error = function(e) NULL
)

if (is.null(client)) {
  record("create_kalshi_client", FALSE, "client construction failed")
  quit(status = 1)
}
record("create_kalshi_client", TRUE, sprintf("api_base=%s", client$api_base))

auth <- client$is_authenticated()
cat(sprintf("\nAuthenticated (key+private key file present): %s\n\n", auth))

## --- Public (no-auth) endpoints ---

ok <- tryCatch(isTRUE(client$test_connectivity()), error = function(e) FALSE)
record("test_connectivity()", ok)

markets <- tryCatch(client$get_nba_markets(), error = function(e) {
  record("get_nba_markets() [exception]", FALSE, e$message); list()
})
record("get_nba_markets()", length(markets) > 0, sprintf("%d markets returned", length(markets)))

ticker <- NULL
if (length(markets) > 0) {
  ticker <- markets[[1]]$ticker
  cat(sprintf("Using sample ticker: %s\n\n", ticker))
}

if (!is.null(ticker)) {
  m <- tryCatch(client$get_market(ticker), error = function(e) NULL)
  record("get_market(ticker)", !is.null(m$market) || !is.null(m))

  ob <- tryCatch(client$get_orderbook(ticker), error = function(e) NULL)
  record("get_orderbook(ticker)", !is.null(ob))

  sd <- tryCatch(client$get_spread_data(ticker), error = function(e) NULL)
  record("get_spread_data(ticker)", !is.null(sd),
         if (!is.null(sd)) sprintf("bid=%.2f ask=%.2f mid=%.2f", sd$best_bid, sd$best_ask, sd$mid_price) else "")

  fsd <- tryCatch(client$get_fresh_spread_data(ticker), error = function(e) NULL)
  record("get_fresh_spread_data(ticker)", !is.null(fsd))

  bp_yes <- tryCatch(client$get_best_price(ticker, "yes"), error = function(e) NULL)
  record("get_best_price(ticker, 'yes')", !is.null(bp_yes))

  bp_no <- tryCatch(client$get_best_price(ticker, "no"), error = function(e) NULL)
  record("get_best_price(ticker, 'no')", !is.null(bp_no))
} else {
  record("get_market/orderbook/spread (skipped)", FALSE, "no ticker available to test with")
}

## --- Authenticated, read-only endpoints (safe: no money moves) ---

if (auth) {
  bal <- tryCatch(client$get_balance(), error = function(e) NULL)
  record("get_balance()", !is.null(bal), if (!is.null(bal)) sprintf("$%.2f", bal) else "")

  pos <- tryCatch(client$get_positions(), error = function(e) NULL)
  record("get_positions()", !is.null(pos), sprintf("%d positions", length(pos %||% list())))

  oo <- tryCatch(client$get_open_orders(), error = function(e) NULL)
  record("get_open_orders()", !is.null(oo), sprintf("%d open orders", length(oo %||% list())))

  fl <- tryCatch(client$get_fills(), error = function(e) NULL)
  record("get_fills()", !is.null(fl), sprintf("%d fills", length(fl %||% list())))
} else {
  cat("\nSkipping authenticated endpoints (get_balance/get_positions/get_open_orders/get_fills) — no API key/private key configured.\n")
}

cat("\nSkipping place_order() / cancel_order() intentionally — these mutate real account/market state and are not exercised by this diagnostic.\n")

## --- Summary ---

cat("\n=== Summary ===\n")
n_pass <- sum(sapply(results, function(r) r$ok))
n_fail <- sum(sapply(results, function(r) !r$ok))
cat(sprintf("%d passed, %d failed\n", n_pass, n_fail))
if (n_fail > 0) {
  cat("\nFailed checks:\n")
  for (r in results) if (!r$ok) cat(sprintf("  - %s %s\n", r$name, if (nzchar(r$detail)) paste0("(", r$detail, ")") else ""))
}
