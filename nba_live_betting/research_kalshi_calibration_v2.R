# Step 2 (corrected) of the spread-straddle research.
#
# Fixes applied after review:
# - Use quoted bid/ask, not last-trade price (a single degenerate odd-lot
#   fill can print wildly off the real book — confirmed directly: a $0.01
#   trade with 0.09 contracts behind it occurred while the real ask was
#   0.54-0.69 on four separate markets).
# - Two snapshot rules depending on when a bracket actually exists, since
#   strikes are created dynamically as a game develops:
#     * "pregame_fixed": for brackets that already exist 60 minutes before
#       tip-off, use the quote nearest that fixed clock time.
#     * "first_stable_quote": for brackets created mid-game (most deep-tail
#       strikes), use the first point with a genuine two-sided quote
#       (bid>0, ask<1) AND at least MIN_CUM_VOLUME contracts traded by then —
#       this screens out the noisy seed-quote period right after a strike
#       opens (confirmed earlier: ask bounced 0.98->0.35->0.98->0.98 before
#       settling) without relying on a single trade print.
# - Preseason games dropped (regular season start confirmed via hoopR:
#   2026-10-20).
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) setwd(args[1])

library(httr2); library(jsonlite); library(tidyverse)

SERIES_TICKER <- if (length(args) >= 2) args[2] else "KXNBASPREAD"
REGULAR_SEASON_START <- as.Date("2026-10-20")
PREGAME_OFFSET_SEC <- 60 * 60   # 60 minutes before tip-off
MIN_CUM_VOLUME <- 20            # contracts, for the first_stable_quote rule
OUT_FILE <- sprintf("calibration_v2_%s.rds", SERIES_TICKER)

source("R/espn_api.R")
source("R/kalshi_api.R")

parse_kalshi_ts <- function(ts) {
  if (is.null(ts) || is.na(ts) || !nzchar(ts)) return(NA_integer_)
  as.integer(as.POSIXct(sub("\\.[0-9]+Z$", "Z", ts), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
}

# ESPN's start_time omits seconds ("2026-11-07T22:00Z"), unlike Kalshi's
# format — try with-seconds first, fall back to without, so a format change
# on either side fails loud (NA) rather than silently matching the wrong
# pattern.
parse_espn_ts <- function(ts) {
  if (is.null(ts) || is.na(ts) || !nzchar(ts)) return(NA_integer_)
  v <- as.integer(as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  if (!is.na(v)) return(v)
  as.integer(as.POSIXct(ts, format = "%Y-%m-%dT%H:%MZ", tz = "UTC"))
}

extract_threshold <- function(title) {
  mm <- regmatches(title, regexpr("[0-9]+\\.[0-9]+", title))
  if (length(mm) == 0 || !nzchar(mm)) return(NA_real_)
  as.numeric(mm)
}

parse_event <- function(ev, series) {
  pat <- paste0(series, "-([0-9]{2})([A-Z]{3})([0-9]{2})([A-Z]{3,4})([A-Z]{3,4})$")
  mm <- regmatches(ev, regexec(pat, ev))[[1]]
  if (length(mm) == 0) return(c(NA, NA, NA))
  yy <- mm[2]; mon <- mm[3]; dd <- mm[4]; away <- mm[5]; home <- mm[6]
  date_str <- tryCatch(format(as.Date(paste0(dd, mon, "20", yy), format = "%d%b%Y"), "%Y-%m-%d"),
                        error = function(e) NA)
  c(date_str, away, home)
}

cat(sprintf("=== Fetching settled markets for %s ===\n", SERIES_TICKER))
all_markets <- list()
cursor <- NULL
repeat {
  req <- request("https://api.elections.kalshi.com/trade-api/v2/markets") %>%
    req_url_query(series_ticker = SERIES_TICKER, status = "settled", limit = 1000,
                  !!!(if (!is.null(cursor) && nzchar(cursor)) list(cursor = cursor) else list()))
  resp <- req %>% req_perform() %>% resp_body_json()
  batch <- resp$markets %||% list()
  all_markets <- c(all_markets, batch)
  cursor <- resp$cursor %||% ""
  if (!nzchar(cursor) || length(batch) == 0) break
}
cat(sprintf("Total settled: %d\n", length(all_markets)))

events <- t(sapply(all_markets, function(m) parse_event(m$event_ticker %||% "", SERIES_TICKER)))
game_date <- as.Date(events[, 1])
away_team <- events[, 2]
home_team <- events[, 3]
threshold <- sapply(all_markets, function(m) extract_threshold(m$title %||% ""))

keep <- !is.na(game_date) & game_date >= REGULAR_SEASON_START
cat(sprintf("Dropping %d preseason/unparseable markets, keeping %d\n", sum(!keep), sum(keep)))

idx_keep <- which(keep)
cat(sprintf("Threshold range kept: %.1f to %.1f\n", min(threshold[idx_keep], na.rm=TRUE), max(threshold[idx_keep], na.rm=TRUE)))

# --- Tip-off lookup: one ESPN scoreboard call per distinct date ---
cat("\n=== Building tip-off time lookup ===\n")
distinct_dates <- unique(game_date[idx_keep])
distinct_dates <- distinct_dates[!is.na(distinct_dates)]
tipoff_lookup <- list()
for (d in distinct_dates) {
  sb <- tryCatch(get_nba_scoreboard(as.Date(d, origin = "1970-01-01")), error = function(e) list(games = list()))
  for (g in sb$games %||% list()) {
    h <- normalize_team_abbrev(g$home_team_abbrev %||% "")
    a <- normalize_team_abbrev(g$away_team_abbrev %||% "")
    key <- paste(as.Date(d, origin = "1970-01-01"), a, h, sep = "_")
    tipoff_lookup[[key]] <- parse_espn_ts(g$start_time)
  }
}
cat(sprintf("Tip-off times found for %d / %d games\n",
            sum(!sapply(tipoff_lookup, is.na)), length(tipoff_lookup)))

get_tipoff <- function(d, away, home) {
  key <- paste(d, away, home, sep = "_")
  tipoff_lookup[[key]] %||% NA_integer_
}

# Sustained high-volume sequential requests get intermittently rate-limited
# (confirmed: most "error" rows succeed fine when retried individually) —
# retry with backoff instead of giving up after one transient failure.
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

# --- Quote snapshot extraction ---
fetch_quote_snapshot <- function(m, cutoff_ts) {
  tryCatch({
    open_ts <- parse_kalshi_ts(m$open_time)
    close_ts <- parse_kalshi_ts(m$close_time)
    if (is.na(open_ts) || is.na(close_ts) || close_ts <= open_ts) {
      return(list(price = NA_real_, mid = NA_real_, method = "no_window", snapshot_ts = NA_real_))
    }

    cs <- fetch_candlesticks_retry(m$ticker, open_ts, close_ts)
    if (is.null(cs)) {
      return(list(price = NA_real_, mid = NA_real_, method = "error", snapshot_ts = NA_real_))
    }
    cands <- cs$candlesticks %||% list()
    if (length(cands) == 0) {
      return(list(price = NA_real_, mid = NA_real_, method = "no_candles", snapshot_ts = NA_real_))
    }

    quote_of <- function(c) {
      bid <- suppressWarnings(as.numeric(c$yes_bid$close_dollars %||% c$yes_bid$open_dollars %||% NA))
      ask <- suppressWarnings(as.numeric(c$yes_ask$close_dollars %||% c$yes_ask$open_dollars %||% NA))
      list(bid = bid, ask = ask)
    }
    # "Two-sided" alone isn't enough: a quote with bid=0.04/ask=0.89 passes
    # that check but is two stale, far-apart resting orders, not genuine
    # price discovery (confirmed: this inflated the recorded "price" well
    # above the true mid on many thin quarter/half-spread markets). Require
    # a reasonably tight spread too.
    MAX_SPREAD <- 0.15
    two_sided <- function(q) !is.na(q$bid) && !is.na(q$ask) && q$bid > 0 && q$ask < 1 &&
      (q$ask - q$bid) <= MAX_SPREAD

    if (!is.na(cutoff_ts) && !is.na(open_ts) && open_ts <= cutoff_ts && cutoff_ts <= close_ts) {
      # Bracket already existed at the fixed pregame cutoff: use the closest candle to it.
      offsets <- abs(sapply(cands, function(c) (c$end_period_ts %||% NA) - cutoff_ts))
      order_idx <- order(offsets)
      for (i in order_idx) {
        q <- quote_of(cands[[i]])
        if (two_sided(q)) {
          return(list(price = q$ask, mid = (q$bid + q$ask) / 2, method = "pregame_fixed",
                      snapshot_ts = cands[[i]]$end_period_ts))
        }
      }
      return(list(price = NA_real_, mid = NA_real_, method = "pregame_fixed_no_quote", snapshot_ts = NA_real_))
    }

    # Bracket created after the pregame cutoff (or no tip-off time available):
    # first point with a genuine two-sided quote AND enough cumulative volume.
    cum_vol <- 0
    for (c in cands) {
      v <- suppressWarnings(as.numeric(c$volume_fp %||% 0))
      if (!is.na(v)) cum_vol <- cum_vol + v
      q <- quote_of(c)
      if (cum_vol >= MIN_CUM_VOLUME && two_sided(q)) {
        return(list(price = q$ask, mid = (q$bid + q$ask) / 2, method = "first_stable_quote",
                    snapshot_ts = c$end_period_ts))
      }
    }
    list(price = NA_real_, mid = NA_real_, method = "never_stabilized", snapshot_ts = NA_real_)
  }, error = function(e) list(price = NA_real_, mid = NA_real_, method = "error", snapshot_ts = NA_real_))
}

cat(sprintf("\n=== Fetching quote snapshots for %d markets ===\n", length(idx_keep)))
results <- vector("list", length(idx_keep))
for (i in seq_along(idx_keep)) {
  idx <- idx_keep[i]
  m <- all_markets[[idx]]
  tipoff_ts <- get_tipoff(game_date[idx], away_team[idx], home_team[idx])
  cutoff_ts <- if (!is.na(tipoff_ts)) tipoff_ts - PREGAME_OFFSET_SEC else NA_integer_

  snap <- fetch_quote_snapshot(m, cutoff_ts)
  Sys.sleep(0.12)  # light pacing: sustained zero-delay calls got ~50% rate-limited

  open_ts <- parse_kalshi_ts(m$open_time)
  close_ts <- parse_kalshi_ts(m$close_time)

  results[[i]] <- tibble(
    ticker = m$ticker,
    event_ticker = m$event_ticker %||% NA,
    game_date = as.character(as.Date(game_date[idx], origin = "1970-01-01")),
    away_team = away_team[idx], home_team = home_team[idx],
    threshold = threshold[idx],
    result = m$result %||% NA,
    volume_fp = as.numeric(m$volume_fp %||% 0),
    tipoff_available = !is.na(tipoff_ts),
    snapshot_price = snap$price,
    snapshot_mid = snap$mid,
    snapshot_method = snap$method,
    minutes_from_open = if (!is.na(snap$snapshot_ts) && !is.na(open_ts)) (snap$snapshot_ts - open_ts) / 60 else NA_real_,
    minutes_to_close = if (!is.na(snap$snapshot_ts) && !is.na(close_ts)) (close_ts - snap$snapshot_ts) / 60 else NA_real_
  )

  if (i %% 200 == 0) {
    cat(sprintf("  [%d/%d] checkpointing...\n", i, length(idx_keep)))
    saveRDS(bind_rows(results[1:i]), OUT_FILE)
  }
}

final_df <- bind_rows(results)
saveRDS(final_df, OUT_FILE)
cat(sprintf("\nDone. Saved %d rows to %s\n", nrow(final_df), OUT_FILE))
cat("snapshot_method distribution:\n")
print(table(final_df$snapshot_method))
cat(sprintf("Usable (non-NA price): %d\n", sum(!is.na(final_df$snapshot_price))))
