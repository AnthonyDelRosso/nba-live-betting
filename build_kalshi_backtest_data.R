# Build Kalshi NBA Backtest Dataset
# Matches Kalshi trade prices to game states from play-by-play data

source("kalshi_data.R")
library(hoopR)

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

OUTPUT_FILE <- "kalshi_nba_backtest_data.rds"
MAX_GAMES <- 500  # Expanded for full backtest

# Team abbreviation mappings (Kalshi uses different abbreviations)
TEAM_MAP <- c(
  "MIN" = "MIN", "DAL" = "DAL", "GSW" = "GS", "UTA" = "UTAH",
  "SAS" = "SA", "HOU" = "HOU", "CHA" = "CHA", "MEM" = "MEM",
  "NYK" = "NY", "TOR" = "TOR", "ATL" = "ATL", "BOS" = "BOS",
  "LAL" = "LAL", "CLE" = "CLE", "ORL" = "ORL", "MIA" = "MIA",
  "CHI" = "CHI", "IND" = "IND", "LAC" = "LAC", "DEN" = "DEN",
  "PHX" = "PHX", "POR" = "POR", "SAC" = "SAC", "OKC" = "OKC",
  "NOP" = "NO", "MIL" = "MIL", "DET" = "DET", "WAS" = "WAS",
  "PHI" = "PHI", "BKN" = "BKN"
)

# ------------------------------------------------------------------------------
# Parse Kalshi market ticker
# ------------------------------------------------------------------------------

#' Parse game info from Kalshi ticker
#' @param ticker e.g., "KXNBAGAME-26JAN28MINDAL-MIN"
#' @return List with date, away_team, home_team, market_team
parse_kalshi_ticker <- function(ticker) {
  # Extract parts
  parts <- str_split(ticker, "-")[[1]]

  if (length(parts) < 3) return(NULL)

  date_str <- parts[2]  # "26JAN28"
  teams_str <- str_extract(date_str, "[A-Z]+$")  # "MINDAL"
  date_part <- str_remove(date_str, teams_str)   # "26JAN28"

  # Parse date (format: YYMMMDD)
  year <- paste0("20", substr(date_part, 1, 2))
  month <- substr(date_part, 3, 5)
  day <- substr(date_part, 6, 7)
  game_date <- as.Date(paste(year, month, day), format = "%Y%b%d")

  # Parse teams (first 3 chars = away, last 3 = home)
  away_kalshi <- substr(teams_str, 1, 3)
  home_kalshi <- substr(teams_str, 4, 6)

  # Market team (which team this YES contract is for)
  market_team <- parts[3]

  list(
    game_date = game_date,
    away_kalshi = away_kalshi,
    home_kalshi = home_kalshi,
    market_team = market_team,
    is_home_market = market_team == home_kalshi
  )
}

# ------------------------------------------------------------------------------
# Fetch Kalshi data
# ------------------------------------------------------------------------------

#' Get all settled NBA game markets
#' @param limit Max markets to fetch
fetch_nba_markets <- function(limit = 1000) {
  message("Fetching NBA game markets from Kalshi...")

  all_markets <- tibble()
  cursor <- NULL

  while (nrow(all_markets) < limit) {
    params <- list(
      series_ticker = "KXNBAGAME",
      status = "settled",
      limit = min(200, limit - nrow(all_markets))
    )
    if (!is.null(cursor)) params$cursor <- cursor

    result <- kalshi_get("/markets", params)

    if (length(result$markets) == 0) break

    batch <- map_dfr(result$markets, function(m) {
      tibble(
        ticker = m$ticker,
        event_ticker = m$event_ticker,
        title = m$title,
        result = m$result,
        volume = m$volume,
        open_time = m$open_time,
        close_time = m$close_time
      )
    })

    all_markets <- bind_rows(all_markets, batch)
    cursor <- result$cursor

    if (is.null(cursor)) break

    message(sprintf("  Fetched %d markets...", nrow(all_markets)))
    Sys.sleep(1.0)  # Slower rate to avoid 503s
  }

  message(sprintf("Total markets: %d", nrow(all_markets)))
  all_markets
}

#' Fetch trades for a market with pagination
#' @param market_ticker Market ticker
#' @param max_trades Max trades to fetch
fetch_all_trades <- function(market_ticker, max_trades = 5000) {
  all_trades <- tibble()
  cursor <- NULL

  while (nrow(all_trades) < max_trades) {
    params <- list(
      ticker = market_ticker,
      limit = min(1000, max_trades - nrow(all_trades))
    )
    if (!is.null(cursor)) params$cursor <- cursor

    result <- tryCatch({
      kalshi_get("/markets/trades", params)
    }, error = function(e) {
      return(list(trades = list()))
    })

    if (length(result$trades) == 0) break

    batch <- map_dfr(result$trades, function(t) {
      tibble(
        trade_id = t$trade_id,
        timestamp = t$created_time,
        price = t$yes_price,
        count = t$count
      )
    })

    all_trades <- bind_rows(all_trades, batch)
    cursor <- result$cursor

    if (is.null(cursor) || nrow(batch) < 1000) break

    Sys.sleep(0.5)  # Rate limiting
  }

  all_trades %>%
    mutate(market_ticker = market_ticker)
}

# ------------------------------------------------------------------------------
# Build combined dataset
# ------------------------------------------------------------------------------

#' Build backtest dataset combining Kalshi and PBP data
build_kalshi_backtest_dataset <- function(max_games = MAX_GAMES) {

  # 1. Fetch Kalshi markets
  markets <- fetch_nba_markets(limit = max_games * 2)

  # Only keep home team markets (avoid duplicates)
  markets <- markets %>%
    rowwise() %>%
    mutate(
      parsed = list(parse_kalshi_ticker(ticker))
    ) %>%
    ungroup() %>%
    filter(map_lgl(parsed, ~ !is.null(.x))) %>%
    mutate(
      game_date = map(parsed, ~ .x$game_date) %>% do.call(c, .),
      away_kalshi = map_chr(parsed, ~ .x$away_kalshi),
      home_kalshi = map_chr(parsed, ~ .x$home_kalshi),
      is_home_market = map_lgl(parsed, ~ .x$is_home_market)
    ) %>%
    filter(is_home_market) %>%  # Keep only home team markets
    select(-parsed) %>%
    distinct(event_ticker, .keep_all = TRUE)

  message(sprintf("Processing %d unique games", nrow(markets)))

  # 2. Fetch trades for each game
  message("\nFetching trade history...")
  all_trades <- list()

  for (i in 1:min(max_games, nrow(markets))) {
    ticker <- markets$ticker[i]
    message(sprintf("  [%d/%d] %s", i, min(max_games, nrow(markets)), ticker))

    trades <- fetch_all_trades(ticker, max_trades = 2000)

    if (nrow(trades) > 0) {
      trades$game_date <- markets$game_date[i]
      trades$away_kalshi <- markets$away_kalshi[i]
      trades$home_kalshi <- markets$home_kalshi[i]
      trades$result <- markets$result[i]  # "yes" if home won
      trades$home_won <- markets$result[i] == "yes"
      all_trades[[ticker]] <- trades
    }

    Sys.sleep(1.0)  # Rate limiting between games
  }

  trades_df <- bind_rows(all_trades)
  message(sprintf("\nTotal trades collected: %d", nrow(trades_df)))

  # 3. Aggregate trades into time buckets (1-minute intervals)
  message("\nAggregating into price timeline...")
  price_timeline <- trades_df %>%
    mutate(
      datetime = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      minute_bucket = floor_date(datetime, "1 minute")
    ) %>%
    group_by(market_ticker, game_date, away_kalshi, home_kalshi, home_won, minute_bucket) %>%
    summarise(
      price_mean = mean(price) / 100,  # Convert cents to probability
      price_last = last(price) / 100,
      price_high = max(price) / 100,
      price_low = min(price) / 100,
      volume = sum(count),
      n_trades = n(),
      .groups = "drop"
    ) %>%
    arrange(market_ticker, minute_bucket)

  message(sprintf("Price timeline rows: %d", nrow(price_timeline)))

  # Save
  saveRDS(price_timeline, OUTPUT_FILE)
  message(sprintf("\nSaved to %s", OUTPUT_FILE))

  price_timeline
}

# ------------------------------------------------------------------------------
# Run
# ------------------------------------------------------------------------------

message("\n=== Building Kalshi NBA Backtest Dataset ===\n")
kalshi_data <- build_kalshi_backtest_dataset(max_games = MAX_GAMES)

message("\n=== Summary ===")
message(sprintf("Games: %d", n_distinct(kalshi_data$market_ticker)))
message(sprintf("Date range: %s to %s",
                min(kalshi_data$game_date), max(kalshi_data$game_date)))
message(sprintf("Total price observations: %d", nrow(kalshi_data)))

message("\nSample data:")
print(kalshi_data %>% head(20))
