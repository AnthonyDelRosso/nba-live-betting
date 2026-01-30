# Kalshi Backtest with Score-Based Matching
# Matches Kalshi prices to PBP by finding closest wall-clock timestamp
# This gives more accurate game state for each market price observation

source("kalshi_predictor.R")
library(hoopR)

# ------------------------------------------------------------------------------
# Team mapping
# ------------------------------------------------------------------------------

KALSHI_TO_HOOPR <- c(
  "MIN" = "MIN", "DAL" = "DAL", "GSW" = "GS", "UTA" = "UTAH",
  "SAS" = "SA", "HOU" = "HOU", "CHA" = "CHA", "MEM" = "MEM",
  "NYK" = "NY", "TOR" = "TOR", "ATL" = "ATL", "BOS" = "BOS",
  "LAL" = "LAL", "CLE" = "CLE", "ORL" = "ORL", "MIA" = "MIA",
  "CHI" = "CHI", "IND" = "IND", "LAC" = "LAC", "DEN" = "DEN",
  "PHX" = "PHX", "POR" = "POR", "SAC" = "SAC", "OKC" = "OKC",
  "NOP" = "NO", "MIL" = "MIL", "DET" = "DET", "WAS" = "WSH",
  "PHI" = "PHI", "BKN" = "BKN"
)

# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------

message("\n=== SCORE-BASED MATCHING BACKTEST ===\n")

# Load Kalshi data
kalshi_data <- readRDS("kalshi_nba_backtest_data.rds")
message(sprintf("Loaded %d Kalshi price observations", nrow(kalshi_data)))

# Load schedule
message("Loading schedule...")
schedule <- load_nba_schedule(seasons = 2026) %>%
  filter(status_type_completed == TRUE) %>%
  select(game_id, game_date,
         home_abbrev = home_abbreviation,
         away_abbrev = away_abbreviation,
         home_winner) %>%
  mutate(game_date = as.Date(game_date))

# Load PBP with timestamps
message("Loading play-by-play...")
pbp <- load_nba_pbp(seasons = 2026)

# Load team ratings
message("Loading team ratings...")
team_box <- load_nba_team_box(seasons = 2026) %>%
  filter(season_type == 2)

team_ratings <- team_box %>%
  arrange(team_abbreviation, game_date) %>%
  mutate(
    poss = field_goals_attempted + 0.44 * free_throws_attempted -
      offensive_rebounds + turnovers
  ) %>%
  group_by(team_abbreviation) %>%
  mutate(
    cum_pts = lag(cumsum(team_score), default = NA_real_),
    cum_pts_allowed = lag(cumsum(opponent_team_score), default = NA_real_),
    cum_poss = lag(cumsum(poss), default = NA_real_),
    net_rating = ((cum_pts / cum_poss) - (cum_pts_allowed / cum_poss)) * 100
  ) %>%
  ungroup() %>%
  select(game_id, team_abbreviation, net_rating)

home_ratings <- team_box %>%
  filter(team_home_away == "home") %>%
  select(game_id, team_abbreviation) %>%
  left_join(team_ratings, by = c("game_id", "team_abbreviation")) %>%
  select(game_id, home_net_rating = net_rating)

away_ratings <- team_box %>%
  filter(team_home_away == "away") %>%
  select(game_id, team_abbreviation) %>%
  left_join(team_ratings, by = c("game_id", "team_abbreviation")) %>%
  select(game_id, away_net_rating = net_rating)

game_ratings <- home_ratings %>%
  left_join(away_ratings, by = "game_id") %>%
  distinct()

# ------------------------------------------------------------------------------
# Match games
# ------------------------------------------------------------------------------

message("\nMatching games...")

kalshi_games <- kalshi_data %>%
  mutate(
    home_abbrev_hoopr = KALSHI_TO_HOOPR[home_kalshi],
    away_abbrev_hoopr = KALSHI_TO_HOOPR[away_kalshi],
    ticker_date_str = str_extract(market_ticker, "\\d{2}[A-Z]{3}\\d{2}"),
    game_date_from_ticker = as.Date(ticker_date_str, format = "%y%b%d")
  ) %>%
  distinct(market_ticker, game_date_from_ticker, home_kalshi, away_kalshi,
           home_abbrev_hoopr, away_abbrev_hoopr, home_won) %>%
  rename(game_date = game_date_from_ticker)

matched_games <- kalshi_games %>%
  inner_join(
    schedule %>% select(game_id, game_date, home_abbrev, away_abbrev, home_winner),
    by = c("game_date", "home_abbrev_hoopr" = "home_abbrev", "away_abbrev_hoopr" = "away_abbrev")
  )

message(sprintf("Matched games: %d", nrow(matched_games)))

# ------------------------------------------------------------------------------
# Build PBP timeline with wall-clock timestamps
# ------------------------------------------------------------------------------

message("\nBuilding PBP timeline with wall-clock timestamps...")

# Get PBP states with actual wall-clock timestamps
pbp_timeline <- pbp %>%
  filter(game_id %in% matched_games$game_id) %>%
  filter(!is.na(home_score), !is.na(away_score)) %>%
  filter(!is.na(wallclock)) %>%
  mutate(
    # Parse the wallclock timestamp (actual time the play occurred)
    pbp_timestamp = as.POSIXct(wallclock, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    home_margin = home_score - away_score,
    # Calculate game clock
    seconds_remaining = case_when(
      period_number <= 4 ~ (4 - period_number) * 720 + clock_minutes * 60 + clock_seconds,
      period_number > 4 ~ clock_minutes * 60 + clock_seconds
    ),
    period = period_number
  ) %>%
  filter(!is.na(pbp_timestamp)) %>%
  select(game_id, pbp_timestamp, home_score, away_score, home_margin,
         seconds_remaining, period) %>%
  # Keep one state per timestamp (reduce size)
  distinct(game_id, pbp_timestamp, .keep_all = TRUE) %>%
  arrange(game_id, pbp_timestamp)

message(sprintf("PBP timeline events: %d", nrow(pbp_timeline)))

# ------------------------------------------------------------------------------
# Match Kalshi prices to PBP by wall-clock timestamp
# ------------------------------------------------------------------------------

message("\nMatching Kalshi prices to PBP states by timestamp...")

# Add game_id to Kalshi data
kalshi_with_games <- kalshi_data %>%
  inner_join(
    matched_games %>% select(market_ticker, game_id, home_won),
    by = "market_ticker"
  )

# For each game, match Kalshi timestamps to closest PBP timestamp
match_to_pbp <- function(kalshi_df, pbp_df) {
  # Get unique games
  games <- unique(kalshi_df$game_id)

  results <- list()

  for (gid in games) {
    kalshi_game <- kalshi_df %>% filter(game_id == gid)
    pbp_game <- pbp_df %>% filter(game_id == gid) %>% arrange(pbp_timestamp)

    if (nrow(pbp_game) == 0) next

    # For each Kalshi timestamp, find closest PBP state
    for (i in 1:nrow(kalshi_game)) {
      kalshi_ts <- kalshi_game$minute_bucket[i]

      # Find closest PBP timestamp
      time_diffs <- abs(difftime(pbp_game$pbp_timestamp, kalshi_ts, units = "secs"))
      closest_idx <- which.min(time_diffs)
      min_diff <- as.numeric(time_diffs[closest_idx])

      # Only match if within 10 minutes (600 seconds)
      if (min_diff <= 600) {
        pbp_state <- pbp_game[closest_idx, ]

        results[[length(results) + 1]] <- tibble(
          game_id = gid,
          kalshi_timestamp = kalshi_ts,
          kalshi_price = kalshi_game$price_mean[i],
          kalshi_volume = kalshi_game$volume[i],
          home_won = kalshi_game$home_won[i],
          home_score = pbp_state$home_score,
          away_score = pbp_state$away_score,
          home_margin = pbp_state$home_margin,
          seconds_remaining = pbp_state$seconds_remaining,
          period = pbp_state$period,
          time_diff_secs = min_diff
        )
      }
    }
  }

  bind_rows(results)
}

# Run matching (this may take a moment)
matched_data <- match_to_pbp(kalshi_with_games, pbp_timeline)

message(sprintf("Score-matched observations: %d", nrow(matched_data)))

# Add ratings
matched_data <- matched_data %>%
  left_join(game_ratings, by = "game_id")

# ------------------------------------------------------------------------------
# Filter and run predictor
# ------------------------------------------------------------------------------

message("\nFiltering to Q3+ with 15+ point leads (either team)...")

# Apply filters (Q3+, 15+ lead for EITHER team)
# Include both BUY_YES (home leading) and BUY_NO (away leading) signals
filtered_data <- matched_data %>%
  filter(period >= 3) %>%
  filter(abs(home_margin) >= 15)  # Either team leading by 15+

message(sprintf("After filters: %d observations", nrow(filtered_data)))

message("\nInitializing predictor...")
predictor <- KalshiPredictor$new()

message("Running predictions...")

# Get home_won from matched_games for each game_id
game_outcomes <- matched_games %>%
  select(game_id, home_won) %>%
  distinct()

# Store key columns before prediction
key_cols <- filtered_data %>%
  select(game_id, kalshi_timestamp, kalshi_price, kalshi_volume,
         home_score, away_score, home_margin, seconds_remaining, period,
         time_diff_secs, home_net_rating, away_net_rating) %>%
  left_join(game_outcomes, by = "game_id")

predictions <- filtered_data %>%
  rowwise() %>%
  mutate(
    game_state = list(create_game_state(
      home_score = home_score,
      away_score = away_score,
      seconds_remaining = seconds_remaining,
      period = period,
      home_net_rating = coalesce(home_net_rating, 0),
      away_net_rating = coalesce(away_net_rating, 0)
    )),
    signal_obj = list(predictor$get_signal(game_state, kalshi_price))
  ) %>%
  ungroup() %>%
  mutate(
    row_id = row_number(),
    model_prob = map_dbl(signal_obj, ~ .x$model_prob),
    edge = map_dbl(signal_obj, ~ .x$edge),
    signal = map_chr(signal_obj, ~ .x$signal),
    confidence = map_chr(signal_obj, ~ .x$confidence),
    bet_tier = map_chr(signal_obj, ~ .x$bet_tier),
    bet_size = map_dbl(signal_obj, ~ .x$bet_size)
  ) %>%
  select(row_id, model_prob, edge, signal, confidence, bet_tier, bet_size)

# Combine key columns with predictions
results <- key_cols %>%
  mutate(row_id = row_number()) %>%
  left_join(predictions, by = "row_id") %>%
  select(-row_id)

# ------------------------------------------------------------------------------
# Calculate P&L
# ------------------------------------------------------------------------------

message("\nCalculating trade outcomes...")

trades <- results %>%
  filter(signal %in% c("BUY_YES", "BUY_NO"), !is.na(edge)) %>%
  mutate(
    # BUY_YES wins when home_won=TRUE, BUY_NO wins when home_won=FALSE
    bet_won = case_when(
      signal == "BUY_YES" ~ home_won == TRUE,
      signal == "BUY_NO" ~ home_won == FALSE
    ),
    # Unit P&L with 7% fee
    # BUY_YES: win=(1-price)*0.93, lose=-price
    # BUY_NO: win=price*0.93, lose=-(1-price)
    unit_pnl = case_when(
      signal == "BUY_YES" & bet_won ~ (1 - kalshi_price) * 0.93,
      signal == "BUY_YES" & !bet_won ~ -kalshi_price,
      signal == "BUY_NO" & bet_won ~ kalshi_price * 0.93,
      signal == "BUY_NO" & !bet_won ~ -(1 - kalshi_price)
    ),
    # Tiered P&L (per $1000 bankroll)
    tiered_pnl = unit_pnl * bet_size * 1000,
    # Flat 2% for comparison
    flat_pnl = unit_pnl * 0.02 * 1000
  )

# ------------------------------------------------------------------------------
# Results
# ------------------------------------------------------------------------------

message("\n", strrep("=", 60))
message("  SCORE-BASED MATCHING BACKTEST RESULTS")
message(strrep("=", 60), "\n")

if (nrow(trades) > 0) {

  message(sprintf("Total Trades: %d across %d games",
                  nrow(trades), n_distinct(trades$game_id)))
  message(sprintf("Win Rate: %.1f%% (%d/%d)\n",
                  mean(trades$bet_won) * 100, sum(trades$bet_won), nrow(trades)))

  message("=== MATCHING QUALITY ===\n")
  message(sprintf("Avg time diff (Kalshi→PBP): %.1f seconds",
                  mean(trades$time_diff_secs)))
  message(sprintf("Max time diff: %.1f seconds\n",
                  max(trades$time_diff_secs)))

  message("=== COMPARISON: FLAT vs TIERED (per $1,000 bankroll) ===\n")
  message(sprintf("Flat Betting (2%% per trade):"))
  message(sprintf("  Total P&L: $%.2f", sum(trades$flat_pnl)))
  message(sprintf("  Avg P&L/Trade: $%.2f\n", mean(trades$flat_pnl)))

  message(sprintf("Tiered Betting (1-5%% based on tier):"))
  message(sprintf("  Total P&L: $%.2f", sum(trades$tiered_pnl)))
  message(sprintf("  Avg P&L/Trade: $%.2f", mean(trades$tiered_pnl)))
  improvement <- (sum(trades$tiered_pnl) / sum(trades$flat_pnl) - 1) * 100
  message(sprintf("  Improvement: +%.1f%%\n", improvement))

  message("=== BY BET TIER ===\n")
  by_tier <- trades %>%
    group_by(bet_tier) %>%
    summarise(
      n = n(),
      bet_size = sprintf("%.0f%%", first(bet_size) * 100),
      win_rate = sprintf("%.1f%%", mean(bet_won) * 100),
      tiered_pnl = sprintf("$%.2f", sum(tiered_pnl)),
      .groups = "drop"
    ) %>%
    arrange(bet_tier)
  print(by_tier)

  message("\n=== BY QUARTER ===\n")
  by_quarter <- trades %>%
    mutate(quarter = paste0("Q", pmin(period, 5))) %>%
    group_by(quarter) %>%
    summarise(
      n = n(),
      win_rate = sprintf("%.1f%%", mean(bet_won) * 100),
      flat_pnl = sprintf("$%.2f", sum(flat_pnl)),
      tiered_pnl = sprintf("$%.2f", sum(tiered_pnl)),
      .groups = "drop"
    )
  print(by_quarter)

  message("\n=== BY LEAD SIZE ===\n")
  by_lead <- trades %>%
    mutate(lead_bucket = cut(abs(home_margin),
                              breaks = c(14, 17, 20, 25, Inf),
                              labels = c("15-17", "18-20", "21-25", "25+"))) %>%
    group_by(lead_bucket) %>%
    summarise(
      n = n(),
      win_rate = sprintf("%.1f%%", mean(bet_won) * 100),
      tiered_pnl = sprintf("$%.2f", sum(tiered_pnl)),
      .groups = "drop"
    )
  print(by_lead)

  message("\n=== CUMULATIVE P&L ===\n")
  trades_ordered <- trades %>%
    arrange(game_id, kalshi_timestamp) %>%
    mutate(
      cum_tiered = cumsum(tiered_pnl),
      trade_num = row_number()
    )

  milestones <- c(100, 250, 500, 750, 1000, nrow(trades_ordered))
  milestones <- milestones[milestones <= nrow(trades_ordered)]

  for (m in milestones) {
    message(sprintf("After %d trades: $%.2f", m, trades_ordered$cum_tiered[m]))
  }

  message("\n=== RISK METRICS ===\n")
  trades_ordered <- trades_ordered %>%
    mutate(
      peak = cummax(cum_tiered),
      drawdown = cum_tiered - peak
    )

  message(sprintf("Max Drawdown: $%.2f", min(trades_ordered$drawdown)))
  sharpe <- mean(trades$tiered_pnl) / sd(trades$tiered_pnl) * sqrt(252)
  message(sprintf("Annualized Sharpe: %.2f", sharpe))

  message("\n=== FINAL SUMMARY ===\n")
  message(sprintf("Starting Bankroll: $1,000"))
  message(sprintf("Final Bankroll (Tiered): $%.2f (+%.1f%%)",
                  1000 + sum(trades$tiered_pnl), sum(trades$tiered_pnl) / 10))

  # Save results
  saveRDS(trades, "kalshi_score_matched_results.rds")
  message("\nSaved to kalshi_score_matched_results.rds")

} else {
  message("No trades generated!")
}

message("\n=== Done ===")
