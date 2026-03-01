# Evaluate KalshiPredictor on January 2026 data
# Tests the updated logic using adjusted_wp for fade decisions

library(tidyverse)
library(hoopR)

# Source the predictor (which sources the models)
source("kalshi_predictor.R")

message("\n", strrep("=", 70))
message("  EVALUATING KALSHI PREDICTOR ON JANUARY 2026 DATA")
message(strrep("=", 70), "\n")

# Initialize predictor
message("Initializing KalshiPredictor...")
predictor <- KalshiPredictor$new()

# ------------------------------------------------------------------------------
# Load Data
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

kalshi_data <- readRDS("kalshi_nba_full_data.rds")

# Filter to home team markets only
kalshi_data <- kalshi_data %>%
  filter(market_team == home_team)

message(sprintf("Loaded %d Kalshi observations", nrow(kalshi_data)))

# Load schedule and PBP
schedule <- load_nba_schedule(seasons = 2026) %>%
  filter(status_type_completed == TRUE) %>%
  select(game_id, game_date, home_name = home_display_name, away_name = away_display_name,
         home_abbrev = home_abbreviation, away_abbrev = away_abbreviation,
         home_score, away_score, home_winner) %>%
  mutate(game_date = as.Date(game_date))

pbp <- load_nba_pbp(seasons = 2026)

# Load team ratings
message("Loading team ratings...")
team_box <- load_nba_team_box(seasons = 2026)

team_ratings <- team_box %>%
  filter(season_type == 2) %>%
  arrange(team_abbreviation, game_date) %>%
  mutate(
    poss = field_goals_attempted + 0.44 * free_throws_attempted - offensive_rebounds + turnovers
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

# ------------------------------------------------------------------------------
# Process Games
# ------------------------------------------------------------------------------

BET_SIZE <- 0.02  # 2% of bankroll per bet

all_fade_positions <- list()
all_wl_positions <- list()
fade_count_total <- 0
wl_count_total <- 0
games_processed <- 0

unique_games <- kalshi_data %>%
  distinct(ticker, game_date, home_team, away_team, result)

message(sprintf("\nProcessing %d games...\n", nrow(unique_games)))

for (g in 1:nrow(unique_games)) {
  game_info <- unique_games[g, ]

  home_abbrev_hoopr <- KALSHI_TO_HOOPR[game_info$home_team]
  away_abbrev_hoopr <- KALSHI_TO_HOOPR[game_info$away_team]

  if (is.na(home_abbrev_hoopr) || is.na(away_abbrev_hoopr)) next

  sched_game <- schedule %>%
    filter(game_date == game_info$game_date,
           home_abbrev == home_abbrev_hoopr,
           away_abbrev == away_abbrev_hoopr)

  if (nrow(sched_game) != 1) next

  game_id <- as.character(sched_game$game_id)
  home_won <- sched_game$home_winner

  # Get net ratings
  home_rating <- team_ratings %>%
    filter(game_id == sched_game$game_id, team_abbreviation == home_abbrev_hoopr) %>%
    pull(net_rating)
  away_rating <- team_ratings %>%
    filter(game_id == sched_game$game_id, team_abbreviation == away_abbrev_hoopr) %>%
    pull(net_rating)

  home_rating <- if (length(home_rating) == 0 || is.na(home_rating)) 0 else home_rating
  away_rating <- if (length(away_rating) == 0 || is.na(away_rating)) 0 else away_rating

  # Get PBP
  game_pbp <- pbp %>%
    filter(game_id == sched_game$game_id) %>%
    filter(!is.na(home_score), !is.na(away_score), !is.na(wallclock)) %>%
    mutate(
      pbp_timestamp = as.POSIXct(wallclock, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      home_margin = home_score - away_score,
      period = period_number,
      seconds_elapsed = case_when(
        period_number <= 4 ~ (period_number - 1) * 720 + (720 - (clock_minutes * 60 + clock_seconds)),
        TRUE ~ 2880 + (period_number - 5) * 300 + (300 - (clock_minutes * 60 + clock_seconds))
      ),
      seconds_remaining = 2880 - seconds_elapsed
    ) %>%
    arrange(pbp_timestamp)

  if (nrow(game_pbp) == 0) next

  game_kalshi <- kalshi_data %>%
    filter(ticker == game_info$ticker) %>%
    arrange(timestamp)

  if (nrow(game_kalshi) == 0) next

  games_processed <- games_processed + 1

  # Reset predictor for this game
  predictor$game_positions[[game_id]] <- NULL
  predictor$open_fades[[game_id]] <- NULL

  # Track positions for this game
  game_fade_positions <- list()
  game_wl_positions <- list()

  # Process each Kalshi observation
  for (i in 1:nrow(game_kalshi)) {
    kalshi_ts <- game_kalshi$timestamp[i]
    kalshi_price <- game_kalshi$price[i]

    if (is.na(kalshi_price)) next

    # Match to PBP
    diffs <- abs(difftime(game_pbp$pbp_timestamp, kalshi_ts, units = "secs"))
    idx <- which.min(diffs)

    if (as.numeric(diffs[idx]) > 600) next

    pbp_row <- game_pbp[idx, ]

    # Create game state
    game_state <- create_game_state(
      home_score = pbp_row$home_score,
      away_score = pbp_row$away_score,
      seconds_remaining = max(0, pbp_row$seconds_remaining),
      period = pbp_row$period,
      home_net_rating = home_rating,
      away_net_rating = away_rating
    )

    # Get signal from predictor
    signal <- tryCatch({
      predictor$get_signal(game_id, game_state, kalshi_price, kalshi_ts)
    }, error = function(e) {
      NULL
    })

    if (is.null(signal)) next

    # Record fade position if signaled
    if (signal$fade$signal != "HOLD") {
      fade_count_total <- fade_count_total + 1

      home_leading <- game_state$home_margin > 0
      leader_at_entry_won <- (home_leading & home_won) | (!home_leading & !home_won)
      fade_won <- !leader_at_entry_won

      game_fade_positions[[length(game_fade_positions) + 1]] <- tibble(
        position_id = fade_count_total,
        game_date = game_info$game_date,
        matchup = sprintf("%s @ %s", sched_game$away_name, sched_game$home_name),
        period = game_state$period,
        lead = abs(game_state$home_margin),
        home_leading = home_leading,
        entry_price = signal$fade$entry_price,
        trailer_win_prob = signal$fade$blown_rate,
        breakeven = signal$fade$breakeven,
        edge = signal$fade$edge,
        fade_won = fade_won,
        pnl = ifelse(fade_won,
                     (1 - signal$fade$entry_price) * 0.93 * BET_SIZE * 1000,
                     -signal$fade$entry_price * BET_SIZE * 1000)
      )

      # Record in predictor to track position limits
      predictor$record_position(game_id, "fade", signal$fade, kalshi_ts)
    }

    # Record with_leader position if signaled
    if (signal$with_leader$signal != "HOLD") {
      wl_count_total <- wl_count_total + 1

      home_leading <- game_state$home_margin > 0
      wl_won <- (home_leading & home_won) | (!home_leading & !home_won)

      game_wl_positions[[length(game_wl_positions) + 1]] <- tibble(
        position_id = wl_count_total,
        game_date = game_info$game_date,
        matchup = sprintf("%s @ %s", sched_game$away_name, sched_game$home_name),
        period = game_state$period,
        lead = abs(game_state$home_margin),
        home_leading = home_leading,
        entry_price = signal$with_leader$entry_price,
        hold_rate = signal$with_leader$hold_rate,
        wl_won = wl_won,
        pnl = ifelse(wl_won,
                     (1 - signal$with_leader$entry_price) * 0.93 * signal$with_leader$bet_size * 1000,
                     -signal$with_leader$entry_price * signal$with_leader$bet_size * 1000)
      )

      predictor$record_position(game_id, "with_leader", signal$with_leader, kalshi_ts)
    }
  }

  if (length(game_fade_positions) > 0) {
    all_fade_positions <- c(all_fade_positions, game_fade_positions)
  }
  if (length(game_wl_positions) > 0) {
    all_wl_positions <- c(all_wl_positions, game_wl_positions)
  }

  if (games_processed %% 50 == 0) {
    message(sprintf("  Processed %d games, %d fade, %d with_leader positions...",
                    games_processed, fade_count_total, wl_count_total))
  }
}

# ------------------------------------------------------------------------------
# Results
# ------------------------------------------------------------------------------

fade_df <- bind_rows(all_fade_positions)
wl_df <- bind_rows(all_wl_positions)

message("\n", strrep("=", 70))
message("  RESULTS: KALSHI PREDICTOR (adjusted_wp)")
message(strrep("=", 70))

message(sprintf("\nGames processed: %d", games_processed))

# FADE Results
message("\n--- FADE STRATEGY ---")
if (nrow(fade_df) > 0) {
  message(sprintf("Total positions: %d", nrow(fade_df)))
  message(sprintf("Win rate: %.1f%% (%d/%d)",
                  mean(fade_df$fade_won) * 100,
                  sum(fade_df$fade_won),
                  nrow(fade_df)))
  message(sprintf("Total P&L: $%.2f", sum(fade_df$pnl)))

  message("\nBy Period:")
  fade_df %>%
    group_by(period) %>%
    summarise(
      n = n(),
      wins = sum(fade_won),
      win_rate = mean(fade_won) * 100,
      pnl = sum(pnl),
      avg_model_prob = mean(trailer_win_prob) * 100,
      actual_win_rate = mean(fade_won) * 100,
      .groups = "drop"
    ) %>%
    print()

  message("\nExpected vs Actual:")
  fade_df %>%
    group_by(period) %>%
    summarise(
      n = n(),
      model_predicted = mean(trailer_win_prob) * 100,
      actual = mean(fade_won) * 100,
      diff = actual - model_predicted,
      .groups = "drop"
    ) %>%
    print()

  message("\nBy Lead Size:")
  fade_df %>%
    mutate(lead_bucket = case_when(
      lead >= 20 ~ "20+",
      lead >= 15 ~ "15-19",
      lead >= 10 ~ "10-14",
      TRUE ~ "7-9"
    )) %>%
    group_by(lead_bucket) %>%
    summarise(
      n = n(),
      win_rate = mean(fade_won) * 100,
      pnl = sum(pnl),
      .groups = "drop"
    ) %>%
    arrange(desc(lead_bucket)) %>%
    print()
} else {
  message("No fade positions taken")
}

# WITH LEADER Results
message("\n--- WITH LEADER STRATEGY ---")
if (nrow(wl_df) > 0) {
  message(sprintf("Total positions: %d", nrow(wl_df)))
  message(sprintf("Win rate: %.1f%% (%d/%d)",
                  mean(wl_df$wl_won) * 100,
                  sum(wl_df$wl_won),
                  nrow(wl_df)))
  message(sprintf("Total P&L: $%.2f", sum(wl_df$pnl)))

  message("\nBy Period:")
  wl_df %>%
    group_by(period) %>%
    summarise(
      n = n(),
      wins = sum(wl_won),
      win_rate = mean(wl_won) * 100,
      pnl = sum(pnl),
      .groups = "drop"
    ) %>%
    print()
} else {
  message("No with_leader positions taken")
}

# Combined
message("\n--- COMBINED ---")
total_pnl <- sum(fade_df$pnl, na.rm = TRUE) + sum(wl_df$pnl, na.rm = TRUE)
total_positions <- nrow(fade_df) + nrow(wl_df)
message(sprintf("Total positions: %d", total_positions))
message(sprintf("Total P&L: $%.2f", total_pnl))

message("\n", strrep("=", 70))
message("  EVALUATION COMPLETE")
message(strrep("=", 70))
