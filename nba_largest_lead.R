library(tidyverse)
library(hoopR)
library(DBI)
library(RSQLite)

# Set higher connection buffer for data loading
Sys.setenv("VROOM_CONNECTION_SIZE" = 262144)

# Constants
REGULATION_SECONDS <- 2880
OVERTIME_SECONDS <- 300
QUARTER_SECONDS <- 720
OT_PERIOD_SECONDS <- 300

# Database path
DB_PATH <- "lead_database.sqlite"

# ------------------------------------------------------------------------------
# Database functions
# ------------------------------------------------------------------------------

#' Initialize SQLite database connection
#' Creates database file and table if they don't exist
init_database <- function(db_path = DB_PATH) {
  # Connect to database (creates file if doesn't exist)
  conn <- dbConnect(RSQLite::SQLite(), db_path)

  # Create table if it doesn't exist
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS largest_leads (
      game_id INTEGER,
      largestLead INTEGER,
      secondsElapsedAtLead REAL,
      secondsRemainingAtLead REAL,
      minutesElapsedAtLead REAL,
      periodAtLead INTEGER,
      scoreHomeAtLead INTEGER,
      scoreAwayAtLead INTEGER,
      teamWithLead TEXT,
      teamWinner TEXT,
      teamLoser TEXT,
      team_home TEXT,
      team_away TEXT,
      teamWithLeadAbbrev TEXT,
      teamWithLeadWon INTEGER,
      blewLead INTEGER,
      home_rest_days INTEGER,
      away_rest_days INTEGER,
      leadTeamRestDays INTEGER,
      restAdvantage INTEGER,
      home_net_rating REAL,
      away_net_rating REAL,
      leadTeamNetRating REAL,
      netRatingAdvantage REAL,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (game_id, teamWithLead)
    )
  ")

  message(sprintf("Connected to database: %s", db_path))
  conn
}

#' Write lead data to database
#' Inserts new records, skips duplicates based on game_id + teamWithLead
write_to_database <- function(lead_data, conn) {
  # Get existing game/team combinations
  existing <- dbGetQuery(conn, "SELECT game_id, teamWithLead FROM largest_leads")

  # Filter to new records only
  if (nrow(existing) > 0) {
    new_data <- lead_data %>%
      anti_join(existing, by = c("game_id", "teamWithLead"))
  } else {
    new_data <- lead_data
  }

  if (nrow(new_data) == 0) {
    message("No new records to insert.")
    return(invisible(0))
  }

  # Prepare data for insertion (convert logicals to integers for SQLite)
  insert_data <- new_data %>%
    mutate(
      teamWithLeadWon = as.integer(teamWithLeadWon),
      blewLead = as.integer(blewLead)
    ) %>%
    select(
      game_id, largestLead, secondsElapsedAtLead, secondsRemainingAtLead,
      minutesElapsedAtLead, periodAtLead, scoreHomeAtLead, scoreAwayAtLead,
      teamWithLead, teamWinner, teamLoser, team_home, team_away,
      teamWithLeadAbbrev, teamWithLeadWon, blewLead,
      home_rest_days, away_rest_days, leadTeamRestDays, restAdvantage,
      home_net_rating, away_net_rating, leadTeamNetRating, netRatingAdvantage
    )

  # Write to database
  dbWriteTable(conn, "largest_leads", insert_data, append = TRUE, row.names = FALSE)

  message(sprintf("Inserted %d new records into database.", nrow(insert_data)))
  invisible(nrow(insert_data))
}

#' Read all data from database
read_from_database <- function(db_path = DB_PATH) {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(conn))

  data <- dbGetQuery(conn, "SELECT * FROM largest_leads") %>%
    as_tibble() %>%
    mutate(
      teamWithLeadWon = as.logical(teamWithLeadWon),
      blewLead = as.logical(blewLead)
    )

  message(sprintf("Read %d records from database.", nrow(data)))
  data
}

#' Close database connection
close_database <- function(conn) {
  dbDisconnect(conn)
  message("Database connection closed.")
}

# ------------------------------------------------------------------------------
# 1. Load schedule data for specified season(s)
# ------------------------------------------------------------------------------

get_schedule <- function(seasons = 2024) {
  message("Loading schedule data from hoopR...")
  schedule <- load_nba_schedule(seasons = seasons)

  # Filter to completed games only and extract relevant columns
  schedule %>%
    filter(status_type_completed == TRUE) %>%
    select(
      game_id,
      game_date,
      season,
      season_type,
      home_team_abbrev = home_abbreviation,
      away_team_abbrev = away_abbreviation,
      home_score,
      away_score,
      home_winner
    ) %>%
    mutate(
      away_winner = !home_winner,
      teamWinner = if_else(home_winner, home_team_abbrev, away_team_abbrev),
      teamLoser = if_else(home_winner, away_team_abbrev, home_team_abbrev)
    ) %>%
    distinct()
}

# ------------------------------------------------------------------------------
# 2. Calculate rest days for each team
# ------------------------------------------------------------------------------

#' Calculate rest days for each team before each game
#' Rest days = days since previous game - 1 (so 0 = back-to-back)
calculate_rest_days <- function(schedule) {
  # Reshape to get one row per team per game
  home_games <- schedule %>%
    select(game_id, game_date, team = home_team_abbrev) %>%
    mutate(location = "home")

  away_games <- schedule %>%
    select(game_id, game_date, team = away_team_abbrev) %>%
    mutate(location = "away")

  # Combine and calculate rest days per team
  all_games <- bind_rows(home_games, away_games) %>%
    mutate(game_date = as.Date(game_date)) %>%
    arrange(team, game_date) %>%
    group_by(team) %>%
    mutate(
      prev_game_date = lag(game_date),
      rest_days = as.integer(game_date - prev_game_date) - 1L
    ) %>%
    ungroup() %>%
    select(game_id, team, rest_days)

  # Pivot to get home and away rest days per game
  home_rest <- schedule %>%
    select(game_id, team = home_team_abbrev) %>%
    left_join(all_games, by = c("game_id", "team")) %>%
    select(game_id, home_rest_days = rest_days)

  away_rest <- schedule %>%
    select(game_id, team = away_team_abbrev) %>%
    left_join(all_games, by = c("game_id", "team")) %>%
    select(game_id, away_rest_days = rest_days)

  # Combine into single dataframe
  home_rest %>%
    left_join(away_rest, by = "game_id") %>%
    distinct()
}

# ------------------------------------------------------------------------------
# 3. Calculate rolling net ratings for each team
# ------------------------------------------------------------------------------

#' Calculate rolling net rating going into each game
#' Uses Dean Oliver's formula to estimate possessions
#' Returns OFF_RATING, DEF_RATING, NET_RATING per 100 possessions
calculate_net_ratings <- function(seasons, season_type = 2) {
  message("Loading team box scores for net rating calculation...")
  team_box <- load_nba_team_box(seasons = seasons)

  # Filter to specified season type
  if (!is.null(season_type)) {
    team_box <- team_box %>% filter(season_type == !!season_type)
  }

  # Calculate possessions using Dean Oliver formula approximation
  # POSS ≈ FGA + 0.44*FTA - OREB + TOV
  team_ratings <- team_box %>%
    arrange(team_abbreviation, game_date) %>%
    mutate(
      poss = field_goals_attempted + 0.44 * free_throws_attempted -
        offensive_rebounds + turnovers
    ) %>%
    group_by(team_abbreviation) %>%
    mutate(
      # Cumulative stats BEFORE current game (use lag)
      cum_pts = lag(cumsum(team_score), default = NA_real_),
      cum_pts_allowed = lag(cumsum(opponent_team_score), default = NA_real_),
      cum_poss = lag(cumsum(poss), default = NA_real_),
      # Calculate ratings per 100 possessions
      off_rating = (cum_pts / cum_poss) * 100,
      def_rating = (cum_pts_allowed / cum_poss) * 100,
      net_rating = off_rating - def_rating
    ) %>%
    ungroup() %>%
    select(game_id, team_abbreviation, net_rating)

  # Pivot to get home and away ratings per game
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

  # Combine
  home_ratings %>%
    left_join(away_ratings, by = "game_id") %>%
    distinct()
}

# ------------------------------------------------------------------------------
# 4. Load play-by-play data for games
# ------------------------------------------------------------------------------

get_pbp_data <- function(seasons = 2024) {
  message("Loading play-by-play data from hoopR...")
  pbp <- load_nba_pbp(seasons = seasons)
  message(sprintf("Loaded %d play-by-play records", nrow(pbp)))
  pbp
}

# ------------------------------------------------------------------------------
# 5. Parse scores and calculate margins
# ------------------------------------------------------------------------------

parse_scores <- function(pbp_data) {
  pbp_data %>%
    # Filter to plays with valid scores
    filter(!is.na(home_score), !is.na(away_score)) %>%
    mutate(
      # Calculate margin from each team's perspective
      marginHome = home_score - away_score,
      marginAway = away_score - home_score,
      # Convert period and time to total seconds elapsed
      # hoopR uses period_number for quarter, clock_minutes and clock_seconds for time remaining
      secondsElapsed = case_when(
        period_number <= 4 ~ (period_number - 1) * QUARTER_SECONDS +
          (QUARTER_SECONDS - (clock_minutes * 60 + clock_seconds)),
        period_number > 4 ~ REGULATION_SECONDS + (period_number - 5) * OT_PERIOD_SECONDS +
          (OT_PERIOD_SECONDS - (clock_minutes * 60 + clock_seconds))
      ),
      # Total game seconds (regulation = 2880, each OT = 300)
      totalGameSeconds = if_else(
        period_number <= 4,
        REGULATION_SECONDS,
        REGULATION_SECONDS + (period_number - 4) * OVERTIME_SECONDS
      ),
      # Time remaining
      secondsRemaining = totalGameSeconds - secondsElapsed,
      # Minutes elapsed for interpretability
      minutesElapsed = secondsElapsed / 60
    )
}

# ------------------------------------------------------------------------------
# 6. Calculate largest lead for each team in each game
# ------------------------------------------------------------------------------

calculate_largest_leads <- function(pbp_parsed) {
  # Get largest lead for home team
  home_leads <- pbp_parsed %>%
    filter(marginHome > 0) %>%
    group_by(game_id) %>%
    slice_max(marginHome, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(
      game_id,
      largestLead = marginHome,
      secondsElapsedAtLead = secondsElapsed,
      secondsRemainingAtLead = secondsRemaining,
      minutesElapsedAtLead = minutesElapsed,
      periodAtLead = period_number,
      scoreHomeAtLead = home_score,
      scoreAwayAtLead = away_score,
      team_home = home_team_abbrev,
      team_away = away_team_abbrev
    ) %>%
    mutate(teamWithLead = "home")

  # Get largest lead for away team
  away_leads <- pbp_parsed %>%
    filter(marginAway > 0) %>%
    group_by(game_id) %>%
    slice_max(marginAway, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(
      game_id,
      largestLead = marginAway,
      secondsElapsedAtLead = secondsElapsed,
      secondsRemainingAtLead = secondsRemaining,
      minutesElapsedAtLead = minutesElapsed,
      periodAtLead = period_number,
      scoreHomeAtLead = home_score,
      scoreAwayAtLead = away_score,
      team_home = home_team_abbrev,
      team_away = away_team_abbrev
    ) %>%
    mutate(teamWithLead = "away")

  bind_rows(home_leads, away_leads)
}

# ------------------------------------------------------------------------------
# 7. Join with game outcomes, rest data, and net ratings
# ------------------------------------------------------------------------------

add_game_outcomes <- function(largest_leads, schedule, rest_data, net_ratings) {
  # Get unique game outcomes
  game_outcomes <- schedule %>%
    select(game_id, teamWinner, teamLoser) %>%
    distinct()

  largest_leads %>%
    left_join(game_outcomes, by = "game_id") %>%
    left_join(rest_data, by = "game_id") %>%
    left_join(net_ratings, by = "game_id") %>%
    mutate(
      # Identify which team had the lead
      teamWithLeadAbbrev = if_else(teamWithLead == "home", team_home, team_away),
      # Did the team with the lead win?
      teamWithLeadWon = teamWithLeadAbbrev == teamWinner,
      # Binary outcome: did they blow the lead (lose)?
      blewLead = !teamWithLeadWon,
      # Rest days for the team with the lead
      leadTeamRestDays = if_else(teamWithLead == "home", home_rest_days, away_rest_days),
      # Rest advantage (positive = lead team more rested)
      restAdvantage = if_else(
        teamWithLead == "home",
        home_rest_days - away_rest_days,
        away_rest_days - home_rest_days
      ),
      # Net rating for the team with the lead
      leadTeamNetRating = if_else(teamWithLead == "home", home_net_rating, away_net_rating),
      # Net rating advantage (positive = lead team is better)
      netRatingAdvantage = if_else(
        teamWithLead == "home",
        home_net_rating - away_net_rating,
        away_net_rating - home_net_rating
      )
    )
}

# ------------------------------------------------------------------------------
# 8. Main pipeline function
# ------------------------------------------------------------------------------

build_lead_dataset <- function(seasons = 2024,
                                season_type = 2,
                                write_to_db = TRUE,
                                db_path = DB_PATH) {
  # season_type: 2 = Regular Season, 3 = Playoffs

  message("Step 1: Loading schedule...")
  schedule <- get_schedule(seasons)

  # Filter by season type if specified
  if (!is.null(season_type)) {
    schedule <- schedule %>% filter(season_type == !!season_type)
  }

  game_ids <- unique(schedule$game_id)
  message(sprintf("Found %d completed games", length(game_ids)))

  message("\nStep 2: Calculating rest days...")
  rest_data <- calculate_rest_days(schedule)
  message(sprintf("Calculated rest days for %d games", nrow(rest_data)))

  message("\nStep 3: Calculating net ratings...")
  net_ratings <- calculate_net_ratings(seasons, season_type)
  message(sprintf("Calculated net ratings for %d games", nrow(net_ratings)))

  message("\nStep 4: Loading play-by-play data...")
  pbp_raw <- get_pbp_data(seasons)

  # Filter to games in our schedule
  pbp_raw <- pbp_raw %>%
    filter(game_id %in% game_ids)
  message(sprintf("Filtered to %d play-by-play records for selected games", nrow(pbp_raw)))

  message("\nStep 5: Parsing scores and calculating margins...")
  pbp_parsed <- parse_scores(pbp_raw)

  message("\nStep 6: Calculating largest leads...")
  largest_leads <- calculate_largest_leads(pbp_parsed)

  message("\nStep 7: Adding game outcomes, rest data, and net ratings...")
  final_data <- add_game_outcomes(largest_leads, schedule, rest_data, net_ratings)

  # Write to database if enabled
  if (write_to_db) {
    message("\nStep 8: Writing to database...")
    conn <- init_database(db_path)
    write_to_database(final_data, conn)
    close_database(conn)
  }

  message("\nDone! Dataset ready for modeling.")

  final_data
}

# ------------------------------------------------------------------------------
# 9. Run the pipeline (adjust seasons as needed)
# ------------------------------------------------------------------------------

# Example: Pull data for 2023-24 season (Regular Season)
# lead_data <- build_lead_dataset(seasons = 2024, season_type = 2)

# Preview the structure
# glimpse(lead_data)

# Quick summary
# lead_data %>%
#   group_by(teamWithLead) %>%
#   summarise(
#     n_games = n(),
#     n_blown = sum(blewLead),
#     pct_blown = mean(blewLead) * 100,
#     avg_lead = mean(largestLead),
#     avg_minutes_elapsed = mean(minutesElapsedAtLead)
#   )

# ------------------------------------------------------------------------------
# 10. Prepare data for modeling
# ------------------------------------------------------------------------------

prepare_model_data <- function(lead_data) {
  lead_data %>%
    select(
      game_id,
      largestLead,
      secondsElapsedAtLead,
      secondsRemainingAtLead,
      minutesElapsedAtLead,
      periodAtLead,
      teamWithLead,
      blewLead,
      leadTeamRestDays,
      restAdvantage,
      leadTeamNetRating,
      netRatingAdvantage
    ) %>%
    mutate(
      blewLead = factor(blewLead, levels = c(FALSE, TRUE),
                        labels = c("held", "blown")),
      periodAtLead = factor(periodAtLead)
    ) %>%
    drop_na()
}

# ------------------------------------------------------------------------------
# 11. Train blown lead prediction model
# ------------------------------------------------------------------------------

MODEL_PATH <- "blown_lead_model.rds"

#' Train logistic regression model to predict blown leads
#' @param db_path Path to SQLite database
#' @param save_model Whether to save the trained model to disk
#' @return Trained workflow object
train_blown_lead_model <- function(db_path = DB_PATH, save_model = TRUE) {
  library(tidymodels)

  # Load data from database
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  lead_data <- dbGetQuery(conn, "SELECT * FROM largest_leads") %>% as_tibble()
  dbDisconnect(conn)

  # Prepare modeling data
  model_data <- lead_data %>%
    filter(!is.na(netRatingAdvantage), !is.na(restAdvantage)) %>%
    mutate(
      blewLead = factor(blewLead, levels = c(0, 1), labels = c("held", "blown")),
      teamWithLead = factor(teamWithLead),
      periodAtLead = factor(periodAtLead)
    ) %>%
    select(
      blewLead,
      largestLead,
      minutesElapsedAtLead,
      periodAtLead,
      teamWithLead,
      restAdvantage,
      netRatingAdvantage
    )

  message(sprintf("Training on %d records...", nrow(model_data)))

  # Create recipe
  lead_recipe <- recipe(blewLead ~ ., data = model_data) %>%
    step_dummy(all_nominal_predictors())

  # Logistic regression specification
  log_spec <- logistic_reg() %>%
    set_engine("glm") %>%
    set_mode("classification")

  # Workflow
  lead_workflow <- workflow() %>%
    add_recipe(lead_recipe) %>%
    add_model(log_spec)

  # Fit model on all data
  lead_fit <- lead_workflow %>% fit(data = model_data)

  # Save model if requested
 if (save_model) {
    saveRDS(lead_fit, MODEL_PATH)
    message(sprintf("Model saved to: %s", MODEL_PATH))
  }

  message("Model training complete!")
  lead_fit
}

#' Load trained model from disk
#' @param model_path Path to saved model RDS file
#' @return Trained workflow object
load_model <- function(model_path = MODEL_PATH) {
  if (!file.exists(model_path)) {
    stop(sprintf("Model file not found: %s. Run train_blown_lead_model() first.", model_path))
  }
  readRDS(model_path)
}

# ------------------------------------------------------------------------------
# 12. Predict blown lead probability
# ------------------------------------------------------------------------------

#' Predict probability of blowing a lead
#' @param model Trained model (or NULL to load from disk)
#' @param largest_lead Size of the lead in points
#' @param minutes_elapsed Minutes into the game when lead was reached
#' @param period Quarter/period (1-4 for regulation, 5+ for OT)
#' @param is_home Whether the team with the lead is the home team
#' @param rest_advantage Days of rest advantage (positive = more rested)
#' @param net_rating_advantage Net rating advantage (positive = better team)
#' @return Probability of blowing the lead (0-1)
predict_blown_lead <- function(model = NULL,
                                largest_lead,
                                minutes_elapsed,
                                period = 1,
                                is_home = TRUE,
                                rest_advantage = 0,
                                net_rating_advantage = 0) {
  library(tidymodels)

  # Load model if not provided
  if (is.null(model)) {
    model <- load_model()
  }

  # Create prediction data frame
  new_data <- tibble(
    largestLead = largest_lead,
    minutesElapsedAtLead = minutes_elapsed,
    periodAtLead = factor(period, levels = c(1, 2, 3, 4, 5, 6, 7)),
    teamWithLead = factor(ifelse(is_home, "home", "away"), levels = c("away", "home")),
    restAdvantage = rest_advantage,
    netRatingAdvantage = net_rating_advantage
  )

  # Predict
  pred <- predict(model, new_data, type = "prob")

  # Return probability of blowing lead
  pred$.pred_blown
}

#' Predict for multiple scenarios at once
#' @param model Trained model (or NULL to load from disk)
#' @param scenarios Data frame with columns: largest_lead, minutes_elapsed,
#'        period, is_home, rest_advantage, net_rating_advantage
#' @return Data frame with predictions added
predict_scenarios <- function(model = NULL, scenarios) {
  library(tidymodels)

  if (is.null(model)) {
    model <- load_model()
  }

  # Prepare data
  pred_data <- scenarios %>%
    mutate(
      largestLead = largest_lead,
      minutesElapsedAtLead = minutes_elapsed,
      periodAtLead = factor(period, levels = c(1, 2, 3, 4, 5, 6, 7)),
      teamWithLead = factor(ifelse(is_home, "home", "away"), levels = c("away", "home")),
      restAdvantage = rest_advantage,
      netRatingAdvantage = net_rating_advantage
    )

  # Predict
  preds <- predict(model, pred_data, type = "prob")

  # Return with predictions
  scenarios %>%
    mutate(prob_blown = preds$.pred_blown)
}

# ------------------------------------------------------------------------------
# Example usage
# ------------------------------------------------------------------------------

# Train and save model:
# model <- train_blown_lead_model()

# Make a single prediction:
# predict_blown_lead(
#   largest_lead = 15,
#   minutes_elapsed = 24,  # halftime
#   period = 2,
#   is_home = TRUE,
#   rest_advantage = 1,
#   net_rating_advantage = 5
# )

# Predict multiple scenarios:
# scenarios <- tibble(
#   largest_lead = c(10, 15, 20),
#   minutes_elapsed = c(24, 24, 24),
#   period = c(2, 2, 2),
#   is_home = c(TRUE, TRUE, TRUE),
#   rest_advantage = c(0, 0, 0),
#   net_rating_advantage = c(0, 0, 0)
# )
# predict_scenarios(scenarios = scenarios)
