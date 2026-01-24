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
      teamWithLeadAbbrev, teamWithLeadWon, blewLead
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
# 2. Load play-by-play data for games
# ------------------------------------------------------------------------------

get_pbp_data <- function(seasons = 2024) {
  message("Loading play-by-play data from hoopR...")
  pbp <- load_nba_pbp(seasons = seasons)
  message(sprintf("Loaded %d play-by-play records", nrow(pbp)))
  pbp
}

# ------------------------------------------------------------------------------
# 3. Parse scores and calculate margins
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
# 4. Calculate largest lead for each team in each game
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
# 5. Join with game outcomes to determine if lead was blown
# ------------------------------------------------------------------------------

add_game_outcomes <- function(largest_leads, schedule) {
  # Get unique game outcomes

game_outcomes <- schedule %>%
    select(game_id, teamWinner, teamLoser) %>%
    distinct()

  largest_leads %>%
    left_join(game_outcomes, by = "game_id") %>%
    mutate(
      # Identify which team had the lead
      teamWithLeadAbbrev = if_else(teamWithLead == "home", team_home, team_away),
      # Did the team with the lead win?
      teamWithLeadWon = teamWithLeadAbbrev == teamWinner,
      # Binary outcome: did they blow the lead (lose)?
      blewLead = !teamWithLeadWon
    )
}

# ------------------------------------------------------------------------------
# 6. Main pipeline function
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

  message("\nStep 2: Loading play-by-play data...")
  pbp_raw <- get_pbp_data(seasons)

  # Filter to games in our schedule
  pbp_raw <- pbp_raw %>%
    filter(game_id %in% game_ids)
  message(sprintf("Filtered to %d play-by-play records for selected games", nrow(pbp_raw)))

  message("\nStep 3: Parsing scores and calculating margins...")
  pbp_parsed <- parse_scores(pbp_raw)

  message("\nStep 4: Calculating largest leads...")
  largest_leads <- calculate_largest_leads(pbp_parsed)

  message("\nStep 5: Adding game outcomes...")
  final_data <- add_game_outcomes(largest_leads, schedule)

  # Write to database if enabled
  if (write_to_db) {
    message("\nStep 6: Writing to database...")
    conn <- init_database(db_path)
    write_to_database(final_data, conn)
    close_database(conn)
  }

  message("\nDone! Dataset ready for modeling.")

  final_data
}

# ------------------------------------------------------------------------------
# 7. Run the pipeline (adjust seasons as needed)
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
# 8. Prepare data for modeling
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
      blewLead
    ) %>%
    mutate(
      blewLead = factor(blewLead, levels = c(FALSE, TRUE),
                        labels = c("held", "blown")),
      periodAtLead = factor(periodAtLead)
    ) %>%
    drop_na()
}

# Example modeling setup with tidymodels
# library(tidymodels)
# model_data <- prepare_model_data(lead_data)
#
# # Split data
# set.seed(123)
# split <- initial_split(model_data, prop = 0.8, strata = blewLead)
# train <- training(split)
# test <- testing(split)
#
# # Logistic regression recipe
# lead_recipe <- recipe(blewLead ~ largestLead + minutesElapsedAtLead + periodAtLead,
#                       data = train) %>%
#   step_dummy(all_nominal_predictors())
#
# # Model specification
# log_spec <- logistic_reg() %>%
#   set_engine("glm") %>%
#   set_mode("classification")
#
# # Workflow
# lead_workflow <- workflow() %>%
#   add_recipe(lead_recipe) %>%
#   add_model(log_spec)
#
# # Fit
# lead_fit <- lead_workflow %>% fit(data = train)
