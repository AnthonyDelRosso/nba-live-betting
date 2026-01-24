library(tidyverse)
library(magrittr)
library(tidymodels)
library(nbastatR)
library(DBI)
library(RSQLite)

# Set higher connection buffer for API calls
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

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
      idGame INTEGER,
      largestLead INTEGER,
      secondsElapsedAtLead REAL,
      secondsRemainingAtLead REAL,
      minutesElapsedAtLead REAL,
      periodAtLead INTEGER,
      scoreHomeAtLead INTEGER,
      scoreAwayAtLead INTEGER,
      teamWithLead TEXT,
      slugTeamWinner TEXT,
      slugTeamLoser TEXT,
      slugTeam_home TEXT,
      slugTeam_away TEXT,
      slugTeamWithLead TEXT,
      teamWithLeadWon INTEGER,
      blewLead INTEGER,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (idGame, teamWithLead)
    )
  ")

  message(sprintf("Connected to database: %s", db_path))
  conn
}

#' Write lead data to database
#' Inserts new records, skips duplicates based on idGame + teamWithLead
write_to_database <- function(lead_data, conn) {
  # Get existing game/team combinations
  existing <- dbGetQuery(conn, "SELECT idGame, teamWithLead FROM largest_leads")

  # Filter to new records only
  if (nrow(existing) > 0) {
    new_data <- lead_data %>%
      anti_join(existing, by = c("idGame", "teamWithLead"))
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
      idGame, largestLead, secondsElapsedAtLead, secondsRemainingAtLead,
      minutesElapsedAtLead, periodAtLead, scoreHomeAtLead, scoreAwayAtLead,
      teamWithLead, slugTeamWinner, slugTeamLoser, slugTeam_home, slugTeam_away,
      slugTeamWithLead, teamWithLeadWon, blewLead
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
# 1. Get game IDs for specified season(s)
# ------------------------------------------------------------------------------

get_game_ids <- function(seasons = 2024, season_type = "Regular Season") {
  game_logs(
    seasons = seasons,
    season_types = season_type,
    result_types = "team"
  ) %>%
    distinct(idGame, dateGame, slugTeamWinner, slugTeamLoser,
             slugMatchup, outcomeGame, slugTeam, nameTeam)
}

# ------------------------------------------------------------------------------
# 2. Pull play-by-play data for games
# ------------------------------------------------------------------------------

get_pbp_data <- function(game_ids, chunk_size = 50) {
  # Process in chunks to avoid API rate limits
  n_games <- length(game_ids)
  n_chunks <- ceiling(n_games / chunk_size)

  pbp_list <- vector("list", n_chunks)

  for (i in seq_len(n_chunks)) {
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, n_games)
    chunk_ids <- game_ids[start_idx:end_idx]

    message(sprintf("Fetching chunk %d of %d (games %d-%d)",
                    i, n_chunks, start_idx, end_idx))

    pbp_list[[i]] <- play_by_play_v2(
      game_ids = chunk_ids,
      return_message = FALSE
    )

    # Pause between chunks to respect rate limits
    if (i < n_chunks) Sys.sleep(2)
  }

  bind_rows(pbp_list)
}

# ------------------------------------------------------------------------------
# 3. Parse scores and calculate margins
# ------------------------------------------------------------------------------

parse_scores <- function(pbp_data) {
  pbp_data %>%
    filter(!is.na(slugScore), slugScore != "") %>%
    # Parse slugScore format: "AWAY - HOME" (e.g., "105 - 98")
    separate(
      slugScore,
      into = c("scoreAway", "scoreHome"),
      sep = " - ",
      convert = TRUE,
      remove = FALSE
    ) %>%
    mutate(
      # Calculate margin from home team perspective
      marginHome = scoreHome - scoreAway,
      marginAway = scoreAway - scoreHome,
      # Convert period and time to total seconds elapsed
      secondsElapsed = case_when(
        numberPeriod <= 4 ~ (numberPeriod - 1) * 12 * 60 +
          (12 * 60 - (minuteRemainingQuarter * 60 + secondsRemainingQuarter)),
        numberPeriod > 4 ~ 48 * 60 + (numberPeriod - 5) * 5 * 60 +
          (5 * 60 - (minuteRemainingQuarter * 60 + secondsRemainingQuarter))
      ),
      # Total game seconds (regulation = 2880, each OT = 300)
      totalGameSeconds = if_else(
        numberPeriod <= 4,
        2880,
        2880 + (numberPeriod - 4) * 300
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
    group_by(idGame) %>%
    slice_max(marginHome, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(
      idGame,
      largestLead = marginHome,
      secondsElapsedAtLead = secondsElapsed,
      secondsRemainingAtLead = secondsRemaining,
      minutesElapsedAtLead = minutesElapsed,
      periodAtLead = numberPeriod,
      scoreHomeAtLead = scoreHome,
      scoreAwayAtLead = scoreAway
    ) %>%
    mutate(teamWithLead = "home")

  # Get largest lead for away team
  away_leads <- pbp_parsed %>%
    filter(marginAway > 0) %>%
    group_by(idGame) %>%
    slice_max(marginAway, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(
      idGame,
      largestLead = marginAway,
      secondsElapsedAtLead = secondsElapsed,
      secondsRemainingAtLead = secondsRemaining,
      minutesElapsedAtLead = minutesElapsed,
      periodAtLead = numberPeriod,
      scoreHomeAtLead = scoreHome,
      scoreAwayAtLead = scoreAway
    ) %>%
    mutate(teamWithLead = "away")

  bind_rows(home_leads, away_leads)
}

# ------------------------------------------------------------------------------
# 5. Join with game outcomes to determine if lead was blown
# ------------------------------------------------------------------------------

add_game_outcomes <- function(largest_leads, game_info) {
  # Get unique game info with winner
  game_outcomes <- game_info %>%
    select(idGame, slugTeamWinner, slugTeamLoser) %>%
    distinct()

  # Get home/away team mapping
  team_mapping <- game_info %>%
    mutate(
      isHome = str_detect(slugMatchup, "vs\\."),
      homeAway = if_else(isHome, "home", "away")
    ) %>%
    select(idGame, slugTeam, homeAway) %>%
    distinct() %>%
    pivot_wider(
      names_from = homeAway,
      values_from = slugTeam,
      names_prefix = "slugTeam_"
    )

  largest_leads %>%
    left_join(game_outcomes, by = "idGame") %>%
    left_join(team_mapping, by = "idGame") %>%
    mutate(
      # Identify which team had the lead
      slugTeamWithLead = if_else(teamWithLead == "home", slugTeam_home, slugTeam_away),
      # Did the team with the lead win?
      teamWithLeadWon = slugTeamWithLead == slugTeamWinner,
      # Binary outcome: did they blow the lead (lose)?
      blewLead = !teamWithLeadWon
    )
}

# ------------------------------------------------------------------------------
# 6. Main pipeline function
# ------------------------------------------------------------------------------

build_lead_dataset <- function(seasons = 2024,
                                season_type = "Regular Season",
                                chunk_size = 50,
                                write_to_db = TRUE,
                                db_path = DB_PATH) {

  message("Step 1: Fetching game logs...")
  game_info <- get_game_ids(seasons, season_type)
  game_ids <- unique(game_info$idGame)
  message(sprintf("Found %d unique games", length(game_ids)))

  message("\nStep 2: Fetching play-by-play data...")
  pbp_raw <- get_pbp_data(game_ids, chunk_size)
  message(sprintf("Retrieved %d play-by-play records", nrow(pbp_raw)))

  message("\nStep 3: Parsing scores and calculating margins...")
  pbp_parsed <- parse_scores(pbp_raw)

  message("\nStep 4: Calculating largest leads...")
  largest_leads <- calculate_largest_leads(pbp_parsed)

  message("\nStep 5: Adding game outcomes...")
  final_data <- add_game_outcomes(largest_leads, game_info)

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

# Example: Pull data for 2023-24 season
# lead_data <- build_lead_dataset(seasons = 2024)

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
      idGame,
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
