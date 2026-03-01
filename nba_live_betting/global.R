# Global configuration and package loading for NBA Live Betting Platform

# ------------------------------------------------------------------------------
# Package Loading
# ------------------------------------------------------------------------------

# Core packages
library(shiny)

# Try to load optional packages, use alternatives if not available
if (!requireNamespace("shinydashboard", quietly = TRUE)) {
  warning("shinydashboard not available - install with: install.packages('shinydashboard')")
} else {
  library(shinydashboard)
}

if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
  warning("shinyWidgets not available - install with: install.packages('shinyWidgets')")
} else {
  library(shinyWidgets)
}

# Data manipulation
library(tidyverse)
library(lubridate)

# HTTP/API
library(httr2)
library(jsonlite)

# OOP
library(R6)

# Display
library(DT)

if (!requireNamespace("plotly", quietly = TRUE)) {
  warning("plotly not available - charts will be limited. Install with: install.packages('plotly')")
  # Create stub function
  plotlyOutput <- function(...) div("plotly not installed")
  renderPlotly <- function(...) NULL
} else {
  library(plotly)
}

# Async scheduling
library(later)

# Configuration
library(yaml)

# NBA data (optional, for team ratings)
if (!requireNamespace("hoopR", quietly = TRUE)) {
  warning("hoopR not available - team ratings will not be loaded")
} else {
  library(hoopR)
}

# Set buffer size for large data
Sys.setenv("VROOM_CONNECTION_SIZE" = 262144)

# ------------------------------------------------------------------------------
# Determine Paths
# ------------------------------------------------------------------------------

# Find the app directory - works whether run from RStudio, command line, or Shiny Server
get_app_dir <- function() {
  # Try multiple methods to find the app directory

  # Method 1: Check if we're in the nba_live_betting directory
  if (file.exists("app.R") && file.exists("global.R")) {
    return(normalizePath(getwd()))
  }

 # Method 2: Check if nba_live_betting is a subdirectory
  if (dir.exists("nba_live_betting") && file.exists("nba_live_betting/app.R")) {
    return(normalizePath("nba_live_betting"))
  }

  # Method 3: Use the directory of this script if sourced
  if (exists("ofile")) {
    return(normalizePath(dirname(ofile)))
  }

  # Fallback: assume current directory
  normalizePath(getwd())
}

APP_DIR <- get_app_dir()

# Parent directory contains the model files
PARENT_DIR <- normalizePath(file.path(APP_DIR, ".."))

message(sprintf("App directory: %s", APP_DIR))
message(sprintf("Parent directory: %s", PARENT_DIR))

# ------------------------------------------------------------------------------
# Source Core Files
# ------------------------------------------------------------------------------

# Source from parent directory (existing models)
# Use chdir=TRUE so any relative paths inside these files resolve correctly
wp_model_path <- file.path(PARENT_DIR, "win_probability_model.R")
if (file.exists(wp_model_path)) {
  source(wp_model_path, chdir = TRUE)
  message("Loaded: win_probability_model.R")
} else {
  stop(sprintf("REQUIRED FILE NOT FOUND: %s\nMake sure win_probability_model.R exists in: %s",
               wp_model_path, PARENT_DIR))
}

# Source blown lead model
blown_lead_path <- file.path(PARENT_DIR, "nba_largest_lead.R")
if (file.exists(blown_lead_path)) {
  source(blown_lead_path, chdir = TRUE)
  message("Loaded: nba_largest_lead.R")
} else {
  stop(sprintf("REQUIRED FILE NOT FOUND: %s\nMake sure nba_largest_lead.R exists in: %s",
               blown_lead_path, PARENT_DIR))
}

# Source kalshi predictor (use chdir=TRUE so its relative source() calls work)
predictor_path <- file.path(PARENT_DIR, "kalshi_predictor.R")
if (file.exists(predictor_path)) {
  source(predictor_path, chdir = TRUE)
  message("Loaded: kalshi_predictor.R")
} else {
  stop(sprintf("REQUIRED FILE NOT FOUND: %s\nMake sure kalshi_predictor.R exists in: %s",
               predictor_path, PARENT_DIR))
}

# Source platform R files (use absolute paths)
source(file.path(APP_DIR, "R/logging.R"))
source(file.path(APP_DIR, "R/trading_state.R"))
source(file.path(APP_DIR, "R/espn_api.R"))
source(file.path(APP_DIR, "R/paper_trading.R"))
source(file.path(APP_DIR, "R/kalshi_api.R"))
source(file.path(APP_DIR, "R/automated_trader.R"))

# Source Shiny modules
source(file.path(APP_DIR, "modules/mod_live_games.R"))
source(file.path(APP_DIR, "modules/mod_positions.R"))
source(file.path(APP_DIR, "modules/mod_trade_log.R"))
source(file.path(APP_DIR, "modules/mod_controls.R"))
source(file.path(APP_DIR, "modules/mod_performance.R"))

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

# Load config if exists
CONFIG <- list(
  paper_balance = 4500,        # Updated: $4,500 bankroll for realistic P&L
  poll_interval = 30,
  kalshi_fee = 0.07,
  log_level = "INFO",
  # In-game adjustment settings
  enable_ingame_adjustments = TRUE,
  price_history_minutes = 5    # Minutes of price history to track for velocity
)

config_path <- file.path(APP_DIR, "config/config.yml")
if (file.exists(config_path)) {
  user_config <- yaml::read_yaml(config_path)
  CONFIG <- modifyList(CONFIG, user_config)
}

# Model paths (in parent directory)
MODEL_PATHS <- list(
  wp_model = file.path(PARENT_DIR, "win_probability_model.rds"),
  blown_lead_model = file.path(PARENT_DIR, "blown_lead_model_v2.rds")
)

# Data and log directories
DATA_DIR <- file.path(APP_DIR, "data")
LOG_DIR <- file.path(APP_DIR, "logs")

# Create directories if they don't exist
if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)
if (!dir.exists(LOG_DIR)) dir.create(LOG_DIR, recursive = TRUE)

# ------------------------------------------------------------------------------
# Team Data
# ------------------------------------------------------------------------------

# Team colors for display
TEAM_COLORS <- c(
  "ATL" = "#E03A3E", "BOS" = "#007A33", "BKN" = "#000000", "CHA" = "#1D1160",

"CHI" = "#CE1141", "CLE" = "#860038", "DAL" = "#00538C", "DEN" = "#0E2240",
  "DET" = "#C8102E", "GSW" = "#1D428A", "HOU" = "#CE1141", "IND" = "#002D62",
  "LAC" = "#C8102E", "LAL" = "#552583", "MEM" = "#5D76A9", "MIA" = "#98002E",
  "MIL" = "#00471B", "MIN" = "#0C2340", "NOP" = "#0C2340", "NYK" = "#006BB6",
  "OKC" = "#007AC1", "ORL" = "#0077C0", "PHI" = "#006BB6", "PHX" = "#1D1160",
  "POR" = "#E03A3E", "SAC" = "#5A2D81", "SAS" = "#C4CED4", "TOR" = "#CE1141",
  "UTA" = "#002B5C", "WAS" = "#002B5C"
)

# ESPN team IDs for logos
TEAM_ESPN_IDS <- c(
  "ATL" = "1", "BOS" = "2", "BKN" = "17", "CHA" = "30", "CHI" = "4",
  "CLE" = "5", "DAL" = "6", "DEN" = "7", "DET" = "8", "GSW" = "9",
  "HOU" = "10", "IND" = "11", "LAC" = "12", "LAL" = "13", "MEM" = "29",
  "MIA" = "14", "MIL" = "15", "MIN" = "16", "NOP" = "3", "NYK" = "18",
  "OKC" = "25", "ORL" = "19", "PHI" = "20", "PHX" = "21", "POR" = "22",
  "SAC" = "23", "SAS" = "24", "TOR" = "28", "UTA" = "26", "WAS" = "27"
)

#' Get team logo URL
#' @param team_abbrev Team abbreviation
#' @return ESPN logo URL
get_team_logo <- function(team_abbrev) {
  team_id <- TEAM_ESPN_IDS[team_abbrev]
  if (is.na(team_id)) return(NA_character_)
  sprintf("https://a.espncdn.com/i/teamlogos/nba/500/%s.png", team_id)
}

# ------------------------------------------------------------------------------
# Initialize Global Objects
# ------------------------------------------------------------------------------

# These will be created in the server function but declared here for reference
# logger <- create_logger(log_dir = "logs", level = CONFIG$log_level)
# state <- TradingState$new(mode = "paper", paper_balance = CONFIG$paper_balance, logger = logger)
# predictor <- KalshiPredictor$new(
#   wp_model_path = MODEL_PATHS$wp_model,
#   blown_lead_model_path = MODEL_PATHS$blown_lead_model
# )
# trader <- create_automated_trader(state, predictor, logger, mode = "paper")

# ------------------------------------------------------------------------------
# Helper Functions
# ------------------------------------------------------------------------------

#' Format currency
#' @param x Numeric value
#' @return Formatted currency string
format_currency <- function(x) {
  sprintf("$%.2f", x)
}

#' Format percentage
#' @param x Numeric value (0-1)
#' @return Formatted percentage string
format_pct <- function(x) {
  sprintf("%.1f%%", x * 100)
}

#' Format game clock
#' @param period Period number
#' @param clock_minutes Minutes on clock
#' @param clock_seconds Seconds on clock
#' @return Formatted clock string (e.g., "Q3 5:32")
format_game_clock <- function(period, clock_minutes, clock_seconds) {
  period <- period %||% 1
  clock_minutes <- clock_minutes %||% 0
  clock_seconds <- clock_seconds %||% 0
  period_str <- if (period <= 4) paste0("Q", period) else paste0("OT", period - 4)
  sprintf("%s %d:%02d", period_str, clock_minutes, clock_seconds)
}

#' Validate and sanitize game state for predictor
#' Ensures all required fields exist and are of correct type
#' @param game_state Raw game state (possibly from ESPN)
#' @return Validated game state list or NULL if invalid
validate_game_state <- function(game_state) {
  # Must be a list
 if (!is.list(game_state)) {
    return(NULL)
  }

  # Extract values with defaults
  home_score <- as.numeric(game_state$home_score %||% 0)
  away_score <- as.numeric(game_state$away_score %||% 0)
  home_margin <- as.numeric(game_state$home_margin %||% (home_score - away_score))
  period <- as.integer(game_state$period %||% 1)
  seconds_remaining <- as.numeric(game_state$seconds_remaining %||% 2880)
  minutes_elapsed <- as.numeric(game_state$minutes_elapsed %||% ((2880 - seconds_remaining) / 60))

  # Validate numeric values
  if (is.na(home_margin) || is.na(period) || is.na(seconds_remaining)) {
    return(NULL)
  }

  # Ensure period is reasonable
  period <- max(1, min(10, period))

  # Build validated game state
  list(
    home_score = home_score,
    away_score = away_score,
    home_margin = home_margin,
    seconds_remaining = seconds_remaining,
    seconds_elapsed = game_state$seconds_elapsed %||% (2880 - seconds_remaining),
    minutes_elapsed = minutes_elapsed,
    period = period,
    clock_minutes = as.numeric(game_state$clock_minutes %||% 0),
    clock_seconds = as.numeric(game_state$clock_seconds %||% 0),
    home_net_rating = as.numeric(game_state$home_net_rating %||% 0),
    away_net_rating = as.numeric(game_state$away_net_rating %||% 0),
    home_rating_advantage = as.numeric(game_state$home_rating_advantage %||% 0),
    home_rest_days = as.numeric(game_state$home_rest_days %||% 1),
    away_rest_days = as.numeric(game_state$away_rest_days %||% 1),
    rest_advantage = as.numeric(game_state$rest_advantage %||% 0),
    net_rating_advantage = as.numeric(game_state$net_rating_advantage %||% 0),
    # Preserve team identifiers for Kalshi market ticker lookup
    home_team_abbrev = as.character(game_state$home_team_abbrev %||% ""),
    away_team_abbrev = as.character(game_state$away_team_abbrev %||% "")
  )
}

#' Safely get signal from predictor
#' Validates game state and catches errors
#' @param predictor KalshiPredictor object
#' @param game_id Game ID
#' @param game_state Game state (will be validated)
#' @param market_price Market price (0-1)
#' @param live_data Optional in-game adjustment data (price velocity, run differential)
#' @return Signal list or NULL on error
safe_get_signal <- function(predictor, game_id, game_state, market_price, live_data = NULL) {
  if (is.null(predictor)) return(NULL)

  # Validate game state
  validated_state <- validate_game_state(game_state)
  if (is.null(validated_state)) return(NULL)

  # Validate market price
  if (!is.numeric(market_price) || is.na(market_price)) return(NULL)
  market_price <- max(0.01, min(0.99, market_price))

  # Get signal with error handling
  tryCatch({
    predictor$get_signal(
      game_id = as.character(game_id),
      game_state = validated_state,
      market_price = market_price,
      current_time = Sys.time(),
      live_data = live_data  # Pass in-game adjustment data
    )
  }, error = function(e) {
    warning(sprintf("get_signal error for game %s: %s", game_id, e$message))
    NULL
  })
}

#' Safely get adjusted win probability from predictor
#' @param predictor KalshiPredictor object
#' @param game_state Game state (will be validated)
#' @return WP result list or NULL on error
safe_get_wp <- function(predictor, game_state) {
  if (is.null(predictor)) return(NULL)

  validated_state <- validate_game_state(game_state)
  if (is.null(validated_state)) return(NULL)

  tryCatch({
    predictor$get_adjusted_wp(validated_state)
  }, error = function(e) {
    NULL
  })
}

#' Get team net ratings for current season
#' @param season Season year
#' @return Tibble with team ratings
get_team_ratings <- function(season = 2026) {
  message(sprintf("[RATINGS] Loading team ratings for season %d...", season))

  tryCatch({
    message("[RATINGS] Fetching team box scores from hoopR...")
    team_box <- load_nba_team_box(seasons = season)

    message(sprintf("[RATINGS] Loaded %d team box score records", nrow(team_box)))

    team_box <- team_box %>%
      filter(season_type == 2)  # Regular season

    message(sprintf("[RATINGS] Filtered to %d regular season records", nrow(team_box)))

    if (nrow(team_box) == 0) {
      message("[RATINGS] WARNING: No regular season data found!")
      return(tibble(
        team_abbreviation = character(),
        net_rating = numeric(),
        games = integer()
      ))
    }

    ratings <- team_box %>%
      arrange(team_abbreviation, game_date) %>%
      mutate(
        poss = field_goals_attempted + 0.44 * free_throws_attempted -
          offensive_rebounds + turnovers
      ) %>%
      group_by(team_abbreviation) %>%
      summarise(
        games = n(),
        total_pts = sum(team_score),
        total_pts_allowed = sum(opponent_team_score),
        total_poss = sum(poss),
        off_rating = (total_pts / total_poss) * 100,
        def_rating = (total_pts_allowed / total_poss) * 100,
        net_rating = off_rating - def_rating,
        .groups = "drop"
      ) %>%
      select(team_abbreviation, net_rating, games)

    message(sprintf("[RATINGS] Calculated ratings for %d teams", nrow(ratings)))

    # Show top 5 and bottom 5 teams
    if (nrow(ratings) > 0) {
      top5 <- ratings %>% arrange(desc(net_rating)) %>% head(5)
      bot5 <- ratings %>% arrange(net_rating) %>% head(5)
      message("[RATINGS] Top 5 teams:")
      for (i in 1:nrow(top5)) {
        message(sprintf("  %s: %+.1f (%d games)", top5$team_abbreviation[i], top5$net_rating[i], top5$games[i]))
      }
      message("[RATINGS] Bottom 5 teams:")
      for (i in 1:nrow(bot5)) {
        message(sprintf("  %s: %+.1f (%d games)", bot5$team_abbreviation[i], bot5$net_rating[i], bot5$games[i]))
      }
    }

    ratings
  }, error = function(e) {
    message(sprintf("[RATINGS] ERROR: Failed to load team ratings: %s", e$message))
    warning(sprintf("Failed to load team ratings: %s", e$message))
    tibble(
      team_abbreviation = character(),
      net_rating = numeric(),
      games = integer()
    )
  })
}
