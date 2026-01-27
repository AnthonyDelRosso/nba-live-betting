library(tidyverse)
library(hoopR)
library(DBI)
library(RSQLite)
library(tidymodels)

# Set higher connection buffer for data loading
Sys.setenv("VROOM_CONNECTION_SIZE" = 262144)

# ------------------------------------------------------------------------------
# Constants
# ------------------------------------------------------------------------------

REGULATION_SECONDS <- 2880
OVERTIME_SECONDS <- 300
QUARTER_SECONDS <- 720

WP_MODEL_PATH <- "win_probability_model.rds"
WP_DB_PATH <- "win_probability_database.sqlite"

# ------------------------------------------------------------------------------
# 1. Build training data from play-by-play
# ------------------------------------------------------------------------------

#' Extract game states from play-by-play data for win probability modeling
#' Each row represents a moment in a game with the score, time, and eventual winner
build_wp_training_data <- function(seasons = 2015:2024) {
  message("Loading play-by-play data...")
  pbp <- load_nba_pbp(seasons = seasons)

  message("Loading schedule for game outcomes...")
  schedule <- load_nba_schedule(seasons = seasons) %>%
    filter(status_type_completed == TRUE) %>%
    select(game_id, home_winner, season) %>%
    distinct()

  message("Loading team box scores for net ratings...")
  team_box <- load_nba_team_box(seasons = seasons) %>%
    filter(season_type == 2)  # Regular season only for ratings

  # Calculate cumulative net ratings going into each game
  team_ratings <- team_box %>%
    arrange(team_abbreviation, game_date) %>%
    mutate(
      poss = field_goals_attempted + 0.44 * free_throws_attempted -
        offensive_rebounds + turnovers
    ) %>%
    group_by(team_abbreviation, season) %>%
    mutate(
      cum_pts = lag(cumsum(team_score), default = NA_real_),
      cum_pts_allowed = lag(cumsum(opponent_team_score), default = NA_real_),
      cum_poss = lag(cumsum(poss), default = NA_real_),
      net_rating = ((cum_pts / cum_poss) - (cum_pts_allowed / cum_poss)) * 100
    ) %>%
    ungroup() %>%
    select(game_id, team_abbreviation, net_rating)

  # Get home/away net ratings per game
  home_ratings <- team_box %>%
    filter(team_home_away == "home") %>%
    select(game_id, home_team = team_abbreviation) %>%
    left_join(team_ratings, by = c("game_id", "home_team" = "team_abbreviation")) %>%
    select(game_id, home_net_rating = net_rating)

  away_ratings <- team_box %>%
    filter(team_home_away == "away") %>%
    select(game_id, away_team = team_abbreviation) %>%
    left_join(team_ratings, by = c("game_id", "away_team" = "team_abbreviation")) %>%
    select(game_id, away_net_rating = net_rating)

  game_ratings <- home_ratings %>%
    left_join(away_ratings, by = "game_id") %>%
    distinct()

  message("Processing play-by-play data...")

  # Process PBP to get game states
  # Sample at key moments: every score change, end of quarters, specific time intervals
  game_states <- pbp %>%
    filter(!is.na(home_score), !is.na(away_score)) %>%
    mutate(
      # Score margin from home team's perspective
      home_margin = home_score - away_score,

      # Calculate seconds elapsed
      seconds_elapsed = case_when(
        period_number <= 4 ~ (period_number - 1) * QUARTER_SECONDS +
          (QUARTER_SECONDS - (clock_minutes * 60 + clock_seconds)),
        period_number > 4 ~ REGULATION_SECONDS + (period_number - 5) * OVERTIME_SECONDS +
          (OVERTIME_SECONDS - (clock_minutes * 60 + clock_seconds))
      ),

      # Total game length (for calculating time remaining)
      total_seconds = if_else(
        period_number <= 4,
        REGULATION_SECONDS,
        REGULATION_SECONDS + (period_number - 4) * OVERTIME_SECONDS
      ),

      # Seconds remaining in regulation (can be negative in OT)
      seconds_remaining = REGULATION_SECONDS - seconds_elapsed,

      # Minutes elapsed (for interpretability)
      minutes_elapsed = seconds_elapsed / 60,

      # Period
      period = period_number
    ) %>%
    # Join with game outcomes
    left_join(schedule, by = "game_id") %>%
    # Join with team ratings
    left_join(game_ratings, by = "game_id") %>%
    # Calculate net rating advantage for home team
    mutate(
      home_rating_advantage = home_net_rating - away_net_rating
    ) %>%
    # Select relevant columns
    select(
      game_id,
      seconds_elapsed,
      seconds_remaining,
      minutes_elapsed,
      period,
      home_score,
      away_score,
      home_margin,
      home_net_rating,
      away_net_rating,
      home_rating_advantage,
      home_winner
    ) %>%
    filter(!is.na(home_winner), !is.na(seconds_elapsed))

  # Sample to reduce data size while keeping important moments
  # Keep: score changes, quarter ends, and sample every ~30 seconds
  message("Sampling game states...")

  sampled_states <- game_states %>%
    group_by(game_id) %>%
    mutate(
      score_changed = home_margin != lag(home_margin, default = 0),
      quarter_end = period != lag(period, default = 1),
      time_bucket = floor(seconds_elapsed / 30)  # 30-second buckets
    ) %>%
    # Keep score changes, quarter ends, and one observation per time bucket
    filter(score_changed | quarter_end | !duplicated(time_bucket)) %>%
    ungroup() %>%
    select(-score_changed, -quarter_end, -time_bucket)

  message(sprintf("Created %d game state observations from %d games",
                  nrow(sampled_states), n_distinct(sampled_states$game_id)))

  sampled_states
}

# ------------------------------------------------------------------------------
# 2. Train win probability model
# ------------------------------------------------------------------------------

#' Train logistic regression model for win probability
#' Predicts P(home team wins) given current game state
train_wp_model <- function(training_data = NULL, seasons = 2015:2024, save_model = TRUE) {

  if (is.null(training_data)) {
    training_data <- build_wp_training_data(seasons)
  }

  # Prepare modeling data
  model_data <- training_data %>%
    filter(!is.na(home_rating_advantage)) %>%
    mutate(
      home_win = factor(home_winner, levels = c(FALSE, TRUE), labels = c("away", "home")),
      # Bin period for modeling (combine OT periods)
      period_factor = factor(pmin(period, 5), levels = 1:5,
                             labels = c("Q1", "Q2", "Q3", "Q4", "OT"))
    ) %>%
    select(
      home_win,
      home_margin,
      seconds_remaining,
      period_factor,
      home_rating_advantage
    ) %>%
    drop_na()

  message(sprintf("\nTraining on %d observations...", nrow(model_data)))

  # Split data
  set.seed(42)
  data_split <- initial_split(model_data, prop = 0.8, strata = home_win)
  train_data <- training(data_split)
  test_data <- testing(data_split)

  # Create recipe with interactions
  # Key insight: margin matters more when less time remains
  wp_recipe <- recipe(home_win ~ ., data = train_data) %>%
    step_interact(terms = ~ home_margin:seconds_remaining) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_normalize(all_numeric_predictors())

  # Logistic regression specification
  log_spec <- logistic_reg() %>%
    set_engine("glm") %>%
    set_mode("classification")

  # Workflow
  wp_workflow <- workflow() %>%
    add_recipe(wp_recipe) %>%
    add_model(log_spec)

  # Fit model
  wp_fit <- wp_workflow %>% fit(data = train_data)

  # Evaluate on test set
  test_pred <- predict(wp_fit, test_data, type = "prob") %>%
    bind_cols(test_data)

  # Calculate metrics (event_level = "second" because "home" is the second level)
  roc_auc <- roc_auc(test_pred, truth = home_win, .pred_home, event_level = "second")
  brier <- test_pred %>%
    mutate(
      actual = as.numeric(home_win == "home"),
      brier = (actual - .pred_home)^2
    ) %>%
    summarise(brier_score = mean(brier))

  message(sprintf("\nModel Performance:"))
  message(sprintf("  ROC AUC: %.3f", roc_auc$.estimate))
  message(sprintf("  Brier Score: %.4f (lower is better)", brier$brier_score))

  # Calibration check
  calibration <- test_pred %>%
    mutate(prob_bucket = cut(.pred_home, breaks = seq(0, 1, 0.1))) %>%
    group_by(prob_bucket) %>%
    summarise(
      predicted = mean(.pred_home),
      actual = mean(home_win == "home"),
      n = n(),
      .groups = "drop"
    )

  message("\nCalibration (predicted vs actual win rate):")
  print(calibration)

  # Save model
  if (save_model) {
    saveRDS(wp_fit, WP_MODEL_PATH)
    message(sprintf("\nModel saved to: %s", WP_MODEL_PATH))
  }

  list(
    model = wp_fit,
    roc_auc = roc_auc$.estimate,
    brier_score = brier$brier_score,
    calibration = calibration
  )
}

#' Load trained win probability model
load_wp_model <- function(model_path = WP_MODEL_PATH) {
  if (!file.exists(model_path)) {
    stop(sprintf("Model file not found: %s. Run train_wp_model() first.", model_path))
  }
  readRDS(model_path)
}

# ------------------------------------------------------------------------------
# 3. Predict win probability
# ------------------------------------------------------------------------------

#' Calculate win probability for a given game state
#' @param model Trained model (or NULL to load from disk)
#' @param home_margin Current score margin (home - away)
#' @param seconds_remaining Seconds remaining in regulation (negative in OT)
#' @param period Current period (1-4 for regulation, 5+ for OT)
#' @param home_rating_advantage Home team net rating minus away team net rating
#' @return Probability that home team wins (0-1)
predict_wp <- function(model = NULL,
                       home_margin,
                       seconds_remaining,
                       period = 1,
                       home_rating_advantage = 0) {

  if (is.null(model)) {
    model <- load_wp_model()
  }

  # Create prediction data
  new_data <- tibble(
    home_margin = home_margin,
    seconds_remaining = seconds_remaining,
    period_factor = factor(pmin(period, 5), levels = 1:5,
                           labels = c("Q1", "Q2", "Q3", "Q4", "OT")),
    home_rating_advantage = home_rating_advantage
  )

  # Predict
  pred <- predict(model, new_data, type = "prob")

  pred$.pred_home
}

#' Predict win probability for multiple game states
#' @param model Trained model (or NULL to load from disk)
#' @param states Data frame with columns: home_margin, seconds_remaining, period, home_rating_advantage
#' @return Data frame with predictions added
predict_wp_states <- function(model = NULL, states) {

  if (is.null(model)) {
    model <- load_wp_model()
  }

  pred_data <- states %>%
    mutate(
      period_factor = factor(pmin(period, 5), levels = 1:5,
                             labels = c("Q1", "Q2", "Q3", "Q4", "OT"))
    )

  preds <- predict(model, pred_data, type = "prob")

  states %>%
    mutate(home_wp = preds$.pred_home)
}

# ------------------------------------------------------------------------------
# 4. Generate win probability charts
# ------------------------------------------------------------------------------

#' Generate win probability for an entire game
#' @param game_id ESPN game ID
#' @param model Trained model (or NULL to load from disk)
#' @return Data frame with win probability at each play
get_game_wp <- function(game_id, model = NULL, season = 2024) {

  if (is.null(model)) {
    model <- load_wp_model()
  }

  # Load play-by-play for this game
  pbp <- load_nba_pbp(seasons = season) %>%
    filter(game_id == !!game_id)

  if (nrow(pbp) == 0) {
    stop(sprintf("No play-by-play data found for game %s", game_id))
  }

  # Get team ratings (simplified - use 0 if not available)
  home_rating_advantage <- 0  # Can be enhanced with actual ratings

  # Process game states
  game_wp <- pbp %>%
    filter(!is.na(home_score), !is.na(away_score)) %>%
    mutate(
      home_margin = home_score - away_score,
      seconds_elapsed = case_when(
        period_number <= 4 ~ (period_number - 1) * QUARTER_SECONDS +
          (QUARTER_SECONDS - (clock_minutes * 60 + clock_seconds)),
        period_number > 4 ~ REGULATION_SECONDS + (period_number - 5) * OVERTIME_SECONDS +
          (OVERTIME_SECONDS - (clock_minutes * 60 + clock_seconds))
      ),
      seconds_remaining = REGULATION_SECONDS - seconds_elapsed,
      period = period_number,
      home_rating_advantage = home_rating_advantage
    ) %>%
    select(
      game_id,
      play_id = id,
      seconds_elapsed,
      seconds_remaining,
      period,
      home_score,
      away_score,
      home_margin,
      home_rating_advantage,
      text
    )

  # Calculate win probability
  game_wp <- predict_wp_states(model, game_wp)

  game_wp
}

#' Plot win probability chart for a game
plot_game_wp <- function(game_wp, home_team = "Home", away_team = "Away") {

  # Get final winner
  final_margin <- tail(game_wp$home_margin, 1)
  winner <- if_else(final_margin > 0, home_team, away_team)

  # Create plot
  p <- game_wp %>%
    ggplot(aes(x = seconds_elapsed / 60, y = home_wp)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = c(12, 24, 36), linetype = "dotted", color = "gray70") +
    scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent,
      sec.axis = sec_axis(~ 1 - ., labels = scales::percent, name = paste(away_team, "Win Prob"))
    ) +
    scale_x_continuous(
      breaks = c(0, 12, 24, 36, 48),
      labels = c("Start", "Q1", "Q2", "Q3", "End")
    ) +
    labs(
      title = sprintf("Win Probability: %s vs %s", home_team, away_team),
      subtitle = sprintf("Winner: %s", winner),
      x = "Game Time (minutes)",
      y = paste(home_team, "Win Probability")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  p
}

# ------------------------------------------------------------------------------
# 5. Win probability added (WPA) calculations
# ------------------------------------------------------------------------------

#' Calculate win probability added for each play
#' WPA = change in win probability from one play to the next
calculate_wpa <- function(game_wp) {
  game_wp %>%
    arrange(seconds_elapsed) %>%
    mutate(
      prev_wp = lag(home_wp, default = 0.5),
      wpa_home = home_wp - prev_wp,
      wpa_away = -wpa_home
    )
}

#' Get biggest swings in a game
get_biggest_swings <- function(game_wp, n = 10) {
  wpa_data <- calculate_wpa(game_wp)

  wpa_data %>%
    mutate(abs_wpa = abs(wpa_home)) %>%
    slice_max(abs_wpa, n = n) %>%
    select(seconds_elapsed, period, home_score, away_score,
           home_wp, wpa_home, text) %>%
    arrange(desc(abs(wpa_home)))
}

# ------------------------------------------------------------------------------
# 6. Explore relationship with blown lead model
# ------------------------------------------------------------------------------

#' Compare win probability model with blown lead probability
#' Shows how WP relates to probability of blowing a lead
compare_wp_blown_lead <- function(wp_model = NULL) {

  if (is.null(wp_model)) {
    wp_model <- load_wp_model()
  }

  # Generate scenarios: various leads at different times
  scenarios <- expand_grid(
    home_margin = c(5, 10, 15, 20, 25),
    minutes_remaining = c(48, 36, 24, 12, 6, 2),
    home_rating_advantage = c(-5, 0, 5)
  ) %>%
    mutate(
      seconds_remaining = minutes_remaining * 60,
      period = case_when(
        minutes_remaining > 36 ~ 1,
        minutes_remaining > 24 ~ 2,
        minutes_remaining > 12 ~ 3,
        TRUE ~ 4
      )
    )

  # Get win probabilities
  results <- predict_wp_states(wp_model, scenarios) %>%
    mutate(
      blown_lead_prob = 1 - home_wp,  # P(losing) when leading
      rating_label = case_when(
        home_rating_advantage == -5 ~ "Underdog (-5)",
        home_rating_advantage == 0 ~ "Even",
        home_rating_advantage == 5 ~ "Favorite (+5)"
      )
    )

  # Plot
  p <- results %>%
    ggplot(aes(x = minutes_remaining, y = blown_lead_prob,
               color = factor(home_margin), linetype = rating_label)) +
    geom_line(linewidth = 1) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.5)) +
    scale_x_reverse() +
    scale_color_viridis_d(name = "Lead Size") +
    labs(
      title = "Probability of Blowing Lead by Time Remaining",
      subtitle = "Derived from Win Probability Model",
      x = "Minutes Remaining",
      y = "Probability of Losing (Blowing Lead)",
      linetype = "Team Quality"
    ) +
    theme_minimal() +
    theme(legend.position = "right")

  list(scenarios = results, plot = p)
}

# ------------------------------------------------------------------------------
# Example usage
# ------------------------------------------------------------------------------

# # Train model
# result <- train_wp_model(seasons = 2015:2024)
#
# # Single prediction
# predict_wp(
#   home_margin = 10,
#   seconds_remaining = 300,  # 5 minutes left
#   period = 4,
#   home_rating_advantage = 3
# )
#
# # Get win probability for a specific game
# game_wp <- get_game_wp(game_id = 401584668, season = 2024)
# plot_game_wp(game_wp, "Lakers", "Celtics")
#
# # Get biggest swings
# get_biggest_swings(game_wp)
#
# # Compare with blown lead concept
# comparison <- compare_wp_blown_lead()
# print(comparison$plot)
