library(tidyverse)
library(hoopR)
library(DBI)
library(RSQLite)

# Set higher connection buffer for data loading
Sys.setenv("VROOM_CONNECTION_SIZE" = 262144)

# ------------------------------------------------------------------------------
# Constants
# ------------------------------------------------------------------------------

DB_PATH <- "segment_playoff_database.sqlite"

# Define 10-game segments (games 71-82 in final segment)
SEGMENT_BREAKS <- c(0, 10, 20, 30, 40, 50, 60, 70, 82)
SEGMENT_LABELS <- c("1-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-82")

# ------------------------------------------------------------------------------
# Database functions
# ------------------------------------------------------------------------------

#' Initialize SQLite database for segment analysis
init_segment_database <- function(db_path = DB_PATH) {
  conn <- dbConnect(RSQLite::SQLite(), db_path)

  # Table for segment-level performance
 dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS segment_performance (
      season INTEGER,
      team TEXT,
      segment TEXT,
      games_played INTEGER,
      wins INTEGER,
      losses INTEGER,
      win_pct REAL,
      points_scored INTEGER,
      points_allowed INTEGER,
      point_diff INTEGER,
      avg_point_diff REAL,
      possessions REAL,
      off_rating REAL,
      def_rating REAL,
      net_rating REAL,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (season, team, segment)
    )
  ")

  # Table for playoff results
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS playoff_results (
      season INTEGER,
      team TEXT,
      made_playoffs INTEGER,
      playoff_wins INTEGER,
      playoff_losses INTEGER,
      playoff_win_pct REAL,
      round_reached TEXT,
      champion INTEGER,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (season, team)
    )
  ")

  message(sprintf("Connected to database: %s", db_path))
  conn
}

#' Close database connection
close_database <- function(conn) {
  dbDisconnect(conn)
  message("Database connection closed.")
}

# ------------------------------------------------------------------------------
# 1. Load and process regular season game data
# ------------------------------------------------------------------------------

#' Get team box scores and assign game numbers within season
get_regular_season_games <- function(seasons) {
  message("Loading team box scores...")
  team_box <- load_nba_team_box(seasons = seasons)

  # Filter to regular season only (season_type = 2)
  regular_season <- team_box %>%
    filter(season_type == 2) %>%
    select(
      game_id,
      game_date,
      season,
      team = team_abbreviation,
      team_score,
      opponent_team_score = opponent_team_score,
      team_winner,
      field_goals_attempted,
      free_throws_attempted,
      offensive_rebounds,
      turnovers
    ) %>%
    # Calculate possessions using Dean Oliver formula
    mutate(
      possessions = field_goals_attempted + 0.44 * free_throws_attempted -
        offensive_rebounds + turnovers,
      point_diff = team_score - opponent_team_score,
      win = as.integer(team_winner)
    )

  # Assign game number within each team's season
  regular_season <- regular_season %>%
    arrange(team, season, game_date) %>%
    group_by(team, season) %>%
    mutate(
      game_number = row_number()
    ) %>%
    ungroup()

  message(sprintf("Loaded %d team-game records", nrow(regular_season)))
  regular_season
}

#' Assign games to 10-game segments
assign_segments <- function(games_data) {
  games_data %>%
    mutate(
      segment = cut(
        game_number,
        breaks = SEGMENT_BREAKS,
        labels = SEGMENT_LABELS,
        include.lowest = TRUE,
        right = TRUE
      )
    )
}

# ------------------------------------------------------------------------------
# 2. Calculate segment-level performance metrics
# ------------------------------------------------------------------------------

#' Aggregate performance by team, season, and segment
calculate_segment_performance <- function(games_with_segments) {
  games_with_segments %>%
    group_by(season, team, segment) %>%
    summarise(
      games_played = n(),
      wins = sum(win),
      losses = games_played - wins,
      win_pct = wins / games_played,
      points_scored = sum(team_score),
      points_allowed = sum(opponent_team_score),
      point_diff = points_scored - points_allowed,
      avg_point_diff = point_diff / games_played,
      possessions = sum(possessions),
      off_rating = (points_scored / possessions) * 100,
      def_rating = (points_allowed / possessions) * 100,
      net_rating = off_rating - def_rating,
      .groups = "drop"
    )
}

# ------------------------------------------------------------------------------
# 3. Get playoff results
# ------------------------------------------------------------------------------

#' Load playoff data and determine results for each team
get_playoff_results <- function(seasons) {
  message("Loading playoff data...")

  # Load playoff box scores (season_type = 3)
  playoff_box <- load_nba_team_box(seasons = seasons) %>%
    filter(season_type == 3)

  if (nrow(playoff_box) == 0) {
    message("No playoff data found for specified seasons")
    return(tibble())
  }

  # Get all teams that played in each regular season
  regular_teams <- load_nba_team_box(seasons = seasons) %>%
    filter(season_type == 2) %>%
    select(season, team = team_abbreviation) %>%
    distinct()

  # Calculate playoff performance per team
  playoff_performance <- playoff_box %>%
    group_by(season, team = team_abbreviation) %>%
    summarise(
      playoff_wins = sum(team_winner),
      playoff_losses = n() - playoff_wins,
      playoff_games = n(),
      .groups = "drop"
    ) %>%
    mutate(
      made_playoffs = 1L,
      playoff_win_pct = playoff_wins / playoff_games
    )

  # Determine round reached based on wins
  # NBA playoffs: 4 wins = first round, 8 = second round, 12 = conf finals, 16 = finals win
  playoff_performance <- playoff_performance %>%
    mutate(
      round_reached = case_when(
        playoff_wins >= 16 ~ "Champion",
        playoff_wins >= 12 ~ "Finals",
        playoff_wins >= 8 ~ "Conference Finals",
        playoff_wins >= 4 ~ "Second Round",
        TRUE ~ "First Round"
      ),
      champion = as.integer(playoff_wins >= 16)
    )

  # Join with all regular season teams to include teams that missed playoffs
  full_results <- regular_teams %>%
    left_join(playoff_performance, by = c("season", "team")) %>%
    mutate(
      made_playoffs = coalesce(made_playoffs, 0L),
      playoff_wins = coalesce(playoff_wins, 0L),
      playoff_losses = coalesce(playoff_losses, 0L),
      playoff_win_pct = coalesce(playoff_win_pct, NA_real_),
      round_reached = coalesce(round_reached, "Missed Playoffs"),
      champion = coalesce(champion, 0L)
    ) %>%
    select(-playoff_games)

  message(sprintf("Processed playoff results for %d team-seasons", nrow(full_results)))
  full_results
}

# ------------------------------------------------------------------------------
# 4. Main pipeline
# ------------------------------------------------------------------------------

#' Build complete dataset for segment-playoff analysis
build_segment_playoff_dataset <- function(seasons = 2015:2024,
                                           write_to_db = TRUE,
                                           db_path = DB_PATH) {

  message("=== Building Season Segment Playoff Dataset ===\n")

  # Step 1: Load regular season games
  message("Step 1: Loading regular season games...")
  games <- get_regular_season_games(seasons)

  # Step 2: Assign segments
  message("\nStep 2: Assigning 10-game segments...")
  games_segmented <- assign_segments(games)

  # Verify segment distribution
  segment_counts <- games_segmented %>%
    count(segment) %>%
    mutate(pct = n / sum(n) * 100)
  message("Segment distribution:")
  print(segment_counts)

  # Step 3: Calculate segment performance
  message("\nStep 3: Calculating segment-level performance...")
  segment_performance <- calculate_segment_performance(games_segmented)
  message(sprintf("Created %d segment-performance records", nrow(segment_performance)))

  # Step 4: Get playoff results
  message("\nStep 4: Loading playoff results...")
  playoff_results <- get_playoff_results(seasons)

  # Step 5: Write to database
  if (write_to_db) {
    message("\nStep 5: Writing to database...")
    conn <- init_segment_database(db_path)

    # Write segment performance (convert to plain data.frame for SQLite)
    dbWriteTable(conn, "segment_performance", as.data.frame(segment_performance),
                 overwrite = TRUE, row.names = FALSE)
    message(sprintf("Wrote %d segment performance records", nrow(segment_performance)))

    # Write playoff results (convert to plain data.frame for SQLite)
    dbWriteTable(conn, "playoff_results", as.data.frame(playoff_results),
                 overwrite = TRUE, row.names = FALSE)
    message(sprintf("Wrote %d playoff result records", nrow(playoff_results)))

    close_database(conn)
  }

  message("\n=== Dataset build complete! ===")

  list(
    segment_performance = segment_performance,
    playoff_results = playoff_results
  )
}

# ------------------------------------------------------------------------------
# 5. Analysis functions
# ------------------------------------------------------------------------------

#' Combine segment performance with playoff results
get_analysis_data <- function(db_path = DB_PATH) {
  conn <- dbConnect(RSQLite::SQLite(), db_path)

  segment_perf <- dbGetQuery(conn, "SELECT * FROM segment_performance") %>% as_tibble()
  playoff_res <- dbGetQuery(conn, "SELECT * FROM playoff_results") %>% as_tibble()

  dbDisconnect(conn)

  # Calculate season-long net rating (weighted by games played in each segment)
  season_net_rating <- segment_perf %>%
    filter(!is.na(segment)) %>%
    group_by(season, team) %>%
    summarise(
      season_net_rating = sum(net_rating * games_played) / sum(games_played),
      season_games = sum(games_played),
      season_wins = sum(wins),
      season_win_pct = season_wins / season_games,
      .groups = "drop"
    )

  # Pivot segment data wide for analysis
  segment_wide <- segment_perf %>%
    select(season, team, segment, win_pct, net_rating) %>%
    pivot_wider(
      names_from = segment,
      values_from = c(win_pct, net_rating),
      names_sep = "_"
    )

  # Join with playoff results and season-long metrics
  analysis_data <- segment_wide %>%
    left_join(season_net_rating, by = c("season", "team")) %>%
    left_join(playoff_res, by = c("season", "team"))

  list(
    long = segment_perf %>%
      left_join(season_net_rating, by = c("season", "team")) %>%
      left_join(playoff_res, by = c("season", "team")),
    wide = analysis_data
  )
}

#' Calculate correlation between segment performance and playoff success
analyze_segment_correlations <- function(db_path = DB_PATH) {
  data <- get_analysis_data(db_path)

  # Filter to playoff teams only for playoff win analysis, exclude NA segments
  playoff_teams <- data$long %>%
    filter(made_playoffs == 1, !is.na(segment))

  # Correlation: segment net rating vs playoff wins
  segment_playoff_cor <- playoff_teams %>%
    group_by(segment) %>%
    summarise(
      cor_net_rating_playoff_wins = cor(net_rating, playoff_wins, use = "complete.obs"),
      cor_win_pct_playoff_wins = cor(win_pct, playoff_wins, use = "complete.obs"),
      n = n(),
      .groups = "drop"
    ) %>%
    arrange(segment)

  message("\n=== Segment Correlations with Playoff Wins (Playoff Teams Only) ===")
  print(segment_playoff_cor)

  # Which segment best predicts making the playoffs?
  all_teams <- data$long %>%
    filter(!is.na(segment))

  segment_playoff_made_cor <- all_teams %>%
    group_by(segment) %>%
    summarise(
      cor_net_rating_made_playoffs = cor(net_rating, made_playoffs, use = "complete.obs"),
      cor_win_pct_made_playoffs = cor(win_pct, made_playoffs, use = "complete.obs"),
      n = n(),
      .groups = "drop"
    ) %>%
    arrange(segment)

  message("\n=== Segment Correlations with Making Playoffs (All Teams) ===")
  print(segment_playoff_made_cor)

  list(
    playoff_wins_correlation = segment_playoff_cor,
    made_playoffs_correlation = segment_playoff_made_cor
  )
}

#' Fit regression models to assess segment importance
fit_segment_models <- function(db_path = DB_PATH) {
  data <- get_analysis_data(db_path)

  # Prepare wide data for modeling - filter out NA segment columns
  model_data <- data$wide %>%
    filter(!is.na(playoff_wins), made_playoffs == 1) %>%
    select(
      playoff_wins,
      season_net_rating,
      `net_rating_1-10`, `net_rating_11-20`, `net_rating_21-30`, `net_rating_31-40`,
      `net_rating_41-50`, `net_rating_51-60`, `net_rating_61-70`, `net_rating_71-82`
    ) %>%
    drop_na()

  message(sprintf("\nModeling with %d playoff team-seasons\n", nrow(model_data)))

  # Model 1: Season net rating only (baseline)
  baseline_model <- lm(playoff_wins ~ season_net_rating, data = model_data)

  message("=== Baseline Model: Playoff Wins ~ Season Net Rating Only ===")
  print(summary(baseline_model))

  # Model 2: Full model with season net rating + all segments
  full_model <- lm(playoff_wins ~ ., data = model_data)

  message("\n=== Full Model: Playoff Wins ~ Season Net Rating + All Segments ===")
  print(summary(full_model))

  # Compare models
  message("\n=== Model Comparison (ANOVA) ===")
  print(anova(baseline_model, full_model))

  # Calculate relative importance using standardized coefficients
  model_data_scaled <- model_data %>%
    mutate(across(everything(), ~ as.numeric(scale(.))))

  scaled_model <- lm(playoff_wins ~ ., data = model_data_scaled)

  coef_importance <- tibble(
    predictor = names(coef(scaled_model))[-1],
    std_coefficient = coef(scaled_model)[-1]
  ) %>%
    mutate(
      predictor = str_remove(predictor, "`?net_rating_`?"),
      predictor = str_remove_all(predictor, "`"),
      abs_importance = abs(std_coefficient)
    ) %>%
    arrange(desc(abs_importance))

  message("\n=== Standardized Coefficients (All Predictors) ===")
  print(coef_importance)

  list(
    baseline_model = baseline_model,
    full_model = full_model,
    scaled_model = scaled_model,
    importance = coef_importance,
    r2_baseline = summary(baseline_model)$r.squared,
    r2_full = summary(full_model)$r.squared
  )
}

#' Analyze "hot streak" timing - when do playoff teams peak?
analyze_peak_timing <- function(db_path = DB_PATH) {
  data <- get_analysis_data(db_path)

  # Find peak segment for each team-season (exclude NA segments)
  peak_segments <- data$long %>%
    filter(!is.na(segment)) %>%
    group_by(season, team, made_playoffs, playoff_wins, round_reached) %>%
    slice_max(net_rating, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(season, team, peak_segment = segment, peak_net_rating = net_rating,
           made_playoffs, playoff_wins, round_reached)

  # Compare peak timing between playoff and non-playoff teams
  peak_distribution <- peak_segments %>%
    group_by(made_playoffs, peak_segment) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(made_playoffs) %>%
    mutate(
      pct = count / sum(count) * 100,
      made_playoffs = if_else(made_playoffs == 1, "Playoff Team", "Missed Playoffs")
    )

  message("\n=== Peak Performance Timing by Playoff Status ===")
  print(peak_distribution %>% pivot_wider(names_from = made_playoffs, values_from = c(count, pct)))

  # For playoff teams: does peak timing correlate with playoff success?
  playoff_peak <- peak_segments %>%
    filter(made_playoffs == 1)

  peak_vs_success <- playoff_peak %>%
    group_by(peak_segment) %>%
    summarise(
      n_teams = n(),
      avg_playoff_wins = mean(playoff_wins),
      champions = sum(round_reached == "Champion"),
      .groups = "drop"
    )

  message("\n=== Peak Timing vs Playoff Success ===")
  print(peak_vs_success)

  list(
    peak_distribution = peak_distribution,
    peak_vs_success = peak_vs_success
  )
}

# ------------------------------------------------------------------------------
# Classification Analysis: Predict Playoff Round Reached
# ------------------------------------------------------------------------------

#' Fit classification models to predict playoff round reached
#' Uses multinomial logistic regression and random forest
fit_round_classification <- function(db_path = DB_PATH) {
  library(nnet)      # For multinomial logistic regression
  library(caret)     # For confusion matrix and CV
  library(randomForest)

  data <- get_analysis_data(db_path)

  # Prepare data with round_reached as target
  # Order the factor levels logically
  round_levels <- c("Missed Playoffs", "First Round", "Second Round",
                    "Conference Finals", "Finals", "Champion")

  model_data <- data$wide %>%
    mutate(
      round_reached = factor(round_reached, levels = round_levels, ordered = TRUE)
    ) %>%
    select(
      round_reached,
      season_net_rating,
      `net_rating_1-10`, `net_rating_11-20`, `net_rating_21-30`, `net_rating_31-40`,
      `net_rating_41-50`, `net_rating_51-60`, `net_rating_61-70`, `net_rating_71-82`
    ) %>%
    drop_na() %>%
    # Rename columns to avoid issues with formula interface
    rename(
      seg_01_10 = `net_rating_1-10`,
      seg_11_20 = `net_rating_11-20`,
      seg_21_30 = `net_rating_21-30`,
      seg_31_40 = `net_rating_31-40`,
      seg_41_50 = `net_rating_41-50`,
      seg_51_60 = `net_rating_51-60`,
      seg_61_70 = `net_rating_61-70`,
      seg_71_82 = `net_rating_71-82`
    )

  message(sprintf("\n=== Classification: Predicting Playoff Round ==="))
  message(sprintf("Total observations: %d\n", nrow(model_data)))

  # Class distribution
  class_dist <- model_data %>%
    count(round_reached) %>%
    mutate(pct = n / sum(n) * 100)

  message("Class Distribution:")
  print(class_dist)

  # Split data for validation
  set.seed(42)
  train_idx <- sample(1:nrow(model_data), size = 0.8 * nrow(model_data))
  train_data <- model_data[train_idx, ]
  test_data <- model_data[-train_idx, ]

  # ------------------------------
  # 1. Multinomial Logistic Regression
  # ------------------------------
  message("\n--- Multinomial Logistic Regression ---\n")

  # Fit model (suppress iteration output)
  multinom_model <- multinom(round_reached ~ ., data = train_data, trace = FALSE, MaxNWts = 5000)

  # Predictions
  train_pred_multinom <- predict(multinom_model, train_data)
  test_pred_multinom <- predict(multinom_model, test_data)

  # Confusion matrix
  message("Test Set Confusion Matrix (Multinomial Logistic):")
  cm_multinom <- confusionMatrix(test_pred_multinom, test_data$round_reached)
  print(cm_multinom$table)
  message(sprintf("\nOverall Accuracy: %.1f%%", cm_multinom$overall["Accuracy"] * 100))

  # Variable importance from coefficients
  coefs <- coef(multinom_model)
  avg_abs_coef <- colMeans(abs(coefs))
  importance_multinom <- tibble(
    predictor = names(avg_abs_coef),
    importance = avg_abs_coef
  ) %>%
    filter(predictor != "(Intercept)") %>%
    mutate(
      predictor = case_when(
        predictor == "season_net_rating" ~ "Season Net Rating",
        TRUE ~ str_replace(str_replace(predictor, "seg_", ""), "_", "-")
      )
    ) %>%
    arrange(desc(importance))

  message("\nVariable Importance (avg absolute coefficient):")
  print(importance_multinom)

  # ------------------------------
  # 2. Random Forest
  # ------------------------------
  message("\n--- Random Forest Classification ---\n")

  # Convert ordered factor to unordered for RF
  train_data_rf <- train_data %>% mutate(round_reached = factor(round_reached, ordered = FALSE))
  test_data_rf <- test_data %>% mutate(round_reached = factor(round_reached, ordered = FALSE))

  rf_model <- randomForest(round_reached ~ ., data = train_data_rf,
                           ntree = 500, importance = TRUE)

  # Predictions
  test_pred_rf <- predict(rf_model, test_data_rf)

  # Confusion matrix
  message("Test Set Confusion Matrix (Random Forest):")
  cm_rf <- confusionMatrix(test_pred_rf, test_data_rf$round_reached)
  print(cm_rf$table)
  message(sprintf("\nOverall Accuracy: %.1f%%", cm_rf$overall["Accuracy"] * 100))

  # Variable importance
  importance_rf <- importance(rf_model, type = 1) %>%
    as.data.frame() %>%
    rownames_to_column("predictor") %>%
    as_tibble() %>%
    rename(importance = MeanDecreaseAccuracy) %>%
    mutate(
      predictor = case_when(
        predictor == "season_net_rating" ~ "Season Net Rating",
        TRUE ~ str_replace(str_replace(predictor, "seg_", ""), "_", "-")
      )
    ) %>%
    arrange(desc(importance))

  message("\nVariable Importance (Mean Decrease Accuracy):")
  print(importance_rf)

  # ------------------------------
  # 3. Per-class accuracy
  # ------------------------------
  message("\n--- Per-Class Performance (Random Forest) ---")
  per_class <- cm_rf$byClass[, c("Sensitivity", "Specificity", "Balanced Accuracy")]
  print(round(per_class, 3))

  list(
    multinom_model = multinom_model,
    rf_model = rf_model,
    confusion_multinom = cm_multinom,
    confusion_rf = cm_rf,
    importance_multinom = importance_multinom,
    importance_rf = importance_rf,
    class_distribution = class_dist
  )
}

#' Simplified classification: Predict playoff tier (simplified classes)
fit_tier_classification <- function(db_path = DB_PATH) {
  library(nnet)
  library(caret)
  library(randomForest)

  data <- get_analysis_data(db_path)

  # Create simplified tiers
  tier_levels <- c("Missed", "First Round Exit", "Deep Run", "Champion")

  model_data <- data$wide %>%
    mutate(
      playoff_tier = case_when(
        round_reached == "Missed Playoffs" ~ "Missed",
        round_reached == "First Round" ~ "First Round Exit",
        round_reached %in% c("Second Round", "Conference Finals", "Finals") ~ "Deep Run",
        round_reached == "Champion" ~ "Champion"
      ),
      playoff_tier = factor(playoff_tier, levels = tier_levels)
    ) %>%
    select(
      playoff_tier,
      season_net_rating,
      `net_rating_1-10`, `net_rating_11-20`, `net_rating_21-30`, `net_rating_31-40`,
      `net_rating_41-50`, `net_rating_51-60`, `net_rating_61-70`, `net_rating_71-82`
    ) %>%
    drop_na() %>%
    rename(
      seg_01_10 = `net_rating_1-10`,
      seg_11_20 = `net_rating_11-20`,
      seg_21_30 = `net_rating_21-30`,
      seg_31_40 = `net_rating_31-40`,
      seg_41_50 = `net_rating_41-50`,
      seg_51_60 = `net_rating_51-60`,
      seg_61_70 = `net_rating_61-70`,
      seg_71_82 = `net_rating_71-82`
    )

  message(sprintf("\n=== Simplified Tier Classification ==="))
  message(sprintf("Total observations: %d\n", nrow(model_data)))

  # Class distribution
  class_dist <- model_data %>%
    count(playoff_tier) %>%
    mutate(pct = n / sum(n) * 100)

  message("Tier Distribution:")
  print(class_dist)

  # Split data
  set.seed(42)
  train_idx <- sample(1:nrow(model_data), size = 0.8 * nrow(model_data))
  train_data <- model_data[train_idx, ]
  test_data <- model_data[-train_idx, ]

  # Random Forest
  message("\n--- Random Forest (Simplified Tiers) ---\n")

  rf_model <- randomForest(playoff_tier ~ ., data = train_data,
                           ntree = 500, importance = TRUE)

  test_pred <- predict(rf_model, test_data)

  cm <- confusionMatrix(test_pred, test_data$playoff_tier)
  message("Confusion Matrix:")
  print(cm$table)
  message(sprintf("\nOverall Accuracy: %.1f%%", cm$overall["Accuracy"] * 100))

  # Variable importance
  importance_df <- importance(rf_model, type = 1) %>%
    as.data.frame() %>%
    rownames_to_column("predictor") %>%
    as_tibble() %>%
    rename(importance = MeanDecreaseAccuracy) %>%
    mutate(
      predictor = case_when(
        predictor == "season_net_rating" ~ "Season Net Rating",
        TRUE ~ str_replace(str_replace(predictor, "seg_", ""), "_", "-")
      )
    ) %>%
    arrange(desc(importance))

  message("\nVariable Importance (Mean Decrease Accuracy):")
  print(importance_df)

  message("\n--- Per-Tier Performance ---")
  per_class <- cm$byClass[, c("Sensitivity", "Specificity", "Balanced Accuracy")]
  print(round(per_class, 3))

  list(
    model = rf_model,
    confusion = cm,
    importance = importance_df,
    tier_distribution = class_dist
  )
}

#' Visualize segment importance
plot_segment_analysis <- function(db_path = DB_PATH) {
  correlations <- analyze_segment_correlations(db_path)

  # Plot correlation with playoff wins
  p1 <- correlations$playoff_wins_correlation %>%
    ggplot(aes(x = segment, y = cor_net_rating_playoff_wins)) +
    geom_col(fill = "steelblue") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Segment Net Rating Correlation with Playoff Wins",
      subtitle = "Which part of the season best predicts playoff success?",
      x = "Season Segment (Games)",
      y = "Correlation with Playoff Wins"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Plot correlation with making playoffs
  p2 <- correlations$made_playoffs_correlation %>%
    ggplot(aes(x = segment, y = cor_net_rating_made_playoffs)) +
    geom_col(fill = "darkgreen") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Segment Net Rating Correlation with Making Playoffs",
      subtitle = "Which part of the season best predicts playoff qualification?",
      x = "Season Segment (Games)",
      y = "Correlation with Making Playoffs"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  list(playoff_wins_plot = p1, made_playoffs_plot = p2)
}

# ------------------------------------------------------------------------------
# 6. Run analysis
# ------------------------------------------------------------------------------

# Example usage:
#
# # Build the dataset (run once)
# data <- build_segment_playoff_dataset(seasons = 2015:2024)
#
# # Run analysis
# correlations <- analyze_segment_correlations()
# models <- fit_segment_models()
# peak_analysis <- analyze_peak_timing()
# plots <- plot_segment_analysis()
#
# # View plots
# print(plots$playoff_wins_plot)
# print(plots$made_playoffs_plot)
