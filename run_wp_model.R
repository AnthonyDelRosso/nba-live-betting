# Train and evaluate win probability model

source("win_probability_model.R")

cat("========================================\n")
cat("TRAINING WIN PROBABILITY MODEL\n")
cat("========================================\n\n")

# Train the model
result <- train_wp_model(seasons = 2015:2024)

cat("\n\n========================================\n")
cat("EXAMPLE PREDICTIONS\n")
cat("========================================\n\n")

model <- result$model

# Test various scenarios
scenarios <- tibble(
  description = c(
    "Start of game (neutral)",
    "Start of game (home favorite +5)",
    "Up 10 at halftime",
    "Up 10 with 5 min left",
    "Up 5 with 1 min left",
    "Down 3 with 30 sec left",
    "Tied with 10 sec left",
    "Up 20 at halftime",
    "Up 20 with 2 min left"
  ),
  home_margin = c(0, 0, 10, 10, 5, -3, 0, 20, 20),
  seconds_remaining = c(2880, 2880, 1440, 300, 60, 30, 10, 1440, 120),
  period = c(1, 1, 2, 4, 4, 4, 4, 2, 4),
  home_rating_advantage = c(0, 5, 0, 0, 0, 0, 0, 0, 0)
)

predictions <- predict_wp_states(model, scenarios)

cat("Win Probability Predictions:\n\n")
predictions %>%
  mutate(
    home_wp_pct = sprintf("%.1f%%", home_wp * 100),
    mins_left = seconds_remaining / 60
  ) %>%
  select(description, home_margin, mins_left, period, home_rating_advantage, home_wp_pct) %>%
  print(n = 20)

cat("\n\n========================================\n")
cat("BLOWN LEAD PROBABILITY COMPARISON\n")
cat("========================================\n\n")

comparison <- compare_wp_blown_lead(model)

cat("Probability of blowing various leads:\n\n")
comparison$scenarios %>%
  filter(home_rating_advantage == 0) %>%
  select(home_margin, minutes_remaining, home_wp, blown_lead_prob) %>%
  mutate(
    home_wp = sprintf("%.1f%%", home_wp * 100),
    blown_lead_prob = sprintf("%.1f%%", blown_lead_prob * 100)
  ) %>%
  pivot_wider(
    names_from = minutes_remaining,
    values_from = c(home_wp, blown_lead_prob),
    names_glue = "{.value}_{minutes_remaining}min"
  ) %>%
  print(width = 200)

cat("\n=== Model Training Complete ===\n")
