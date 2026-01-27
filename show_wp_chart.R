# Generate a win probability chart with team-colored lines

source("win_probability_model.R")

# NBA team primary colors
team_colors <- c(
  "Atlanta Hawks" = "#E03A3E",
  "Boston Celtics" = "#007A33",
  "Brooklyn Nets" = "#000000",
  "Charlotte Hornets" = "#1D1160",
  "Chicago Bulls" = "#CE1141",
  "Cleveland Cavaliers" = "#860038",
  "Dallas Mavericks" = "#00538C",
  "Denver Nuggets" = "#0E2240",
  "Detroit Pistons" = "#C8102E",
  "Golden State Warriors" = "#1D428A",
  "Houston Rockets" = "#CE1141",
  "Indiana Pacers" = "#002D62",
  "LA Clippers" = "#C8102E",
  "Los Angeles Lakers" = "#552583",
  "Memphis Grizzlies" = "#5D76A9",
  "Miami Heat" = "#98002E",
  "Milwaukee Bucks" = "#00471B",
  "Minnesota Timberwolves" = "#0C2340",
  "New Orleans Pelicans" = "#0C2340",
  "New York Knicks" = "#006BB6",
  "Oklahoma City Thunder" = "#007AC1",
  "Orlando Magic" = "#0077C0",
  "Philadelphia 76ers" = "#006BB6",
  "Phoenix Suns" = "#1D1160",
  "Portland Trail Blazers" = "#E03A3E",
  "Sacramento Kings" = "#5A2D81",
  "San Antonio Spurs" = "#C4CED4",
  "Toronto Raptors" = "#CE1141",
  "Utah Jazz" = "#002B5C",
  "Washington Wizards" = "#002B5C"
)

# Load schedule to find a close game
cat("Finding close games from 2024 season...\n\n")

schedule <- load_nba_schedule(seasons = 2024) %>%
  filter(status_type_completed == TRUE) %>%
  mutate(margin = abs(home_score - away_score)) %>%
  select(game_id, game_date, home_name = home_display_name, away_name = away_display_name,
         home_score, away_score, margin) %>%
  arrange(margin)

cat("Top 10 closest games:\n")
print(head(schedule, 10))

# Find a game with good color contrast (green vs red, purple vs gold, etc.)
# Look for Celtics, Bucks, Heat, Bulls, Lakers matchups
contrast_games <- schedule %>%
  filter(
    (str_detect(home_name, "Celtics|Bucks") & str_detect(away_name, "Heat|Bulls|Rockets|Raptors")) |
    (str_detect(away_name, "Celtics|Bucks") & str_detect(home_name, "Heat|Bulls|Rockets|Raptors")) |
    (str_detect(home_name, "Lakers") & str_detect(away_name, "Celtics|Bucks")) |
    (str_detect(away_name, "Lakers") & str_detect(home_name, "Celtics|Bucks"))
  )

# If we found contrast games, use the closest one; otherwise use overall closest
if (nrow(contrast_games) > 0) {
  selected_game <- contrast_games[1, ]
  cat("Found high-contrast game!\n")
} else {
  selected_game <- schedule[1, ]
  cat("Using closest game (no high-contrast games found)\n")
}
cat(sprintf("\n\nGenerating WP chart for: %s vs %s (%s)\n",
            selected_game$away_name, selected_game$home_name, selected_game$game_date))
cat(sprintf("Final score: %d - %d\n\n", selected_game$away_score, selected_game$home_score))

# Get team colors
home_team <- selected_game$home_name
away_team <- selected_game$away_name
home_color <- team_colors[home_team]
away_color <- team_colors[away_team]

cat(sprintf("Home team (%s) color: %s\n", home_team, home_color))
cat(sprintf("Away team (%s) color: %s\n", away_team, away_color))

# Load model
model <- load_wp_model()

# Get play-by-play for this game
pbp <- load_nba_pbp(seasons = 2024) %>%
  filter(game_id == selected_game$game_id)

cat(sprintf("\nLoaded %d plays\n", nrow(pbp)))

# Calculate win probability at each moment
game_wp <- pbp %>%
  filter(!is.na(home_score), !is.na(away_score)) %>%
  mutate(
    home_margin = home_score - away_score,
    seconds_elapsed = case_when(
      period_number <= 4 ~ (period_number - 1) * 720 +
        (720 - (clock_minutes * 60 + clock_seconds)),
      period_number > 4 ~ 2880 + (period_number - 5) * 300 +
        (300 - (clock_minutes * 60 + clock_seconds))
    ),
    seconds_remaining = 2880 - seconds_elapsed,
    period = period_number,
    home_rating_advantage = 0
  ) %>%
  select(seconds_elapsed, seconds_remaining, period, home_score, away_score,
         home_margin, home_rating_advantage, text)

# Get predictions
game_wp <- predict_wp_states(model, game_wp)

# Prepare data for colored segments
# Each segment connects consecutive points, colored by who's favored
game_wp_segments <- game_wp %>%
  arrange(seconds_elapsed) %>%
  mutate(
    minutes = seconds_elapsed / 60,
    next_minutes = lead(minutes),
    next_wp = lead(home_wp),
    # Color based on which team is favored at the START of the segment
    favored_team = if_else(home_wp >= 0.5, "home", "away"),
    segment_color = if_else(home_wp >= 0.5, home_color, away_color)
  ) %>%
  filter(!is.na(next_wp))  # Remove last row (no next point)

# Determine if game went to OT
max_period <- max(game_wp$period)
x_max <- if_else(max_period > 4, max(game_wp$seconds_elapsed) / 60, 48)

# Determine winner
final_margin <- tail(game_wp$home_margin, 1)
winner <- if_else(final_margin > 0, home_team, away_team)

cat("\nCreating win probability chart with team colors...\n")

# Create plot with colored segments
p <- ggplot(game_wp_segments) +
  # Draw segments colored by favored team
  geom_segment(
    aes(x = minutes, xend = next_minutes, y = home_wp, yend = next_wp, color = segment_color),
    linewidth = 1.5
  ) +
  scale_color_identity() +
  # 50% line
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  # Quarter markers
  geom_vline(xintercept = c(12, 24, 36), linetype = "dotted", color = "gray60", alpha = 0.7) +
  # Quarter labels
 annotate("text", x = 6, y = 0.03, label = "Q1", color = "gray50", size = 3.5) +
  annotate("text", x = 18, y = 0.03, label = "Q2", color = "gray50", size = 3.5) +
  annotate("text", x = 30, y = 0.03, label = "Q3", color = "gray50", size = 3.5) +
  annotate("text", x = 42, y = 0.03, label = "Q4", color = "gray50", size = 3.5) +
  # Team labels on y-axis
  annotate("text", x = -1.5, y = 0.85, label = home_team, color = home_color,
           size = 3.5, fontface = "bold", hjust = 0) +
  annotate("text", x = -1.5, y = 0.15, label = away_team, color = away_color,
           size = 3.5, fontface = "bold", hjust = 0) +
  # Scales
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent,
    breaks = seq(0, 1, 0.25)
  ) +
  scale_x_continuous(limits = c(-2, x_max), breaks = seq(0, 48, 12)) +
  # Labels
  labs(
    title = sprintf("Win Probability: %s @ %s", away_team, home_team),
    subtitle = sprintf("Final: %s %d - %s %d | Winner: %s",
                       away_team, selected_game$away_score,
                       home_team, selected_game$home_score, winner),
    x = "Game Time (minutes)",
    y = "Win Probability"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

# Save plot
ggsave("wp_chart_colored.png", p, width = 12, height = 6, dpi = 150)
cat("\nChart saved to: wp_chart_colored.png\n")

# Summary stats
cat("\n\nGame Summary:\n")
cat(sprintf("- Starting home WP: %.1f%%\n", game_wp$home_wp[1] * 100))
cat(sprintf("- Min home WP: %.1f%% (at %.1f min)\n",
            min(game_wp$home_wp) * 100,
            game_wp$seconds_elapsed[which.min(game_wp$home_wp)] / 60))
cat(sprintf("- Max home WP: %.1f%% (at %.1f min)\n",
            max(game_wp$home_wp) * 100,
            game_wp$seconds_elapsed[which.max(game_wp$home_wp)] / 60))
cat(sprintf("- Lead changes (crossed 50%%): %d\n",
            sum(abs(diff(game_wp_segments$favored_team == "home")) == 1)))
