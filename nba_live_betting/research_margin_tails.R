# Step 1 of the spread-straddle research: are NBA final-score margins
# fatter-tailed than a normal/Gaussian model would predict?
#
# Method: for each team-season, compute a rolling (strictly prior-games-only)
# net rating entering each game, require >=10 games of in-season history to
# avoid noisy early-season estimates, fit actual home margin ~ rating_diff
# (this calibrates both the home-court-advantage intercept and the points-
# per-rating-point slope directly from the data), then study the residual
# distribution: empirical tail frequency at various point thresholds vs. what
# a normal distribution with the same variance would predict.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) setwd(args[1])

library(hoopR)
library(tidyverse)

SEASONS <- 2021:2025  # 5 most recent complete seasons
MIN_PRIOR_GAMES <- 10

cat(sprintf("Loading team box scores for seasons: %s...\n", paste(SEASONS, collapse = ", ")))
team_box <- load_nba_team_box(seasons = SEASONS)
team_box <- team_box %>% filter(season_type == 2)  # regular season only
cat(sprintf("Loaded %d team-game rows (%d games)\n", nrow(team_box), nrow(team_box) / 2))

# --- Rolling, look-ahead-free net rating entering each game ---
team_box <- team_box %>%
  arrange(team_abbreviation, season, game_date) %>%
  mutate(poss = field_goals_attempted + 0.44 * free_throws_attempted -
           offensive_rebounds + turnovers) %>%
  group_by(team_abbreviation, season) %>%
  mutate(
    game_num = row_number(),
    games_played_prior = game_num - 1,
    cum_pts = cumsum(team_score) - team_score,
    cum_pts_allowed = cumsum(opponent_team_score) - opponent_team_score,
    cum_poss = cumsum(poss) - poss,
    net_rating_entering = if_else(
      games_played_prior >= MIN_PRIOR_GAMES & cum_poss > 0,
      (cum_pts / cum_poss * 100) - (cum_pts_allowed / cum_poss * 100),
      NA_real_
    )
  ) %>%
  ungroup()

# --- Reassemble one row per game from the home/away team-game rows ---
home_rows <- team_box %>%
  filter(team_home_away == "home") %>%
  select(game_id, season, game_date,
         home_team = team_abbreviation, home_score = team_score,
         home_net_rating = net_rating_entering, home_games_prior = games_played_prior)

away_rows <- team_box %>%
  filter(team_home_away == "away") %>%
  select(game_id,
         away_team = team_abbreviation, away_score = team_score,
         away_net_rating = net_rating_entering, away_games_prior = games_played_prior)

games <- inner_join(home_rows, away_rows, by = "game_id") %>%
  mutate(
    home_margin = home_score - away_score,
    rating_diff = home_net_rating - away_net_rating
  ) %>%
  filter(home_games_prior >= MIN_PRIOR_GAMES, away_games_prior >= MIN_PRIOR_GAMES,
         !is.na(rating_diff))

cat(sprintf("\nUsable games after requiring %d+ prior games for both teams: %d\n",
            MIN_PRIOR_GAMES, nrow(games)))

# --- Fit expected margin: calibrates HCA intercept + pts-per-rating-point slope ---
fit <- lm(home_margin ~ rating_diff, data = games)
cat("\nFitted model: home_margin ~ rating_diff\n")
print(summary(fit)$coefficients)
cat(sprintf("Residual SD: %.2f points | R^2: %.3f\n",
            sd(resid(fit)), summary(fit)$r.squared))

resid_vals <- resid(fit)
resid_sd <- sd(resid_vals)
resid_mean <- mean(resid_vals)

skewness <- mean((resid_vals - resid_mean)^3) / resid_sd^3
exc_kurtosis <- mean((resid_vals - resid_mean)^4) / resid_sd^4 - 3
cat(sprintf("\nResidual skewness: %.3f (0 = symmetric)\n", skewness))
cat(sprintf("Residual excess kurtosis: %.3f (0 = normal, >0 = fat tails)\n", exc_kurtosis))

# --- Core test: empirical tail frequency vs. normal-implied, by point threshold ---
thresholds <- c(10, 15, 20, 25, 30, 35)
tail_table <- map_dfr(thresholds, function(k) {
  emp_p <- mean(abs(resid_vals) > k)
  norm_p <- 2 * (1 - pnorm(k / resid_sd))
  n_obs <- length(resid_vals)
  bt <- binom.test(sum(abs(resid_vals) > k), n_obs, p = norm_p, alternative = "greater")
  tibble(
    threshold_pts = k,
    n_exceeding = sum(abs(resid_vals) > k),
    empirical_prob = emp_p,
    normal_implied_prob = norm_p,
    ratio_emp_to_normal = emp_p / norm_p,
    p_value_excess = bt$p.value
  )
})

cat("\n=== Tail frequency: empirical vs. normal-implied ===\n")
cat("(ratio > 1 means blowouts of that size happen MORE often than a normal\n")
cat(" distribution with the same variance would predict; p_value_excess tests\n")
cat(" whether that excess is distinguishable from sampling noise)\n\n")
print(as.data.frame(tail_table), row.names = FALSE, digits = 4)

# --- Secondary cut: does tail-fatness concentrate in lopsided matchups? ---
games <- games %>% mutate(
  expected_margin = predict(fit),
  abs_rating_diff = abs(rating_diff),
  matchup_bucket = cut(abs_rating_diff, breaks = c(-Inf, 3, 7, Inf),
                        labels = c("close (<3)", "moderate (3-7)", "lopsided (7+)"))
)

cat("\n=== By matchup type: P(|residual| > 20 pts) ===\n")
bucket_summary <- games %>%
  mutate(resid = home_margin - expected_margin) %>%
  group_by(matchup_bucket) %>%
  summarise(
    n = n(),
    resid_sd = sd(resid),
    p_blowout_20 = mean(abs(resid) > 20),
    normal_implied_20 = 2 * (1 - pnorm(20 / sd(resid))),
    .groups = "drop"
  )
print(as.data.frame(bucket_summary), row.names = FALSE, digits = 4)

cat("\nDone.\n")
