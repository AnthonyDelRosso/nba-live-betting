# Analyze In-Game Adjustment Thresholds
# Empirically determine optimal thresholds for:
#   1. Price velocity (trailer price change per minute)
#   2. Run differential (scoring momentum)
#
# Method:
#   - Calculate metrics for all potential fade entry points
#   - Analyze correlation with actual outcomes
#   - Grid search for optimal thresholds

library(tidyverse)
library(hoopR)

source("kalshi_predictor.R")

message("\n", strrep("=", 70))
message("  ANALYZING IN-GAME ADJUSTMENT THRESHOLDS")
message(strrep("=", 70), "\n")

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

message("Loading data...")
kalshi_data <- readRDS("kalshi_nba_full_data.rds") %>%
  filter(market_team == home_team)

schedule <- load_nba_schedule(seasons = 2026) %>%
  filter(status_type_completed == TRUE) %>%
  select(game_id, game_date, home_abbrev = home_abbreviation,
         away_abbrev = away_abbreviation, home_winner) %>%
  mutate(game_date = as.Date(game_date))

pbp <- load_nba_pbp(seasons = 2026)

message(sprintf("  %d Kalshi observations", nrow(kalshi_data)))
message(sprintf("  %d completed games", nrow(schedule)))

# ------------------------------------------------------------------------------
# Build Dataset: All Potential Fade Entry Points with Metrics
# ------------------------------------------------------------------------------

message("\nBuilding analysis dataset...")

all_entries <- list()
entry_count <- 0

unique_games <- kalshi_data %>%
  distinct(ticker, game_date, home_team, away_team, result)

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

  home_won <- sched_game$home_winner

  # Get PBP
  game_pbp <- pbp %>%
    filter(game_id == sched_game$game_id) %>%
    filter(!is.na(home_score), !is.na(away_score), !is.na(wallclock)) %>%
    mutate(
      pbp_timestamp = as.POSIXct(wallclock, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      home_margin = home_score - away_score
    ) %>%
    arrange(pbp_timestamp)

  if (nrow(game_pbp) == 0) next

  # Get Kalshi data
  game_kalshi <- kalshi_data %>%
    filter(ticker == game_info$ticker) %>%
    mutate(timestamp = as.POSIXct(timestamp, tz = "UTC")) %>%
    arrange(timestamp)

  if (nrow(game_kalshi) < 10) next  # Need enough data for velocity

  # Process each snapshot
  for (i in 1:nrow(game_kalshi)) {
    snap <- game_kalshi[i, ]
    market_price <- snap$price

    # Find closest PBP
    pbp_match <- game_pbp %>%
      mutate(time_diff = abs(as.numeric(difftime(pbp_timestamp, snap$timestamp, units = "secs")))) %>%
      filter(time_diff < 180) %>%
      arrange(time_diff) %>%
      slice(1)

    if (nrow(pbp_match) == 0) next

    home_margin <- pbp_match$home_margin
    home_leading <- home_margin > 0
    lead <- abs(home_margin)

    # Only analyze fade-eligible situations (7+ pt lead)
    if (lead < 7) next

    period <- pbp_match$period_number %||% 1
    if (period > 4) next  # Skip OT

    trailer_price <- if (home_leading) (1 - market_price) else market_price

    # Only analyze when trailer >= 20c (potential fade situations)
    if (trailer_price < 0.20) next

    # ========================================
    # CALCULATE PRICE VELOCITY
    # ========================================

    # Look back 5 minutes for price history
    lookback_time <- snap$timestamp - 300  # 5 min ago

    price_5min_ago <- game_kalshi %>%
      filter(timestamp <= lookback_time) %>%
      arrange(desc(timestamp)) %>%
      slice(1)

    velocity <- NA
    if (nrow(price_5min_ago) > 0) {
      old_trailer_price <- if (home_leading) (1 - price_5min_ago$price) else price_5min_ago$price
      time_diff_mins <- as.numeric(difftime(snap$timestamp, price_5min_ago$timestamp, units = "mins"))
      if (time_diff_mins >= 2 && time_diff_mins <= 10) {
        velocity <- (trailer_price - old_trailer_price) / time_diff_mins
      }
    }

    # ========================================
    # CALCULATE RUN DIFFERENTIAL
    # ========================================

    lookback_pbp_time <- snap$timestamp - 300

    recent_pbp <- game_pbp %>%
      filter(pbp_timestamp >= lookback_pbp_time, pbp_timestamp <= snap$timestamp)

    run_diff <- NA
    trailer_pts <- NA
    leader_pts <- NA

    if (nrow(recent_pbp) >= 2) {
      start_pbp <- recent_pbp %>% arrange(pbp_timestamp) %>% slice(1)
      end_pbp <- recent_pbp %>% arrange(desc(pbp_timestamp)) %>% slice(1)

      home_pts <- end_pbp$home_score - start_pbp$home_score
      away_pts <- end_pbp$away_score - start_pbp$away_score

      if (home_leading) {
        trailer_pts <- away_pts
        leader_pts <- home_pts
      } else {
        trailer_pts <- home_pts
        leader_pts <- away_pts
      }

      run_diff <- trailer_pts - leader_pts
    }

    # ========================================
    # DETERMINE OUTCOME
    # ========================================

    # Fade wins if leader loses
    fade_won <- if (home_leading) !home_won else home_won

    # ========================================
    # RECORD ENTRY
    # ========================================

    entry_count <- entry_count + 1

    all_entries[[entry_count]] <- tibble(
      game_id = as.character(sched_game$game_id),
      timestamp = snap$timestamp,
      period = period,
      lead = lead,
      trailer_price = trailer_price,
      velocity = velocity,
      run_diff = run_diff,
      trailer_pts = trailer_pts,
      leader_pts = leader_pts,
      fade_won = fade_won
    )
  }

  if (g %% 20 == 0) {
    message(sprintf("  Processed %d/%d games, %d entries...", g, nrow(unique_games), entry_count))
  }
}

entries_df <- bind_rows(all_entries)
message(sprintf("\nTotal entries analyzed: %d", nrow(entries_df)))

# ------------------------------------------------------------------------------
# ANALYSIS 1: Price Velocity vs Win Rate
# ------------------------------------------------------------------------------

message("\n", strrep("=", 70))
message("  ANALYSIS 1: PRICE VELOCITY")
message(strrep("=", 70), "\n")

vel_data <- entries_df %>% filter(!is.na(velocity))
message(sprintf("Entries with velocity data: %d (%.1f%%)",
                nrow(vel_data), nrow(vel_data)/nrow(entries_df)*100))

if (nrow(vel_data) > 50) {
  # Distribution
  message("\nVelocity Distribution (c/min):")
  message(sprintf("  Min:    %.2f", min(vel_data$velocity) * 100))
  message(sprintf("  25th:   %.2f", quantile(vel_data$velocity, 0.25) * 100))
  message(sprintf("  Median: %.2f", quantile(vel_data$velocity, 0.50) * 100))
  message(sprintf("  75th:   %.2f", quantile(vel_data$velocity, 0.75) * 100))
  message(sprintf("  Max:    %.2f", max(vel_data$velocity) * 100))

  # Win rate by velocity bucket
  message("\nWin Rate by Velocity Bucket:")
  vel_buckets <- vel_data %>%
    mutate(vel_bucket = case_when(
      velocity <= -0.03 ~ "a) <= -3c/min (sharp drop)",
      velocity <= -0.02 ~ "b) -3 to -2c/min",
      velocity <= -0.01 ~ "c) -2 to -1c/min",
      velocity <= 0.01 ~ "d) -1 to +1c/min (stable)",
      velocity <= 0.02 ~ "e) +1 to +2c/min",
      velocity <= 0.03 ~ "f) +2 to +3c/min",
      TRUE ~ "g) > +3c/min (sharp spike)"
    )) %>%
    group_by(vel_bucket) %>%
    summarise(
      n = n(),
      wins = sum(fade_won),
      win_rate = mean(fade_won) * 100,
      avg_trailer_price = mean(trailer_price),
      .groups = "drop"
    ) %>%
    arrange(vel_bucket)

  print(vel_buckets)

  # Correlation
  vel_cor <- cor(vel_data$velocity, vel_data$fade_won, use = "complete.obs")
  message(sprintf("\nCorrelation (velocity vs fade_won): %.3f", vel_cor))

  # Statistical test
  if (sum(vel_data$velocity >= 0.02) > 10 && sum(vel_data$velocity <= -0.02) > 10) {
    spike_wr <- mean(vel_data$fade_won[vel_data$velocity >= 0.02])
    dip_wr <- mean(vel_data$fade_won[vel_data$velocity <= -0.02])
    message(sprintf("\nSpike (>=2c/min) win rate: %.1f%% (n=%d)",
                   spike_wr * 100, sum(vel_data$velocity >= 0.02)))
    message(sprintf("Dip (<=-2c/min) win rate:  %.1f%% (n=%d)",
                   dip_wr * 100, sum(vel_data$velocity <= -0.02)))
    message(sprintf("Difference: %.1f%% (dip - spike)", (dip_wr - spike_wr) * 100))
  }
}

# ------------------------------------------------------------------------------
# ANALYSIS 2: Run Differential vs Win Rate
# ------------------------------------------------------------------------------

message("\n", strrep("=", 70))
message("  ANALYSIS 2: RUN DIFFERENTIAL")
message(strrep("=", 70), "\n")

run_data <- entries_df %>% filter(!is.na(run_diff))
message(sprintf("Entries with run diff data: %d (%.1f%%)",
                nrow(run_data), nrow(run_data)/nrow(entries_df)*100))

if (nrow(run_data) > 50) {
  # Distribution
  message("\nRun Differential Distribution (pts):")
  message(sprintf("  Min:    %d", min(run_data$run_diff)))
  message(sprintf("  25th:   %d", quantile(run_data$run_diff, 0.25)))
  message(sprintf("  Median: %d", quantile(run_data$run_diff, 0.50)))
  message(sprintf("  75th:   %d", quantile(run_data$run_diff, 0.75)))
  message(sprintf("  Max:    %d", max(run_data$run_diff)))

  # Win rate by run differential bucket
  message("\nWin Rate by Run Differential:")
  run_buckets <- run_data %>%
    mutate(run_bucket = case_when(
      run_diff <= -10 ~ "a) <= -10 (leader dominating)",
      run_diff <= -5 ~ "b) -10 to -5",
      run_diff <= -1 ~ "c) -5 to -1",
      run_diff <= 1 ~ "d) -1 to +1 (even)",
      run_diff <= 5 ~ "e) +1 to +5",
      run_diff <= 10 ~ "f) +5 to +10",
      TRUE ~ "g) > +10 (trailer surging)"
    )) %>%
    group_by(run_bucket) %>%
    summarise(
      n = n(),
      wins = sum(fade_won),
      win_rate = mean(fade_won) * 100,
      avg_trailer_price = mean(trailer_price),
      .groups = "drop"
    ) %>%
    arrange(run_bucket)

  print(run_buckets)

  # Correlation
  run_cor <- cor(run_data$run_diff, run_data$fade_won, use = "complete.obs")
  message(sprintf("\nCorrelation (run_diff vs fade_won): %.3f", run_cor))

  # Statistical test
  if (sum(run_data$run_diff >= 8) > 10 && sum(run_data$run_diff <= -8) > 10) {
    trailer_run_wr <- mean(run_data$fade_won[run_data$run_diff >= 8])
    leader_run_wr <- mean(run_data$fade_won[run_data$run_diff <= -8])
    message(sprintf("\nTrailer on run (>=8 pts) win rate: %.1f%% (n=%d)",
                   trailer_run_wr * 100, sum(run_data$run_diff >= 8)))
    message(sprintf("Leader on run (<=-8 pts) win rate: %.1f%% (n=%d)",
                   leader_run_wr * 100, sum(run_data$run_diff <= -8)))
    message(sprintf("Difference: %.1f%%", (trailer_run_wr - leader_run_wr) * 100))
  }
}

# ------------------------------------------------------------------------------
# ANALYSIS 3: Grid Search for Optimal Thresholds
# ------------------------------------------------------------------------------

message("\n", strrep("=", 70))
message("  ANALYSIS 3: OPTIMAL THRESHOLD SEARCH")
message(strrep("=", 70), "\n")

# Only use entries that would pass basic fade filter (trailer >= 35c)
fade_eligible <- entries_df %>%
  filter(trailer_price >= 0.35)

message(sprintf("Fade-eligible entries (trailer >= 35c): %d", nrow(fade_eligible)))
message(sprintf("  With velocity data: %d", sum(!is.na(fade_eligible$velocity))))
message(sprintf("  With run diff data: %d", sum(!is.na(fade_eligible$run_diff))))

# Baseline win rate
baseline_wr <- mean(fade_eligible$fade_won) * 100
message(sprintf("\nBaseline win rate (all fade-eligible): %.1f%%", baseline_wr))

# Grid search for velocity threshold
message("\n--- Velocity Threshold Search ---")
vel_thresholds <- seq(0.01, 0.05, by = 0.005)

vel_results <- map_dfr(vel_thresholds, function(thresh) {
  eligible_vel <- fade_eligible %>% filter(!is.na(velocity))

  # Contrarian logic: boost on dips, penalize on spikes
  dip_entries <- eligible_vel %>% filter(velocity <= -thresh)
  spike_entries <- eligible_vel %>% filter(velocity >= thresh)
  neutral_entries <- eligible_vel %>% filter(abs(velocity) < thresh)

  tibble(
    threshold = thresh * 100,
    dip_n = nrow(dip_entries),
    dip_wr = if (nrow(dip_entries) > 0) mean(dip_entries$fade_won) * 100 else NA,
    spike_n = nrow(spike_entries),
    spike_wr = if (nrow(spike_entries) > 0) mean(spike_entries$fade_won) * 100 else NA,
    neutral_n = nrow(neutral_entries),
    neutral_wr = if (nrow(neutral_entries) > 0) mean(neutral_entries$fade_won) * 100 else NA,
    dip_vs_spike = if (!is.na(dip_wr) && !is.na(spike_wr)) dip_wr - spike_wr else NA
  )
})

print(vel_results)

# Find best velocity threshold (max dip vs spike difference)
best_vel <- vel_results %>%
  filter(!is.na(dip_vs_spike), dip_n >= 10, spike_n >= 10) %>%
  arrange(desc(dip_vs_spike)) %>%
  slice(1)

if (nrow(best_vel) > 0) {
  message(sprintf("\nBest velocity threshold: %.1fc/min", best_vel$threshold))
  message(sprintf("  Dip win rate: %.1f%% (n=%d)", best_vel$dip_wr, best_vel$dip_n))
  message(sprintf("  Spike win rate: %.1f%% (n=%d)", best_vel$spike_wr, best_vel$spike_n))
  message(sprintf("  Advantage: %.1f%%", best_vel$dip_vs_spike))
}

# Grid search for run differential threshold
message("\n--- Run Differential Threshold Search ---")
run_thresholds <- seq(4, 12, by = 2)

run_results <- map_dfr(run_thresholds, function(thresh) {
  eligible_run <- fade_eligible %>% filter(!is.na(run_diff))

  trailer_run <- eligible_run %>% filter(run_diff >= thresh)
  leader_run <- eligible_run %>% filter(run_diff <= -thresh)
  neutral <- eligible_run %>% filter(abs(run_diff) < thresh)

  tibble(
    threshold = thresh,
    trailer_run_n = nrow(trailer_run),
    trailer_run_wr = if (nrow(trailer_run) > 0) mean(trailer_run$fade_won) * 100 else NA,
    leader_run_n = nrow(leader_run),
    leader_run_wr = if (nrow(leader_run) > 0) mean(leader_run$fade_won) * 100 else NA,
    neutral_n = nrow(neutral),
    neutral_wr = if (nrow(neutral) > 0) mean(neutral$fade_won) * 100 else NA,
    trailer_vs_leader = if (!is.na(trailer_run_wr) && !is.na(leader_run_wr)) trailer_run_wr - leader_run_wr else NA
  )
})

print(run_results)

# Find best run threshold
best_run <- run_results %>%
  filter(!is.na(trailer_vs_leader), trailer_run_n >= 10, leader_run_n >= 10) %>%
  arrange(desc(trailer_vs_leader)) %>%
  slice(1)

if (nrow(best_run) > 0) {
  message(sprintf("\nBest run differential threshold: %d pts", best_run$threshold))
  message(sprintf("  Trailer on run win rate: %.1f%% (n=%d)", best_run$trailer_run_wr, best_run$trailer_run_n))
  message(sprintf("  Leader on run win rate: %.1f%% (n=%d)", best_run$leader_run_wr, best_run$leader_run_n))
  message(sprintf("  Advantage: %.1f%%", best_run$trailer_vs_leader))
}

# ------------------------------------------------------------------------------
# ANALYSIS 4: Combined Effect
# ------------------------------------------------------------------------------

message("\n", strrep("=", 70))
message("  ANALYSIS 4: COMBINED EFFECTS")
message(strrep("=", 70), "\n")

combined_data <- fade_eligible %>%
  filter(!is.na(velocity), !is.na(run_diff))

message(sprintf("Entries with both velocity AND run diff: %d", nrow(combined_data)))

if (nrow(combined_data) > 50) {
  # Create combined signal
  combined_analysis <- combined_data %>%
    mutate(
      vel_signal = case_when(
        velocity <= -0.02 ~ "dip",
        velocity >= 0.02 ~ "spike",
        TRUE ~ "neutral"
      ),
      run_signal = case_when(
        run_diff >= 6 ~ "trailer_momentum",
        run_diff <= -6 ~ "leader_momentum",
        TRUE ~ "neutral"
      ),
      combined = paste(vel_signal, run_signal, sep = "_")
    )

  message("\nWin Rate by Combined Signal:")
  combined_summary <- combined_analysis %>%
    group_by(combined) %>%
    summarise(
      n = n(),
      win_rate = mean(fade_won) * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(win_rate))

  print(combined_summary)

  # Best and worst combinations
  if (nrow(combined_summary) > 1) {
    best_combo <- combined_summary %>% filter(n >= 5) %>% slice(1)
    worst_combo <- combined_summary %>% filter(n >= 5) %>% slice(n())

    if (nrow(best_combo) > 0 && nrow(worst_combo) > 0) {
      message(sprintf("\nBest combination: %s (%.1f%% win rate, n=%d)",
                     best_combo$combined, best_combo$win_rate, best_combo$n))
      message(sprintf("Worst combination: %s (%.1f%% win rate, n=%d)",
                     worst_combo$combined, worst_combo$win_rate, worst_combo$n))
    }
  }
}

# ------------------------------------------------------------------------------
# ANALYSIS 5: Trailer Price Interaction
# ------------------------------------------------------------------------------

message("\n", strrep("=", 70))
message("  ANALYSIS 5: TRAILER PRICE INTERACTION")
message(strrep("=", 70), "\n")

# Does velocity/run diff matter more at certain price levels?
price_vel_interaction <- fade_eligible %>%
  filter(!is.na(velocity)) %>%
  mutate(
    price_tier = case_when(
      trailer_price >= 0.45 ~ "high (45c+)",
      trailer_price >= 0.40 ~ "medium (40-45c)",
      TRUE ~ "low (35-40c)"
    ),
    vel_direction = case_when(
      velocity <= -0.015 ~ "dropping",
      velocity >= 0.015 ~ "rising",
      TRUE ~ "stable"
    )
  ) %>%
  group_by(price_tier, vel_direction) %>%
  summarise(
    n = n(),
    win_rate = mean(fade_won) * 100,
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = vel_direction, values_from = c(n, win_rate))

message("Win Rate by Price Tier x Velocity Direction:")
print(price_vel_interaction)

# ------------------------------------------------------------------------------
# Summary Recommendations
# ------------------------------------------------------------------------------

message("\n", strrep("=", 70))
message("  SUMMARY & RECOMMENDATIONS")
message(strrep("=", 70), "\n")

message("Based on this analysis:")
message("")

# Velocity recommendation
if (exists("best_vel") && nrow(best_vel) > 0 && !is.na(best_vel$dip_vs_spike) && best_vel$dip_vs_spike > 3) {
  message(sprintf("1. VELOCITY: Use %.1fc/min threshold", best_vel$threshold))
  message(sprintf("   - Dips show %.1f%% higher win rate than spikes", best_vel$dip_vs_spike))
  message("   - Contrarian logic (buy dips) appears SUPPORTED by data")
} else {
  message("1. VELOCITY: Weak or no predictive signal found")
  message("   - Consider DISABLING velocity adjustments")
}

message("")

# Run differential recommendation
if (exists("best_run") && nrow(best_run) > 0 && !is.na(best_run$trailer_vs_leader) && best_run$trailer_vs_leader > 3) {
  message(sprintf("2. RUN DIFFERENTIAL: Use %d pt threshold", best_run$threshold))
  message(sprintf("   - Trailer momentum shows %.1f%% higher win rate", best_run$trailer_vs_leader))
  message("   - Momentum signal appears SUPPORTED by data")
} else {
  message("2. RUN DIFFERENTIAL: Weak or no predictive signal found")
  message("   - Consider DISABLING run differential adjustments")
}

message("")
message("3. DATA COVERAGE:")
message(sprintf("   - %.1f%% of entries have velocity data",
               sum(!is.na(entries_df$velocity))/nrow(entries_df)*100))
message(sprintf("   - %.1f%% of entries have run diff data",
               sum(!is.na(entries_df$run_diff))/nrow(entries_df)*100))

# Save results
results <- list(
  entries = entries_df,
  velocity_results = if(exists("vel_results")) vel_results else NULL,
  run_results = if(exists("run_results")) run_results else NULL,
  best_velocity_threshold = if(exists("best_vel") && nrow(best_vel) > 0) best_vel$threshold else NA,
  best_run_threshold = if(exists("best_run") && nrow(best_run) > 0) best_run$threshold else NA
)
saveRDS(results, "ingame_threshold_analysis.rds")
message("\nResults saved to ingame_threshold_analysis.rds")
