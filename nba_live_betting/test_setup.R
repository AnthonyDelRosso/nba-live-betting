# Test Script for NBA Live Betting Platform
# Run this to verify all components are working

cat("=== NBA Live Betting Platform Setup Test ===\n\n")

# Test 1: Check required packages
cat("1. Checking required packages...\n")
required_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets",
  "tidyverse", "lubridate",
  "httr2", "jsonlite",
  "R6", "DT", "plotly", "later", "yaml", "hoopR"
)

missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  cat("  MISSING PACKAGES: ", paste(missing, collapse = ", "), "\n")
  cat("  Install with: install.packages(c('", paste(missing, collapse = "', '"), "'))\n")
} else {
  cat("  All packages installed\n")
}

# Test 2: Check model files
cat("\n2. Checking model files in parent directory...\n")
parent_dir <- normalizePath(file.path(getwd(), ".."))

model_files <- c(
  "win_probability_model.rds",
  "blown_lead_model_v2.rds",
  "kalshi_predictor.R",
  "win_probability_model.R",
  "nba_largest_lead.R"
)

for (f in model_files) {
  path <- file.path(parent_dir, f)
  status <- if (file.exists(path)) "OK" else "MISSING"
  cat(sprintf("  %s: %s\n", f, status))
}

# Test 3: Check ESPN API
cat("\n3. Testing ESPN API...\n")
tryCatch({
  source("R/espn_api.R")
  scoreboard <- get_nba_scoreboard(Sys.Date())
  cat(sprintf("  Found %d games for today\n", length(scoreboard$games)))

  if (length(scoreboard$games) > 0) {
    game <- scoreboard$games[[1]]
    cat(sprintf("  Example: %s @ %s (%s)\n",
                game$away_team, game$home_team,
                format_game_status(game)))
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", e$message))
})

# Test 4: Check predictor initialization
cat("\n4. Testing predictor initialization...\n")
tryCatch({
  source(file.path(parent_dir, "kalshi_predictor.R"))

  predictor <- KalshiPredictor$new(
    wp_model_path = file.path(parent_dir, "win_probability_model.rds"),
    blown_lead_model_path = file.path(parent_dir, "blown_lead_model_v2.rds")
  )
  cat("  Predictor initialized successfully\n")

  # Test prediction
  game_state <- create_game_state(
    home_score = 85,
    away_score = 72,
    seconds_remaining = 600,  # 10 minutes left
    period = 4
  )

  signal <- predictor$get_signal("test_game", game_state, market_price = 0.90)
  cat(sprintf("  Test signal (home +13, Q4 10:00): WL=%s, Fade=%s\n",
              signal$with_leader$signal, signal$fade$signal))
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", e$message))
})

# Test 5: Check trading state
cat("\n5. Testing trading state...\n")
tryCatch({
  source("R/logging.R")
  source("R/trading_state.R")

  logger <- create_logger(log_dir = "logs", level = "INFO")
  state <- TradingState$new(mode = "paper", paper_balance = 1000, logger = logger)

  cat(sprintf("  Initial balance: $%.2f\n", state$cash_balance))
  cat("  Trading state initialized successfully\n")
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", e$message))
})

cat("\n=== Setup test complete ===\n")
cat("\nTo run the app:\n")
cat("  1. cd nba_live_betting\n")
cat("  2. R -e \"shiny::runApp()\"\n")
cat("  Or in RStudio: Open app.R and click 'Run App'\n")
