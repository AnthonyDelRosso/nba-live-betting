# Run the segment-playoff analysis

source("season_segment_playoff_analysis.R")

cat("=== Building Dataset ===\n\n")
data <- build_segment_playoff_dataset(seasons = 2015:2024)

cat("\n\n========================================\n")
cat("REGRESSION ANALYSIS\n")
cat("========================================\n\n")

# Run regression models
regression_results <- fit_segment_models()

cat("\n\n========================================\n")
cat("CLASSIFICATION ANALYSIS\n")
cat("========================================\n\n")

# Run classification models
classification_results <- fit_round_classification()

cat("\n\n========================================\n")
cat("SIMPLIFIED TIER CLASSIFICATION\n")
cat("========================================\n\n")

tier_results <- fit_tier_classification()

cat("\n=== Analysis Complete ===\n")
