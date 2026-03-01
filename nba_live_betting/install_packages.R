# Package Installation Script for NBA Live Betting Platform
# Run this script to install all required packages

cat("Installing required packages for NBA Live Betting Platform...\n\n")

# Function to install package with fallback to source
install_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("Installing %s...\n", pkg))
    tryCatch({
      # Try binary first
      install.packages(pkg, dependencies = TRUE)
    }, error = function(e) {
      cat(sprintf("Binary install failed, trying from source...\n"))
      install.packages(pkg, type = "source", dependencies = TRUE)
    })
  } else {
    cat(sprintf("%s already installed\n", pkg))
  }
}

# Core packages (required)
core_packages <- c(
  "shiny",
  "tidyverse",
  "lubridate",
  "httr2",
  "jsonlite",
  "R6",
  "DT",
  "later",
  "yaml"
)

# Optional packages (enhance functionality)
optional_packages <- c(
  "shinydashboard",
  "shinyWidgets",
  "plotly",
  "hoopR"
)

cat("=== Installing Core Packages ===\n")
for (pkg in core_packages) {
  install_pkg(pkg)
}

cat("\n=== Installing Optional Packages ===\n")
cat("(These enhance the UI but app will work without them)\n\n")
for (pkg in optional_packages) {
  tryCatch({
    install_pkg(pkg)
  }, error = function(e) {
    cat(sprintf("WARNING: Could not install %s: %s\n", pkg, e$message))
    cat("The app will still work with reduced functionality.\n")
  })
}

cat("\n=== Checking R Version ===\n")
cat(sprintf("R version: %s\n", R.version.string))

if (as.numeric(R.version$major) < 4) {
  cat("\nWARNING: R version 4.0+ is recommended for best compatibility.\n")
  cat("Some packages may not be available for older R versions.\n")
  cat("Consider upgrading R from: https://cran.r-project.org/\n")
}

cat("\n=== Installation Complete ===\n")
cat("\nTo run the app:\n")
cat("  1. Open RStudio or R console\n")
cat("  2. Set working directory: setwd('path/to/nba_live_betting')\n")
cat("  3. Run: shiny::runApp()\n")
