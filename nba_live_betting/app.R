# NBA Live Betting Platform
# Main Shiny Application Entry Point

# Source global configuration and dependencies
source("global.R")

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

ui <- dashboardPage(
  skin = "blue",

  # Header
  dashboardHeader(
    title = span(icon("basketball"), " NBA Live Betting"),
    titleWidth = 250
  ),

  # Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Live Games", tabName = "games", icon = icon("basketball")),
      menuItem("Positions", tabName = "positions", icon = icon("chart-line")),
      menuItem("Trade Log", tabName = "trades", icon = icon("history")),
      menuItem("Performance", tabName = "performance", icon = icon("chart-bar"))
    ),
    hr(),
    # Controls sidebar
    controlsUI("controls")
  ),

  # Body
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper {
          background-color: #f4f6f9;
        }
        .box {
          border-radius: 5px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12);
        }
        .badge {
          font-size: 11px;
          padding: 3px 8px;
        }
        .badge-primary { background-color: #007bff; }
        .badge-success { background-color: #28a745; }
        .badge-warning { background-color: #ffc107; color: #212529; }
        .badge-danger { background-color: #dc3545; }
        .badge-info { background-color: #17a2b8; }
        .badge-secondary { background-color: #6c757d; }
        .card {
          border-radius: 8px;
          overflow: hidden;
        }
        .progress {
          border-radius: 10px;
          overflow: hidden;
        }
        .progress-bar {
          font-size: 11px;
          line-height: 20px;
        }
        .text-success { color: #28a745 !important; }
        .text-danger { color: #dc3545 !important; }
        .text-muted { color: #6c757d !important; }
        .small-box .icon {
          font-size: 70px;
        }
        .dataTables_wrapper {
          font-size: 13px;
        }
      "))
    ),

    # Tab content
    tabItems(
      # Dashboard tab - overview
      tabItem(
        tabName = "dashboard",
        h2("Dashboard"),
        performanceUI("performance"),
        liveGamesUI("games_dashboard")
      ),

      # Live Games tab
      tabItem(
        tabName = "games",
        h2("Live Games"),
        liveGamesUI("games_full")
      ),

      # Positions tab
      tabItem(
        tabName = "positions",
        h2("Positions"),
        positionsUI("positions")
      ),

      # Trade Log tab
      tabItem(
        tabName = "trades",
        h2("Trade History"),
        tradeLogUI("trade_log")
      ),

      # Performance tab
      tabItem(
        tabName = "performance",
        h2("Performance Analytics"),
        performanceUI("performance_full")
      )
    )
  )
)

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------

server <- function(input, output, session) {
  message("[DEBUG] 1. Server function started")

  # Initialize global objects
  message("[DEBUG] 2. Creating logger...")
  logger <- create_logger(log_dir = LOG_DIR, level = CONFIG$log_level)
  logger$info("Application started")

  message("[DEBUG] 3. Creating trading_state reactiveVal...")
  # Initialize trading state
  trading_state <- reactiveVal()

  message("[DEBUG] 4. Creating TradingState object...")
  state <- TradingState$new(
    mode = "paper",
    paper_balance = CONFIG$paper_balance,
    logger = logger
  )

  message("[DEBUG] 5. Loading saved state...")
  # Try to load saved state
  state_file <- file.path(DATA_DIR, "trading_state.rds")
  if (file.exists(state_file)) {
    tryCatch({
      state$load_state(state_file)
      logger$info("Loaded saved state")
    }, error = function(e) {
      logger$warn(sprintf("Failed to load saved state: %s", e$message))
    })
  }

  message("[DEBUG] 5b. Creating Kalshi price client...")
  # Create Kalshi client for real market price data (paper execution stays simulated)
  price_client <- tryCatch({
    client <- create_kalshi_client(
      config_path = file.path(APP_DIR, "config/config.yml"),
      demo_mode = FALSE,  # Production API — demo has no real NBA markets
      logger = logger
    )
    if (!is.null(client$email) && !is.null(client$password)) {
      auth_ok <- client$authenticate()
      if (auth_ok) {
        message("[KALSHI] Kalshi client authenticated for real price data")
        logger$info("Kalshi client authenticated for real price data")
        client
      } else {
        message("[KALSHI] WARNING: Authentication failed — falling back to simulated prices")
        logger$warn("Kalshi authentication failed, using simulated prices")
        NULL
      }
    } else {
      message("[KALSHI] No credentials found — using simulated prices")
      message("[KALSHI] Set KALSHI_EMAIL / KALSHI_PASSWORD env vars or fill config/config.yml")
      NULL
    }
  }, error = function(e) {
    message(sprintf("[KALSHI] ERROR connecting: %s — using simulated prices", e$message))
    logger$warn(sprintf("Kalshi client error: %s", e$message))
    NULL
  })

  # Log current balance status on startup
  message("========================================")
  message("  NBA LIVE BETTING - SESSION STATUS")
  message("========================================")
  message(sprintf("  Mode:           %s", state$mode))
  message(sprintf("  Cash Balance:   $%.2f", state$cash_balance))
  message(sprintf("  Paper Balance:  $%.2f", state$paper_balance))
  message(sprintf("  Open Positions: %d", nrow(state$get_open_positions())))
  message(sprintf("  Total Trades:   %d", nrow(state$positions)))
  message("----------------------------------------")
  message("  STRATEGY CONFIGURATION")
  message("----------------------------------------")
  message("  Strategies:     With Leader + Fade V2")
  message(sprintf("  Spread:         %s", if (!is.null(price_client)) "REAL Kalshi bid-ask" else "2c bid-ask (simulated)"))
  message("  Bet Size:       2% of bankroll")
  message(sprintf("  In-Game Adj:    %s", ifelse(CONFIG$enable_ingame_adjustments %||% FALSE, "ENABLED", "DISABLED")))
  if (state$cash_balance < 1) {
    message("")
    message("  *** WARNING: Balance is very low! ***")
    message("  *** Use Reset Session to restore ***")
  }
  message("========================================")

  message("[DEBUG] 6. Setting trading_state...")
  trading_state(state)

  message("[DEBUG] 7. Creating KalshiPredictor...")
  # Initialize predictor with V2 fade strategy (market-based)
  predictor <- tryCatch({
    KalshiPredictor$new(
      wp_model_path = MODEL_PATHS$wp_model,
      blown_lead_model_path = MODEL_PATHS$blown_lead_model,
      fade_mode = "market"  # V2 strategy: trailer >= 35c threshold
    )
  }, error = function(e) {
    logger$error(sprintf("Failed to load models: %s", e$message))
    message(sprintf("[STARTUP] ERROR: Failed to load prediction models: %s", e$message))
    NULL
  })

  message("[DEBUG] 8. Creating PaperTradingEngine...")
  # Initialize paper trading engine (with real prices if Kalshi client available)
  paper_engine <- PaperTradingEngine$new(
    state = state,
    logger = logger,
    kalshi_client = price_client
  )

  message("[DEBUG] 9. Creating automated_trader reactiveVal...")
  # Initialize automated trader
  automated_trader <- reactiveVal()

  message("[DEBUG] 10. Creating AutomatedTrader...")
  trader <- AutomatedTrader$new(
    state = state,
    predictor = predictor,
    paper_engine = paper_engine,
    logger = logger,
    poll_interval = CONFIG$poll_interval
  )

  message("[DEBUG] 11. Loading team ratings...")
  # Load team ratings for predictions
  team_ratings_data <- reactiveVal(NULL)

  tryCatch({
    ratings <- get_team_ratings(season = 2026)
    if (nrow(ratings) > 0) {
      trader$update_team_ratings(ratings)
      team_ratings_data(ratings)  # Store for UI display
      message(sprintf("[DEBUG] 11b. Team ratings loaded (%d teams)", nrow(ratings)))
      logger$info("Loaded team ratings", n_teams = nrow(ratings))
    } else {
      message("[DEBUG] 11b. WARNING: No team ratings loaded")
    }
  }, error = function(e) {
    message(sprintf("[DEBUG] 11b. ERROR loading team ratings: %s", e$message))
    logger$warn(sprintf("Failed to load team ratings: %s", e$message))
  })

  message("[DEBUG] 12. Setting automated_trader...")
  automated_trader(trader)

  message("[DEBUG] 13. Setting up auto-refresh timer...")
  # Auto-refresh state every 5 seconds for UI updates
  auto_refresh_timer <- reactiveTimer(5000)

  shiny::observe({
    auto_refresh_timer()
    trading_state(state)
  })

  message("[DEBUG] 14. Setting up session end handler...")
  # Save state on session end
  session$onSessionEnded(function() {
    tryCatch({
      if (trader$is_running) {
        trader$stop()
      }
      state$save_state(state_file)
      logger$info("Session ended, state saved")
    }, error = function(e) {
      logger$error(sprintf("Failed to save state on exit: %s", e$message))
    })
  })

  message("[DEBUG] 15. Calling module servers...")
  # Module servers
  message("[DEBUG] 15a. controlsServer...")
  controlsServer("controls", trading_state, automated_trader, logger, paper_engine)
  message("[DEBUG] 15b. liveGamesServer (dashboard)...")
  liveGamesServer("games_dashboard", trading_state, predictor, team_ratings_data)
  message("[DEBUG] 15c. liveGamesServer (full)...")
  liveGamesServer("games_full", trading_state, predictor, team_ratings_data)
  message("[DEBUG] 15d. positionsServer...")
  positionsServer("positions", trading_state)
  message("[DEBUG] 15e. tradeLogServer...")
  tradeLogServer("trade_log", trading_state)
  message("[DEBUG] 15f. performanceServer...")
  performanceServer("performance", trading_state)
  message("[DEBUG] 15g. performanceServer (full)...")
  performanceServer("performance_full", trading_state)
  message("[DEBUG] 16. Server initialization complete!")
}

# ------------------------------------------------------------------------------
# Run App
# ------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
