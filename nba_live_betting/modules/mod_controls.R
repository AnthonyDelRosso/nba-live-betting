# Controls Module
# Start/stop trading, mode selection, configuration

#' Controls UI
#' @param id Module namespace id
controlsUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Trading Controls Box
    div(class = "box box-primary",
      div(class = "box-header with-border",
        h3(class = "box-title", icon("cogs"), " Trading Controls")
      ),
      div(class = "box-body",
        # Mode selector
        radioGroupButtons(
          inputId = ns("mode"),
          label = "Trading Mode",
          choices = c("Paper" = "paper", "Live" = "live"),
          selected = "paper",
          status = "primary",
          justified = TRUE
        ),

        hr(),

        # Start/Stop buttons
        fluidRow(
          column(6,
            actionButton(
              ns("start"),
              "Start Trading",
              icon = icon("play"),
              class = "btn-success btn-block",
              style = "margin-bottom: 10px;"
            )
          ),
          column(6,
            actionButton(
              ns("stop"),
              "Stop Trading",
              icon = icon("stop"),
              class = "btn-danger btn-block",
              style = "margin-bottom: 10px;"
            )
          )
        ),

        # Status indicator
        uiOutput(ns("status_indicator")),

        hr(),

        # Configuration
        numericInput(
          ns("paper_balance"),
          "Paper Balance ($)",
          value = 1000,
          min = 100,
          max = 100000,
          step = 100
        ),

        numericInput(
          ns("poll_interval"),
          "Poll Interval (seconds)",
          value = 30,
          min = 10,
          max = 120,
          step = 5
        ),

        hr(),

        # Reset button
        actionButton(
          ns("reset"),
          "Reset Session",
          icon = icon("redo"),
          class = "btn-warning btn-block"
        )
      )
    ),

    # Account Summary Box
    div(class = "box box-info",
      div(class = "box-header with-border",
        h3(class = "box-title", icon("wallet"), " Account")
      ),
      div(class = "box-body",
        uiOutput(ns("kalshi_status")),
        uiOutput(ns("account_summary"))
      )
    )
  )
}

#' Controls Server
#' @param id Module namespace id
#' @param state Reactive TradingState object
#' @param trader Reactive AutomatedTrader object
#' @param state_version Reactive counter for forcing UI refresh
controlsServer <- function(id, state, trader, logger, paper_engine = NULL, state_version = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Track running state locally
    is_running <- reactiveVal(FALSE)

    # Kalshi data feed status indicator
    output$kalshi_status <- renderUI({
      use_real <- !is.null(paper_engine) && isTRUE(paper_engine$use_real_prices)
      if (use_real) {
        div(
          style = "margin-bottom: 10px; padding: 5px 8px; background: #d4edda; border-radius: 4px; font-size: 12px;",
          icon("signal", style = "color: #28a745;"),
          span(style = "color: #155724; font-weight: bold;", " Kalshi: Connected"),
          div(class = "text-muted small", "Using real market prices")
        )
      } else {
        div(
          style = "margin-bottom: 10px; padding: 5px 8px; background: #fff3cd; border-radius: 4px; font-size: 12px;",
          icon("wifi", style = "color: #856404;"),
          span(style = "color: #856404; font-weight: bold;", " Kalshi: Simulated"),
          div(class = "text-muted small", "Using model-based prices")
        )
      }
    })

    # Start trading
    observeEvent(input$start, {
      req(trader())

      if (is_running()) {
        showNotification("Trading already running", type = "warning")
        return()
      }

      # Check mode
      if (input$mode == "live") {
        showModal(modalDialog(
          title = "Live Trading Warning",
          tagList(
            p(class = "text-danger", icon("exclamation-triangle"),
              strong(" WARNING: You are about to start LIVE trading with real money!")),
            p("Make sure you have:"),
            tags$ul(
              tags$li("Configured your Kalshi API credentials"),
              tags$li("Tested thoroughly in paper mode"),
              tags$li("Set appropriate position sizes")
            ),
            p("Are you sure you want to proceed?")
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_live"), "Start Live Trading",
                        class = "btn-danger")
          ),
          size = "m"
        ))
        return()
      }

      # Start paper trading
      start_trading()
    })

    # Confirm live trading
    observeEvent(input$confirm_live, {
      removeModal()
      start_trading()
    })

    # Start trading function
    start_trading <- function() {
      tryCatch({
        trader()$start()
        is_running(TRUE)
        showNotification(
          sprintf("Trading started in %s mode", input$mode),
          type = "message"
        )
        if (!is.null(logger)) {
          logger$info(sprintf("Trading started by user in %s mode", input$mode))
        }
      }, error = function(e) {
        showNotification(sprintf("Failed to start: %s", e$message), type = "error")
      })
    }

    # Stop trading
    observeEvent(input$stop, {
      req(trader())

      if (!is_running()) {
        showNotification("Trading not running", type = "warning")
        return()
      }

      tryCatch({
        trader()$stop()
        is_running(FALSE)
        showNotification("Trading stopped", type = "message")
        if (!is.null(logger)) {
          logger$info("Trading stopped by user")
        }
      }, error = function(e) {
        showNotification(sprintf("Failed to stop: %s", e$message), type = "error")
      })
    })

    # Reset session
    observeEvent(input$reset, {
      showModal(modalDialog(
        title = "Reset Session",
        tagList(
          p("This will:"),
          tags$ul(
            tags$li("Stop any running trades"),
            tags$li("Clear all positions"),
            tags$li("Reset balance to starting amount"),
            tags$li("Clear trade log")
          ),
          p(class = "text-warning", "This action cannot be undone.")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_reset"), "Reset", class = "btn-warning")
        ),
        size = "m"
      ))
    })

    # Confirm reset
    observeEvent(input$confirm_reset, {
      removeModal()

      tryCatch({
        # Stop if running
        if (is_running() && !is.null(trader())) {
          tryCatch({
            trader()$stop()
          }, error = function(e) {
            # Ignore stop errors
          })
          is_running(FALSE)
        }

        # Get state object
        s <- state()
        if (is.null(s)) {
          showNotification("State not initialized", type = "error")
          return()
        }

        # Get paper balance with fallback
        new_balance <- input$paper_balance
        if (is.null(new_balance) || !is.numeric(new_balance) || is.na(new_balance)) {
          new_balance <- 1000  # Default
        }

        # Reset state
        s$reset()

        # Update paper balance
        s$paper_balance <- new_balance
        s$cash_balance <- new_balance

        # Delete saved state file to prevent reload of old data
        tryCatch({
          state_path <- if (exists("DATA_DIR", envir = globalenv())) {
            file.path(get("DATA_DIR", envir = globalenv()), "trading_state.rds")
          } else {
            "data/trading_state.rds"
          }
          if (file.exists(state_path)) {
            file.remove(state_path)
          }
        }, error = function(e) {
          # Ignore file deletion errors
        })

        showNotification("Session reset successfully", type = "message")
        if (!is.null(logger)) {
          logger$info("Session reset by user", balance = new_balance)
        }
      }, error = function(e) {
        showNotification(sprintf("Failed to reset: %s", e$message), type = "error")
      })
    })

    # Status indicator
    output$status_indicator <- renderUI({
      if (!is.null(state_version)) state_version()
      status_class <- if (is_running()) "text-success" else "text-muted"
      status_icon <- if (is_running()) "circle" else "circle"
      status_text <- if (is_running()) "Trading Active" else "Stopped"

      div(
        class = "text-center",
        style = "margin: 15px 0;",
        icon(status_icon, class = paste("fa-2x", status_class)),
        div(class = status_class, style = "font-weight: bold;", status_text),
        if (is_running() && !is.null(trader())) {
          div(
            class = "text-muted small",
            sprintf("Last poll: %s",
                   format(trader()$last_poll %||% Sys.time(), "%H:%M:%S"))
          )
        }
      )
    })

    # Account summary
    output$account_summary <- renderUI({
      # Force periodic refresh for R6 state object
      if (!is.null(state_version)) state_version()
      req(state())
      s <- state()

      total_balance <- s$get_total_balance()
      pnl <- total_balance - s$paper_balance
      pnl_pct <- (pnl / s$paper_balance) * 100

      pnl_class <- if (pnl >= 0) "text-success" else "text-danger"

      tagList(
        div(
          class = "row",
          div(class = "col-xs-6",
            div(class = "text-muted small", "Cash"),
            div(class = "h4", format_currency(s$cash_balance))
          ),
          div(class = "col-xs-6",
            div(class = "text-muted small", "Positions"),
            div(class = "h4", format_currency(s$position_value))
          )
        ),
        hr(style = "margin: 10px 0;"),
        div(
          class = "row",
          div(class = "col-xs-6",
            div(class = "text-muted small", "Total"),
            div(class = "h4", format_currency(total_balance))
          ),
          div(class = "col-xs-6",
            div(class = "text-muted small", "P&L"),
            div(class = paste("h4", pnl_class),
                sprintf("%+.2f (%.1f%%)", pnl, pnl_pct))
          )
        ),
        hr(style = "margin: 10px 0;"),
        div(
          class = "row",
          div(class = "col-xs-6",
            div(class = "text-muted small", "Open"),
            div(class = "h5", nrow(s$get_open_positions()))
          ),
          div(class = "col-xs-6",
            div(class = "text-muted small", "Total Trades"),
            div(class = "h5", nrow(s$positions))
          )
        )
      )
    })

    # Return running state for other modules
    reactive(is_running())
  })
}
