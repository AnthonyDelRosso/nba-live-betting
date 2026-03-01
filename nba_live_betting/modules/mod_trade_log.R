# Trade Log Module
# Displays trade history with filtering

#' Trade Log UI
#' @param id Module namespace id
tradeLogUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(12,
        div(class = "box box-primary",
          div(class = "box-header with-border",
            h3(class = "box-title", icon("history"), " Trade Log"),
            div(class = "box-tools pull-right",
              downloadButton(ns("download"), "Export CSV", class = "btn-sm")
            )
          ),
          div(class = "box-body",
            DTOutput(ns("trade_log_table"))
          )
        )
      )
    )
  )
}

#' Trade Log Server
#' @param id Module namespace id
#' @param state Reactive TradingState object
#' @param state_version Reactive counter that increments to force UI refresh
tradeLogServer <- function(id, state, state_version = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render trade log table
    output$trade_log_table <- renderDT({
      # Force periodic refresh — R6 objects don't trigger reactiveVal invalidation
      if (!is.null(state_version)) state_version()
      req(state())
      trade_log <- state()$trade_log

      if (nrow(trade_log) == 0) {
        return(datatable(
          tibble(Message = "No trades recorded"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      # Format for display
      display_data <- trade_log %>%
        mutate(
          # Action badge
          action_badge = case_when(
            action == "OPEN" ~ '<span class="badge badge-primary">OPEN</span>',
            action == "CLOSE" ~ '<span class="badge badge-warning">CLOSE</span>',
            action == "SETTLE" ~ '<span class="badge badge-info">SETTLE</span>',
            TRUE ~ action
          ),
          # Strategy badge
          strategy_badge = case_when(
            strategy == "with_leader" ~ '<span class="badge badge-primary">WL</span>',
            strategy == "fade" ~ '<span class="badge badge-warning">FADE</span>',
            TRUE ~ strategy
          ),
          # Signal badge
          signal_badge = case_when(
            signal == "BUY_YES" ~ '<span class="badge badge-success">YES</span>',
            signal == "BUY_NO" ~ '<span class="badge badge-danger">NO</span>',
            TRUE ~ signal
          ),
          # Format values
          timestamp_fmt = format(timestamp, "%Y-%m-%d %H:%M:%S"),
          price_fmt = sprintf("%.3f", price),
          amount_fmt = sprintf("$%+.2f", amount),
          balance_fmt = sprintf("$%.2f", balance_after)
        ) %>%
        select(
          Time = timestamp_fmt,
          Action = action_badge,
          Game = game_id,
          Strategy = strategy_badge,
          Signal = signal_badge,
          Price = price_fmt,
          Contracts = contracts,
          Amount = amount_fmt,
          Balance = balance_fmt,
          Notes = notes
        ) %>%
        arrange(desc(Time))

      datatable(
        display_data,
        escape = FALSE,
        rownames = FALSE,
        options = list(
          pageLength = 15,
          order = list(list(0, "desc")),
          dom = 'frtip',
          columnDefs = list(
            list(className = 'dt-center', targets = c(1, 3, 4)),
            list(width = '200px', targets = 9)
          )
        )
      ) %>%
        formatStyle(
          'Amount',
          color = styleInterval(0, c('red', 'green'))
        )
    })

    # Download handler
    output$download <- downloadHandler(
      filename = function() {
        sprintf("trade_log_%s.csv", format(Sys.Date(), "%Y%m%d"))
      },
      content = function(file) {
        req(state())
        write_csv(state()$trade_log, file)
      }
    )
  })
}
