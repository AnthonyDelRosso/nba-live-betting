# Performance Module
# Displays P&L metrics and performance charts

#' Performance UI
#' @param id Module namespace id
performanceUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Summary Stats Row
    fluidRow(
      valueBoxOutput(ns("total_pnl"), width = 3),
      valueBoxOutput(ns("win_rate"), width = 3),
      valueBoxOutput(ns("total_trades"), width = 3),
      valueBoxOutput(ns("open_positions"), width = 3)
    ),

    # Charts Row
    fluidRow(
      column(6,
        div(class = "box box-primary",
          div(class = "box-header with-border",
            h3(class = "box-title", icon("chart-line"), " Cumulative P&L")
          ),
          div(class = "box-body",
            plotlyOutput(ns("pnl_chart"), height = "300px")
          )
        )
      ),
      column(6,
        div(class = "box box-primary",
          div(class = "box-header with-border",
            h3(class = "box-title", icon("chart-pie"), " Strategy Breakdown")
          ),
          div(class = "box-body",
            plotlyOutput(ns("strategy_chart"), height = "300px")
          )
        )
      )
    ),

    # Strategy Performance Table
    fluidRow(
      column(12,
        div(class = "box box-primary",
          div(class = "box-header with-border",
            h3(class = "box-title", icon("table"), " Strategy Performance")
          ),
          div(class = "box-body",
            DTOutput(ns("strategy_table"))
          )
        )
      )
    )
  )
}

#' Performance Server
#' @param id Module namespace id
#' @param state Reactive TradingState object
#' @param state_version Reactive counter that increments to force UI refresh
performanceServer <- function(id, state, state_version = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Shared reactive that forces refresh when state_version changes
    # R6 objects don't trigger reactiveVal invalidation (identical() is TRUE
    # for the same reference), so we depend on state_version as a refresh trigger.
    current_state <- reactive({
      if (!is.null(state_version)) state_version()
      req(state())
      state()
    })

    # Total P&L value box
    output$total_pnl <- renderValueBox({
      s <- current_state()
      pnl_summary <- s$get_pnl_summary()
      total_pnl <- pnl_summary$total_pnl

      valueBox(
        value = sprintf("$%+.2f", total_pnl),
        subtitle = "Total P&L",
        icon = icon("dollar-sign"),
        color = if (total_pnl >= 0) "green" else "red"
      )
    })

    # Win rate value box
    output$win_rate <- renderValueBox({
      s <- current_state()
      pnl_summary <- s$get_pnl_summary()

      win_rate <- if (is.na(pnl_summary$win_rate)) {
        "N/A"
      } else {
        sprintf("%.1f%%", pnl_summary$win_rate * 100)
      }

      valueBox(
        value = win_rate,
        subtitle = sprintf("Win Rate (%d W / %d L)",
                          pnl_summary$win_count, pnl_summary$loss_count),
        icon = icon("trophy"),
        color = "blue"
      )
    })

    # Total trades value box
    output$total_trades <- renderValueBox({
      s <- current_state()

      valueBox(
        value = nrow(s$positions),
        subtitle = "Total Trades",
        icon = icon("exchange-alt"),
        color = "purple"
      )
    })

    # Open positions value box
    output$open_positions <- renderValueBox({
      s <- current_state()

      valueBox(
        value = nrow(s$get_open_positions()),
        subtitle = "Open Positions",
        icon = icon("hourglass-half"),
        color = "yellow"
      )
    })

    # Cumulative P&L chart
    output$pnl_chart <- renderPlotly({
      s <- current_state()
      positions <- s$positions

      if (nrow(positions) == 0 || all(is.na(positions$pnl))) {
        return(
          plot_ly() %>%
            add_annotations(
              x = 0.5, y = 0.5,
              text = "No settled trades yet",
              showarrow = FALSE,
              font = list(size = 16, color = "gray")
            ) %>%
            layout(
              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
            )
        )
      }

      # Calculate cumulative P&L
      pnl_data <- positions %>%
        filter(!is.na(pnl)) %>%
        arrange(exit_time) %>%
        mutate(
          trade_num = row_number(),
          cumulative_pnl = cumsum(pnl)
        )

      plot_ly(pnl_data, x = ~trade_num, y = ~cumulative_pnl, type = 'scatter',
              mode = 'lines+markers',
              line = list(color = '#007bff', width = 2),
              marker = list(
                color = ifelse(pnl_data$pnl >= 0, '#28a745', '#dc3545'),
                size = 8
              ),
              hovertemplate = paste(
                "Trade: %{x}<br>",
                "P&L: $%{customdata:.2f}<br>",
                "Cumulative: $%{y:.2f}",
                "<extra></extra>"
              ),
              customdata = ~pnl) %>%
        layout(
          xaxis = list(title = "Trade #"),
          yaxis = list(title = "Cumulative P&L ($)"),
          hovermode = "closest",
          shapes = list(
            list(
              type = "line",
              x0 = 0, x1 = max(pnl_data$trade_num),
              y0 = 0, y1 = 0,
              line = list(color = "gray", dash = "dash", width = 1)
            )
          )
        )
    })

    # Strategy breakdown pie chart
    output$strategy_chart <- renderPlotly({
      s <- current_state()

      strategy_data <- s$get_strategy_performance()

      if (nrow(strategy_data) == 0) {
        return(
          plot_ly() %>%
            add_annotations(
              x = 0.5, y = 0.5,
              text = "No settled trades yet",
              showarrow = FALSE,
              font = list(size = 16, color = "gray")
            ) %>%
            layout(
              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
            )
        )
      }

      colors <- c("with_leader" = "#007bff", "fade" = "#ffc107")

      plot_ly(strategy_data, labels = ~strategy, values = ~total_trades,
              type = 'pie',
              marker = list(colors = colors[strategy_data$strategy]),
              textinfo = 'label+percent',
              hovertemplate = paste(
                "%{label}<br>",
                "Trades: %{value}<br>",
                "P&L: $%{customdata:.2f}",
                "<extra></extra>"
              ),
              customdata = ~total_pnl) %>%
        layout(showlegend = TRUE)
    })

    # Strategy performance table
    output$strategy_table <- renderDT({
      s <- current_state()

      strategy_data <- s$get_strategy_performance()

      if (nrow(strategy_data) == 0) {
        return(datatable(
          tibble(Message = "No settled trades yet"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      display_data <- strategy_data %>%
        mutate(
          strategy_label = case_when(
            strategy == "with_leader" ~ "With Leader",
            strategy == "fade" ~ "Selective Fade",
            TRUE ~ strategy
          ),
          win_rate_fmt = sprintf("%.1f%%", win_rate * 100),
          total_pnl_fmt = sprintf("$%+.2f", total_pnl),
          avg_pnl_fmt = sprintf("$%+.2f", avg_pnl)
        ) %>%
        select(
          Strategy = strategy_label,
          Trades = total_trades,
          Wins = wins,
          Losses = losses,
          `Win Rate` = win_rate_fmt,
          `Total P&L` = total_pnl_fmt,
          `Avg P&L` = avg_pnl_fmt
        )

      datatable(
        display_data,
        rownames = FALSE,
        options = list(
          dom = 't',
          columnDefs = list(
            list(className = 'dt-center', targets = 1:6)
          )
        )
      )
    })
  })
}
