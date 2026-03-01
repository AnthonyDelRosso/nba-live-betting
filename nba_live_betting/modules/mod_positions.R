# Positions Module
# Displays current and historical positions

#' Positions UI
#' @param id Module namespace id
positionsUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(12,
        div(class = "box box-primary",
          div(class = "box-header with-border",
            h3(class = "box-title", icon("chart-line"), " Positions"),
            div(class = "box-tools pull-right",
              radioGroupButtons(
                inputId = ns("filter"),
                label = NULL,
                choices = c("Open" = "open", "Settled" = "settled", "All" = "all"),
                selected = "open",
                size = "sm"
              )
            )
          ),
          div(class = "box-body",
            DTOutput(ns("positions_table"))
          )
        )
      )
    )
  )
}

#' Positions Server
#' @param id Module namespace id
#' @param state Reactive TradingState object
#' @param state_version Reactive counter that increments to force UI refresh
positionsServer <- function(id, state, state_version = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Filter positions based on selection
    filtered_positions <- reactive({
      # Force periodic refresh — R6 objects don't trigger reactiveVal invalidation
      if (!is.null(state_version)) state_version()
      req(state())
      s <- state()

      if (is.null(s) || is.null(s$positions)) {
        message("[POSITIONS UI] State or positions is NULL")
        return(tibble())
      }

      positions <- s$positions

      message(sprintf("[POSITIONS UI] Total positions in state: %d", nrow(positions)))

      if (nrow(positions) == 0) {
        return(positions)
      }

      # Log position statuses
      status_counts <- table(positions$status)
      message(sprintf("[POSITIONS UI] Status breakdown: %s",
                     paste(names(status_counts), status_counts, sep="=", collapse=", ")))

      result <- switch(input$filter,
        "open" = positions %>% filter(status == "open"),
        "settled" = positions %>% filter(status %in% c("settled_win", "settled_loss")),
        "all" = positions
      )

      message(sprintf("[POSITIONS UI] After filter '%s': %d positions", input$filter, nrow(result)))

      result
    })

    # Render positions table
    output$positions_table <- renderDT({
      positions <- filtered_positions()

      if (nrow(positions) == 0) {
        return(datatable(
          tibble(Message = "No positions to display"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      # Format for display
      display_data <- positions %>%
        mutate(
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
          # Status badge
          status_badge = case_when(
            status == "open" ~ '<span class="badge badge-info">Open</span>',
            status == "settled_win" ~ '<span class="badge badge-success">Win</span>',
            status == "settled_loss" ~ '<span class="badge badge-danger">Loss</span>',
            TRUE ~ status
          ),
          # Format values
          entry_time_fmt = format(entry_time, "%H:%M:%S"),
          entry_price_fmt = sprintf("%.2f", entry_price),
          cost_fmt = sprintf("$%.2f", cost_basis),
          pnl_fmt = ifelse(is.na(pnl), "-", sprintf("$%+.2f", pnl)),
          pnl_class = case_when(
            is.na(pnl) ~ "",
            pnl > 0 ~ "text-success",
            pnl < 0 ~ "text-danger",
            TRUE ~ ""
          ),
          # Edge display
          edge_fmt = ifelse(edge_at_entry > 0, sprintf("%.1f%%", edge_at_entry * 100), "-")
        ) %>%
        select(
          Game = game_id,
          Strategy = strategy_badge,
          Signal = signal_badge,
          Time = entry_time_fmt,
          Price = entry_price_fmt,
          Contracts = contracts,
          Cost = cost_fmt,
          Lead = lead_at_entry,
          Period = period_at_entry,
          Edge = edge_fmt,
          Status = status_badge,
          `P&L` = pnl_fmt
        )

      datatable(
        display_data,
        escape = FALSE,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          order = list(list(3, "desc")),  # Sort by time descending
          dom = 'frtip',
          columnDefs = list(
            list(className = 'dt-center', targets = c(1, 2, 10))
          )
        )
      ) %>%
        formatStyle(
          'P&L',
          color = styleInterval(0, c('red', 'green'))
        )
    })
  })
}
