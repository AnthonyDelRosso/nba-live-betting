# Pregame Big-Favorite Module
# Dashboard view of the frozen pregame spread rule (R/pregame_fav_strategy.R):
# today's slate with each game's status, plus the forward-test scorecard.

pregameFavUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(12,
      div(class = "box box-success",
        div(class = "box-header with-border",
          h3(class = "box-title", icon("bullseye"), " Pregame Big Favorite (frozen rule, paper)")
        ),
        div(class = "box-body",
          uiOutput(ns("status_banner")),
          uiOutput(ns("scorecard")),
          DTOutput(ns("slate_table")),
          tags$p(class = "text-muted", style = "margin-top:8px; font-size:12px;",
            "Rule: once per game at T-60 min (skipped if first seen inside T-45). Favorite = team with ",
            "full-game spread strikes asking 60-92c; enter only if market-implied favorite margin >= 8; ",
            "buy YES on the strike asking closest to 70c. 'Now' columns are a live read of current quotes ",
            "(refreshed every 5 min) and can change before the entry window.")
        )
      )
    )
  )
}

#' @param automated_trader reactiveVal holding the AutomatedTrader
#' @param state_version reactive counter for periodic refresh
pregameFavServer <- function(id, automated_trader, state_version) {
  moduleServer(id, function(input, output, session) {

    slate_timer <- reactiveTimer(60000)

    strategy <- reactive({
      tr <- automated_trader()
      if (is.null(tr)) NULL else tr$pregame_strategy
    })

    # Use the trader's scoreboard when it is polling; otherwise fetch our own
    # so the slate still shows while the loop is stopped.
    games <- reactive({
      slate_timer()
      tr <- automated_trader()
      g <- if (!is.null(tr) && isTRUE(tr$is_running)) tr$games_today else NULL
      if (length(g) == 0) {
        g <- tryCatch(get_nba_scoreboard(Sys.Date())$games, error = function(e) list())
      }
      g %||% list()
    })

    output$status_banner <- renderUI({
      state_version()
      tr <- automated_trader()
      st <- strategy()
      if (is.null(st)) {
        return(div(class = "alert alert-danger",
          icon("exclamation-triangle"),
          " Strategy not loaded. Needs paper mode, a connected Kalshi price client, and ",
          "enable_pregame_fav = TRUE. If you just updated the code, fully restart the R process ",
          "(refreshing the browser does not reload global.R)."))
      }
      if (!isTRUE(tr$is_running)) {
        return(div(class = "alert alert-warning",
          icon("pause-circle"),
          " Trading loop is STOPPED — no pregame entries will be made. Start trading and keep this ",
          "browser tab open through each game's T-60 window (closing the tab stops the loop)."))
      }
      div(class = "alert alert-success", icon("play-circle"),
          sprintf(" Running — %d contracts per entry. Decisions log: %s",
                  st$contracts, basename(st$log_path)))
    })

    output$scorecard <- renderUI({
      state_version()
      st <- strategy(); req(st)
      s <- st$summary()
      box_ <- function(label, value) column(2, div(style = "text-align:center; padding:6px;",
        tags$div(style = "font-size:20px; font-weight:600;", value),
        tags$div(class = "text-muted", style = "font-size:12px;", label)))
      fluidRow(
        box_("Games evaluated", s$evaluated),
        box_("Entered", s$entered),
        box_("Settled", s$settled),
        box_("Win rate", if (is.na(s$win_rate)) "—" else sprintf("%.0f%%", 100 * s$win_rate)),
        box_("Total P&L", sprintf("$%.2f", s$total_pnl)),
        box_("Return", if (is.na(s$return_pct)) "—" else sprintf("%+.1f%%", s$return_pct))
      )
    })

    slate <- reactive({
      state_version()
      st <- strategy(); req(st)
      gs <- games()
      if (length(gs) == 0) return(tibble())
      now <- Sys.time()
      parse_ts <- function(ts) {
        v <- as.POSIXct(ts %||% NA, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        if (is.na(v)) v <- as.POSIXct(ts %||% NA, format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
        v
      }
      base <- map_dfr(gs, function(g) tibble(
        game_id = as.character(g$game_id),
        matchup = sprintf("%s @ %s", normalize_team_abbrev(g$away_team_abbrev %||% ""),
                          normalize_team_abbrev(g$home_team_abbrev %||% "")),
        tip = parse_ts(g$start_time),
        scheduled = isTRUE(g$is_scheduled)))
      live <- tryCatch(st$preview(gs), error = function(e) tibble())
      if (nrow(live) == 0) live <- tibble(game_id = character())
      for (col in c("now_decision", "now_fav", "now_leg")) if (!col %in% names(live)) live[[col]] <- NA_character_
      if (!"now_implied" %in% names(live)) live$now_implied <- NA_real_
      dec <- st$decisions %>% select(game_id, decision, fav_team, implied_median, leg_ticker,
                                     leg_threshold, leg_ask, contracts, result, pnl)

      base %>% left_join(dec, by = "game_id") %>% left_join(live, by = "game_id") %>%
        arrange(tip) %>%
        mutate(
          mins = as.numeric(difftime(tip, now, units = "mins")),
          Tip = format(tip, "%I:%M %p ET", tz = "America/New_York"),
          Window = format(tip - 60 * 60, "%I:%M %p", tz = "America/New_York"),
          Status = case_when(
            decision == "entered" & result %in% "void" ~ sprintf(
              '<span class="badge badge-secondary">VOID</span> %s >%.1f refunded at cost', fav_team, leg_threshold),
            decision == "entered" & !is.na(result) ~ sprintf(
              '<span class="badge badge-%s">SETTLED %s</span> %s $%.2f',
              ifelse(result == "yes", "success", "danger"), ifelse(result == "yes", "WIN", "LOSS"),
              ifelse(pnl >= 0, "+", ""), pnl),
            decision == "entered" ~ sprintf('<span class="badge badge-primary">ENTERED</span> %s >%.1f x%d @ %.2f',
                                            fav_team, leg_threshold, contracts, leg_ask),
            decision == "below_margin_filter" ~ sprintf('<span class="badge badge-secondary">SKIP</span> implied margin %.1f < 8', implied_median),
            decision == "no_fav_legs" ~ '<span class="badge badge-secondary">SKIP</span> no strikes 60-92c',
            decision == "ambiguous_fav" ~ '<span class="badge badge-secondary">SKIP</span> two-sided favorite',
            decision == "missed_window" ~ '<span class="badge badge-warning">MISSED</span> first seen inside T-45',
            decision == "no_market_by_window" ~ '<span class="badge badge-warning">NO MARKET</span> no quotable ladder by T-45',
            decision == "entry_rejected" ~ '<span class="badge badge-danger">REJECTED</span> see log',
            !scheduled ~ '<span class="badge badge-secondary">NOT EVALUATED</span> started before loop saw it',
            mins < 45 ~ '<span class="badge badge-warning">WINDOW PASSED</span> not evaluated',
            mins <= 60 ~ '<span class="badge badge-info">IN WINDOW</span> evaluating',
            TRUE ~ sprintf('<span class="badge badge-info">WAITING</span> opens in %s',
                           ifelse(mins - 60 >= 60, sprintf("%dh %02dm", (mins - 60) %/% 60, round((mins - 60) %% 60)),
                                  sprintf("%dm", round(mins - 60))))
          ),
          `Now (live)` = case_when(
            !is.na(decision) | !scheduled ~ "",
            now_decision == "qualifies" ~ sprintf('<b class="text-success">qualifies</b> — %s, margin %.1f', now_leg, now_implied),
            now_decision == "below_margin_filter" ~ sprintf("%s margin %.1f (below 8)", now_fav, now_implied),
            now_decision == "no_fav_legs" ~ "no strikes 60-92c",
            now_decision == "no_market_yet" ~ "market not listed yet",
            TRUE ~ coalesce(now_decision, "")
          )
        ) %>%
        select(Game = matchup, Tip, `Entry window` = Window, Status, `Now (live)`)
    })

    output$slate_table <- renderDT({
      d <- slate()
      if (nrow(d) == 0) {
        return(datatable(tibble(Message = "No games on today's slate"), options = list(dom = "t"), rownames = FALSE))
      }
      datatable(d, escape = FALSE, rownames = FALSE,
                options = list(dom = "t", pageLength = 20, ordering = FALSE))
    })
  })
}
