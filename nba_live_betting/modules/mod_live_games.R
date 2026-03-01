# Live Games Module
# Displays live games with scores, win probability, and trading signals

#' Live Games UI
#' @param id Module namespace id
liveGamesUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(12,
        div(class = "box box-primary",
          div(class = "box-header with-border",
            h3(class = "box-title", icon("basketball"), " Live Games"),
            div(class = "box-tools pull-right",
              actionButton(ns("refresh"), "Refresh", icon = icon("sync"),
                          class = "btn-sm btn-default")
            )
          ),
          div(class = "box-body",
            uiOutput(ns("games_grid"))
          )
        )
      )
    )
  )
}

#' Live Games Server
#' @param id Module namespace id
#' @param state Reactive TradingState object
#' @param predictor KalshiPredictor object
#' @param team_ratings Reactive team ratings tibble (optional)
#' @param state_version Reactive counter for forcing UI refresh
liveGamesServer <- function(id, state, predictor, team_ratings = NULL, state_version = NULL) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values for games
    games <- reactiveVal(list())
    last_refresh <- reactiveVal(Sys.time())

    # Auto-refresh timer (30 seconds)
    auto_timer <- shiny::reactiveTimer(30000)

    # Fetch games on timer or manual refresh
    shiny::observe({
      auto_timer()
      input$refresh

      tryCatch({
        scoreboard <- get_nba_scoreboard(Sys.Date())
        games(scoreboard$games)
        last_refresh(Sys.time())
      }, error = function(e) {
        shiny::showNotification(
          sprintf("Failed to fetch games: %s", e$message),
          type = "error"
        )
      })
    })

    # Render games grid
    output$games_grid <- renderUI({
      # Force periodic refresh for position counts on game cards
      if (!is.null(state_version)) state_version()
      req(state())  # Ensure state is initialized before rendering
      game_list <- games()

      # Get current team ratings
      ratings <- if (!is.null(team_ratings) && is.function(team_ratings)) {
        tryCatch(team_ratings(), error = function(e) NULL)
      } else if (!is.null(team_ratings) && is.data.frame(team_ratings)) {
        team_ratings
      } else {
        NULL
      }

      if (length(game_list) == 0) {
        return(div(
          class = "text-center",
          style = "padding: 40px;",
          icon("calendar-times", class = "fa-3x text-muted"),
          h4("No games today", class = "text-muted"),
          p("Check back later for scheduled games.")
        ))
      }

      # Sort: in-progress first, then scheduled, then final
      game_list <- game_list[order(
        -sapply(game_list, function(g) isTRUE(g$is_in_progress)),
        -sapply(game_list, function(g) !isTRUE(g$is_final) && !isTRUE(g$is_in_progress)),
        sapply(game_list, function(g) isTRUE(g$is_final))
      )]

      # Get state once (avoid multiple reactive calls inside lapply)
      current_state <- state()

      # Create game cards (2 per row)
      rows <- lapply(seq(1, length(game_list), by = 2), function(i) {
        cols <- list(
          column(6, game_card(game_list[[i]], current_state, predictor, ns, ratings))
        )
        if (i + 1 <= length(game_list)) {
          cols <- c(cols, list(
            column(6, game_card(game_list[[i + 1]], current_state, predictor, ns, ratings))
          ))
        }
        fluidRow(cols)
      })

      tagList(
        div(class = "text-muted small mb-2",
            sprintf("Last updated: %s", format(last_refresh(), "%H:%M:%S"))),
        rows
      )
    })
  })
}

#' Create a game card
#' @param game Game data from ESPN
#' @param state TradingState object
#' @param predictor KalshiPredictor
#' @param ns Namespace function
#' @param team_ratings Team ratings tibble (optional)
game_card <- function(game, state, predictor, ns, team_ratings = NULL) {
  # Determine card style based on status (use isTRUE for safe boolean checks)
  is_live <- isTRUE(game$is_in_progress)
  is_final <- isTRUE(game$is_final)
  is_scheduled <- isTRUE(game$is_scheduled)

  if (is_live) {
    border_class <- "border-success"
    status_badge <- span(class = "badge badge-success", "LIVE")
    status_text <- format_game_status(game)
  } else if (is_final) {
    border_class <- "border-secondary"
    status_badge <- span(class = "badge badge-secondary", "FINAL")
    winner <- if (game$home_score > game$away_score) game$home_team else game$away_team
    status_text <- sprintf("%s wins", winner)
  } else if (is_scheduled) {
    border_class <- "border-info"
    status_badge <- span(class = "badge badge-info", "SCHEDULED")
    status_text <- format_game_status(game)
  } else {
    # Unknown status - show what we have
    border_class <- "border-warning"
    status_badge <- span(class = "badge badge-warning", "STATUS")
    status_text <- format_game_status(game)
  }

  # Get team colors
  home_color <- TEAM_COLORS[game$home_team_abbrev] %||% "#333"
  away_color <- TEAM_COLORS[game$away_team_abbrev] %||% "#333"

  # Look up team ratings
  home_rating <- 0
  away_rating <- 0
  rating_diff <- 0

  if (!is.null(team_ratings) && is.data.frame(team_ratings) && nrow(team_ratings) > 0) {
    home_abbrev <- game$home_team_abbrev
    away_abbrev <- game$away_team_abbrev

    if (!is.null(home_abbrev)) {
      home_row <- team_ratings[team_ratings$team_abbreviation == home_abbrev, , drop = FALSE]
      if (nrow(home_row) > 0 && !is.na(home_row$net_rating[1])) {
        home_rating <- home_row$net_rating[1]
      }
    }

    if (!is.null(away_abbrev)) {
      away_row <- team_ratings[team_ratings$team_abbreviation == away_abbrev, , drop = FALSE]
      if (nrow(away_row) > 0 && !is.na(away_row$net_rating[1])) {
        away_rating <- away_row$net_rating[1]
      }
    }

    rating_diff <- home_rating - away_rating
  }

  # Get positions for this game (safely)
  n_positions <- 0
  open_positions <- 0
  tryCatch({
    if (!is.null(state) && is.function(state$get_game_positions)) {
      positions <- state$get_game_positions(game$game_id)
      if (is.data.frame(positions)) {
        n_positions <- nrow(positions)
        open_positions <- sum(positions$status == "open", na.rm = TRUE)
      }
    }
  }, error = function(e) {
    # Ignore position lookup errors
  })

  # Get win probability
  wp_display <- ""
  signal_display <- ""
  ratings_display <- ""

  # Build ratings display (show for all games)
  if (home_rating != 0 || away_rating != 0) {
    # Determine which team has the advantage
    if (rating_diff > 0) {
      adv_text <- sprintf("Home %+.1f", rating_diff)
      adv_color <- home_color
    } else if (rating_diff < 0) {
      adv_text <- sprintf("Away %+.1f", abs(rating_diff))
      adv_color <- away_color
    } else {
      adv_text <- "Even"
      adv_color <- "#666"
    }

    ratings_display <- div(
      style = "font-size: 11px; margin-top: 5px; display: flex; justify-content: space-between;",
      span(style = sprintf("color: %s;", away_color), sprintf("%.1f", away_rating)),
      span(style = sprintf("color: %s; font-weight: bold;", adv_color), adv_text),
      span(style = sprintf("color: %s;", home_color), sprintf("%+.1f", home_rating))
    )
  }

  # For SCHEDULED games - show pre-game win probability
  if (is_scheduled && !is.null(predictor)) {
    tryCatch({
      # Create pre-game state (score 0-0, start of game)
      pregame_state <- list(
        home_score = 0,
        away_score = 0,
        home_margin = 0,
        seconds_remaining = 2880,
        seconds_elapsed = 0,
        minutes_elapsed = 0,
        period = 1,
        clock_minutes = 12,
        clock_seconds = 0,
        home_net_rating = home_rating,
        away_net_rating = away_rating,
        home_rating_advantage = rating_diff,
        rest_advantage = 0,
        net_rating_advantage = 0
      )

      # Get pre-game WP using safe wrapper
      wp_result <- tryCatch({
        if (exists("safe_get_wp", envir = globalenv())) {
          get("safe_get_wp", envir = globalenv())(predictor, pregame_state)
        } else {
          predictor$get_adjusted_wp(pregame_state)
        }
      }, error = function(e) {
        NULL
      })

      if (is.list(wp_result) && !is.null(wp_result$adjusted_wp)) {
        home_wp <- as.numeric(wp_result$adjusted_wp)
        if (length(home_wp) == 1 && !is.na(home_wp)) {
          home_wp <- max(0.01, min(0.99, home_wp))
          away_wp <- 1 - home_wp

          # Pre-game WP bar
          wp_display <- div(
            div(class = "text-muted small", style = "margin-top: 8px;", "Pre-game Win Probability"),
            div(
              class = "progress",
              style = "height: 20px; margin: 5px 0;",
              div(class = "progress-bar",
                  style = sprintf("width: %.0f%%; background-color: %s;", home_wp * 100, home_color),
                  sprintf("%.0f%%", home_wp * 100)),
              div(class = "progress-bar",
                  style = sprintf("width: %.0f%%; background-color: %s;", away_wp * 100, away_color),
                  sprintf("%.0f%%", away_wp * 100))
            )
          )
        }
      }
    }, error = function(e) {
      # Silently ignore prediction errors
    })
  }

  # For LIVE games - show current win probability and signals
  if (is_live && !is.null(predictor)) {
    tryCatch({
      game_state <- tryCatch({
        espn_to_game_state(game, home_net_rating = home_rating, away_net_rating = away_rating)
      }, error = function(e) {
        NULL
      })

      if (!is.list(game_state)) {
        game_state <- NULL
      }

      if (!is.null(game_state)) {
        # Get adjusted WP using safe wrapper from global environment
        wp_result <- tryCatch({
          if (exists("safe_get_wp", envir = globalenv())) {
            get("safe_get_wp", envir = globalenv())(predictor, game_state)
          } else {
            predictor$get_adjusted_wp(game_state)
          }
        }, error = function(e) {
          NULL
        })

        if (is.list(wp_result) && !is.null(wp_result$adjusted_wp)) {
          home_wp <- as.numeric(wp_result$adjusted_wp)
          if (length(home_wp) == 1 && !is.na(home_wp)) {
            home_wp <- max(0.01, min(0.99, home_wp))
            away_wp <- 1 - home_wp

            # WP bar
            wp_display <- div(
              class = "progress",
              style = "height: 20px; margin: 10px 0;",
              div(class = "progress-bar",
                  style = sprintf("width: %.0f%%; background-color: %s;", home_wp * 100, home_color),
                  sprintf("%.0f%%", home_wp * 100)),
              div(class = "progress-bar",
                  style = sprintf("width: %.0f%%; background-color: %s;", away_wp * 100, away_color),
                  sprintf("%.0f%%", away_wp * 100))
            )

            # Get signal preview using safe wrapper from global environment
            signal <- tryCatch({
              if (exists("safe_get_signal", envir = globalenv())) {
                get("safe_get_signal", envir = globalenv())(predictor, game$game_id, game_state, home_wp)
              } else {
                predictor$get_signal(game$game_id, game_state, home_wp)
              }
            }, error = function(e) {
              NULL
            })

            # Safely extract signals
            signals <- c()
            if (is.list(signal) && is.list(signal$with_leader)) {
              wl_sig <- as.character(signal$with_leader$signal %||% "HOLD")
              if (length(wl_sig) == 1 && !is.na(wl_sig) && wl_sig != "HOLD") {
                signals <- c(signals, sprintf("WL: %s", wl_sig))
              }
            }
            if (is.list(signal) && is.list(signal$fade)) {
              fade_sig <- as.character(signal$fade$signal %||% "HOLD")
              if (length(fade_sig) == 1 && !is.na(fade_sig) && fade_sig != "HOLD") {
                edge <- as.numeric(signal$fade$edge %||% 0)
                if (length(edge) == 1 && !is.na(edge)) {
                  signals <- c(signals, sprintf("Fade: %s (%.1f%% edge)", fade_sig, edge * 100))
                } else {
                  signals <- c(signals, sprintf("Fade: %s", fade_sig))
                }
              }
            }

            if (length(signals) > 0) {
              signal_display <- div(
                class = "alert alert-warning py-1 px-2 mb-0 mt-2",
                style = "font-size: 11px;",
                icon("lightbulb"), " Signal: ",
                paste(signals, collapse = " | ")
              )
            }
          }
        }
      }
    }, error = function(e) {
      # Silently ignore prediction errors
    })
  }

  # Position indicator
  position_display <- ""
  if (n_positions > 0) {
    position_display <- div(
      class = "mt-2",
      style = "font-size: 11px;",
      icon("chart-line"),
      sprintf(" %d positions (%d open)", n_positions, open_positions)
    )
  }

  # Build card
  div(
    class = paste("card mb-3", border_class),
    style = "border-width: 2px;",
    div(
      class = "card-header d-flex justify-content-between align-items-center",
      style = "padding: 8px 12px;",
      div(status_badge, " ", status_text),
      div(class = "text-muted small", game$broadcast %||% "")
    ),
    div(
      class = "card-body",
      style = "padding: 12px;",

      # Teams and scores
      fluidRow(
        column(5,
          div(
            style = sprintf("color: %s; font-weight: bold;", away_color),
            game$away_team
          ),
          div(
            style = sprintf("color: %s; font-weight: bold;", home_color),
            game$home_team
          )
        ),
        column(3,
          if (is_scheduled) {
            # Show "vs" for scheduled games
            div(style = "font-size: 18px; color: #999; text-align: center; padding-top: 8px;", "vs")
          } else {
            tagList(
              div(
                style = sprintf("font-size: 24px; font-weight: bold; color: %s;",
                               if (game$away_score > game$home_score) away_color else "#999"),
                game$away_score
              ),
              div(
                style = sprintf("font-size: 24px; font-weight: bold; color: %s;",
                               if (game$home_score > game$away_score) home_color else "#999"),
                game$home_score
              )
            )
          }
        ),
        column(4,
          if (is_live) {
            div(
              style = "text-align: right;",
              div(style = "font-size: 18px; font-weight: bold;",
                  format_game_clock(game$period %||% 1, game$clock_minutes %||% 0, game$clock_seconds %||% 0)),
              div(class = "text-muted small",
                  sprintf("Lead: %+d", game$home_margin %||% 0))
            )
          }
        )
      ),

      # Team ratings display
      ratings_display,

      # Win probability bar
      wp_display,

      # Signal indicator
      signal_display,

      # Position indicator
      position_display
    )
  )
}
