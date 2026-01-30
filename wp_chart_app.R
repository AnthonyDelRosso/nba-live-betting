# Shiny App: Daily NBA Win Probability Charts
# Displays WP charts for all games on a selected date

library(shiny)
library(tidyverse)
library(hoopR)
library(ggplot2)
library(ggimage)

source("win_probability_model.R")

# Set higher connection buffer for data loading
Sys.setenv("VROOM_CONNECTION_SIZE" = 262144)

# ------------------------------------------------------------------------------
# Team colors and ESPN IDs (from show_wp_chart.R)
# ------------------------------------------------------------------------------

team_colors <- c(
  "Atlanta Hawks" = "#E03A3E",
  "Boston Celtics" = "#007A33",
  "Brooklyn Nets" = "#000000",
  "Charlotte Hornets" = "#1D1160",
  "Chicago Bulls" = "#CE1141",
  "Cleveland Cavaliers" = "#860038",
  "Dallas Mavericks" = "#00538C",
  "Denver Nuggets" = "#0E2240",
  "Detroit Pistons" = "#C8102E",
  "Golden State Warriors" = "#1D428A",
  "Houston Rockets" = "#CE1141",
  "Indiana Pacers" = "#002D62",
  "LA Clippers" = "#C8102E",
  "Los Angeles Lakers" = "#552583",
  "Memphis Grizzlies" = "#5D76A9",
  "Miami Heat" = "#98002E",
  "Milwaukee Bucks" = "#00471B",
  "Minnesota Timberwolves" = "#0C2340",
  "New Orleans Pelicans" = "#0C2340",
  "New York Knicks" = "#006BB6",
  "Oklahoma City Thunder" = "#007AC1",
  "Orlando Magic" = "#0077C0",
  "Philadelphia 76ers" = "#006BB6",
  "Phoenix Suns" = "#1D1160",
  "Portland Trail Blazers" = "#E03A3E",
  "Sacramento Kings" = "#5A2D81",
  "San Antonio Spurs" = "#C4CED4",
  "Toronto Raptors" = "#CE1141",
  "Utah Jazz" = "#002B5C",
  "Washington Wizards" = "#002B5C"
)

team_espn_ids <- c(
  "Atlanta Hawks" = "1",
  "Boston Celtics" = "2",
  "Brooklyn Nets" = "17",
  "Charlotte Hornets" = "30",
  "Chicago Bulls" = "4",
  "Cleveland Cavaliers" = "5",
  "Dallas Mavericks" = "6",
  "Denver Nuggets" = "7",
  "Detroit Pistons" = "8",
  "Golden State Warriors" = "9",
  "Houston Rockets" = "10",
  "Indiana Pacers" = "11",
  "LA Clippers" = "12",
  "Los Angeles Lakers" = "13",
  "Memphis Grizzlies" = "29",
  "Miami Heat" = "14",
  "Milwaukee Bucks" = "15",
  "Minnesota Timberwolves" = "16",
  "New Orleans Pelicans" = "3",
  "New York Knicks" = "18",
  "Oklahoma City Thunder" = "25",
  "Orlando Magic" = "19",
  "Philadelphia 76ers" = "20",
  "Phoenix Suns" = "21",
  "Portland Trail Blazers" = "22",
  "Sacramento Kings" = "23",
  "San Antonio Spurs" = "24",
  "Toronto Raptors" = "28",
  "Utah Jazz" = "26",
  "Washington Wizards" = "27"
)

# ------------------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------------------

get_logo_url <- function(team_name) {
  team_id <- team_espn_ids[team_name]
  if (is.na(team_id)) return(NA_character_)
  sprintf("https://a.espncdn.com/i/teamlogos/nba/500/%s.png", team_id)
}

# Split segments that cross 50% for accurate color transitions
split_crossing_segments <- function(df, home_col, away_col) {
  result <- list()

  for (i in 1:nrow(df)) {
    row <- df[i, ]
    wp_start <- row$home_wp
    wp_end <- row$next_wp
    min_start <- row$minutes
    min_end <- row$next_minutes

    crosses_50 <- (wp_start < 0.5 & wp_end > 0.5) | (wp_start > 0.5 & wp_end < 0.5)

    if (crosses_50 && wp_start != wp_end) {
      t <- (0.5 - wp_start) / (wp_end - wp_start)
      cross_minutes <- min_start + t * (min_end - min_start)

      seg1 <- tibble(
        minutes = min_start,
        next_minutes = cross_minutes,
        home_wp = wp_start,
        next_wp = 0.5,
        segment_color = if_else(wp_start >= 0.5, home_col, away_col)
      )

      seg2 <- tibble(
        minutes = cross_minutes,
        next_minutes = min_end,
        home_wp = 0.5,
        next_wp = wp_end,
        segment_color = if_else(wp_end >= 0.5, home_col, away_col)
      )

      result <- append(result, list(seg1, seg2))
    } else {
      seg <- tibble(
        minutes = min_start,
        next_minutes = min_end,
        home_wp = wp_start,
        next_wp = wp_end,
        segment_color = if_else(wp_start >= 0.5, home_col, away_col)
      )
      result <- append(result, list(seg))
    }
  }

  bind_rows(result)
}

# Generate a win probability chart for a single game
generate_wp_chart <- function(game_id, home_team, away_team,
                               home_score, away_score, game_date,
                               pbp_data, model, is_in_progress = FALSE,
                               home_rating_advantage = 0) {

  # For in-progress games, fetch live PBP data from ESPN
  # For completed games, use cached season PBP data
  if (is_in_progress) {
    pbp <- tryCatch({
      live_data <- espn_nba_pbp(game_id = game_id)
      plays <- live_data %>% pluck("plays") %>% as_tibble()

      # Parse clock_display_value (e.g., "5:32") into minutes and seconds
      if ("clock_display_value" %in% names(plays) && !"clock_minutes" %in% names(plays)) {
        plays <- plays %>%
          mutate(
            clock_parts = strsplit(as.character(clock_display_value), ":"),
            clock_minutes = sapply(clock_parts, function(x) as.numeric(x[1])),
            clock_seconds = sapply(clock_parts, function(x) if(length(x) > 1) as.numeric(x[2]) else 0)
          ) %>%
          select(-clock_parts)
      }

      # Rename period to period_number if needed
      if ("period" %in% names(plays) && !"period_number" %in% names(plays)) {
        plays <- plays %>% rename(period_number = period)
      }

      plays
    }, error = function(e) {
      tibble()
    })
  } else {
    pbp <- pbp_data %>% filter(game_id == !!game_id)
  }

  if (nrow(pbp) == 0) {
    return(ggplot() +
             annotate("text", x = 0.5, y = 0.5, label = "No play-by-play data available") +
             theme_void())
  }

  # Get team colors
  home_color <- team_colors[home_team]
  away_color <- team_colors[away_team]

  # Use fallback colors if not found
  if (is.na(home_color)) home_color <- "#1D428A"
  if (is.na(away_color)) away_color <- "#CE1141"

  # Get logos
  home_logo <- get_logo_url(home_team)
  away_logo <- get_logo_url(away_team)

  # Calculate game states
tryCatch({
  # Use the passed home_rating_advantage for all game states
  rating_adv <- home_rating_advantage

  game_wp <- pbp %>%
    filter(!is.na(home_score), !is.na(away_score)) %>%
    mutate(
      home_margin = home_score - away_score,
      seconds_elapsed = case_when(
        period_number <= 4 ~ (period_number - 1) * 720 +
          (720 - (clock_minutes * 60 + clock_seconds)),
        period_number > 4 ~ 2880 + (period_number - 5) * 300 +
          (300 - (clock_minutes * 60 + clock_seconds))
      ),
      seconds_remaining = 2880 - seconds_elapsed,
      period = period_number,
      home_rating_advantage = rating_adv
    ) %>%
    select(seconds_elapsed, seconds_remaining, period, home_score, away_score,
           home_margin, home_rating_advantage)

  if (nrow(game_wp) == 0) {
    return(ggplot() +
             annotate("text", x = 0.5, y = 0.5, label = "Insufficient data") +
             theme_void())
  }

  # Get predictions
  game_wp <- predict_wp_states(model, game_wp)

  # Prepare segments
  game_wp_base <- game_wp %>%
    arrange(seconds_elapsed) %>%
    mutate(
      minutes = seconds_elapsed / 60,
      next_minutes = lead(minutes),
      next_wp = lead(home_wp)
    ) %>%
    filter(!is.na(next_wp))

  if (nrow(game_wp_base) == 0) {
    return(ggplot() +
             annotate("text", x = 0.5, y = 0.5, label = "Insufficient data") +
             theme_void())
  }

  game_wp_segments <- split_crossing_segments(game_wp_base, home_color, away_color)

  # Determine if game went to OT
  max_period <- max(game_wp$period)
  x_max <- if_else(max_period > 4, max(game_wp$seconds_elapsed) / 60, 48)

  # Determine winner or in-progress status
  final_margin <- tail(game_wp$home_margin, 1)
  if (is_in_progress) {
    status_text <- "In Progress"
  } else {
    winner <- if_else(final_margin > 0, home_team, away_team)
    status_text <- paste(winner, "wins")
  }

  # Format rating advantage for display
  rating_text <- if (abs(rating_adv) < 0.1) {
    "Even"
  } else if (rating_adv > 0) {
    sprintf("Home +%.1f", rating_adv)
  } else {
    sprintf("Away +%.1f", abs(rating_adv))
  }

  # Logo data
  logo_data <- tibble(
    x = c(-3, -3),
    y = c(0.88, 0.12),
    image = c(home_logo, away_logo),
    team = c(home_team, away_team)
  )

  # Create plot
  p <- ggplot(game_wp_segments) +
    geom_segment(
      aes(x = minutes, xend = next_minutes, y = home_wp, yend = next_wp, color = segment_color),
      linewidth = 1.2
    ) +
    scale_color_identity() +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray40", linewidth = 0.6) +
    geom_vline(xintercept = c(12, 24, 36), linetype = "dotted", color = "gray60", alpha = 0.7) +
    annotate("text", x = 6, y = 0.03, label = "Q1", color = "gray50", size = 2.5) +
    annotate("text", x = 18, y = 0.03, label = "Q2", color = "gray50", size = 2.5) +
    annotate("text", x = 30, y = 0.03, label = "Q3", color = "gray50", size = 2.5) +
    annotate("text", x = 42, y = 0.03, label = "Q4", color = "gray50", size = 2.5)

  # Add logos if available
  if (!is.na(home_logo) && !is.na(away_logo)) {
    p <- p + geom_image(data = logo_data, aes(x = x, y = y, image = image), size = 0.1, asp = 1.6)
  }

  # Add team names
  p <- p +
    annotate("text", x = 0, y = 0.88, label = home_team, color = home_color,
             size = 2.5, fontface = "bold", hjust = 0) +
    annotate("text", x = 0, y = 0.12, label = away_team, color = away_color,
             size = 2.5, fontface = "bold", hjust = 0) +
    scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent,
      breaks = seq(0, 1, 0.25)
    ) +
    scale_x_continuous(limits = c(-5, x_max), breaks = seq(0, 48, 12)) +
    labs(
      title = sprintf("%s @ %s", away_team, home_team),
      subtitle = sprintf("%s | %d-%d | %s | %s",
                         format(as.Date(game_date), "%b %d"),
                         away_score, home_score, status_text, rating_text),
      x = "Minutes",
      y = "Win Probability"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9, color = "gray40"),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7),
      plot.margin = margin(5, 10, 5, 5)
    )

  p
  }, error = function(e) {
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message)) +
      theme_void()
  })
}

# ------------------------------------------------------------------------------
# Shiny UI
# ------------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("NBA Win Probability Charts"),

  sidebarLayout(
    sidebarPanel(
      selectInput("season", "Season",
                  choices = 2015:2026,
                  selected = 2026),
      dateInput("game_date", "Game Date",
                value = Sys.Date() - 1,
                max = Sys.Date()),
      helpText("Select a date to view win probability charts for completed and in-progress games."),
      hr(),
      textOutput("game_count"),
      uiOutput("refresh_status"),
      width = 2
    ),

    mainPanel(
      uiOutput("loading_message"),
      uiOutput("charts_ui"),
      width = 10
    )
  )
)

# ------------------------------------------------------------------------------
# Shiny Server
# ------------------------------------------------------------------------------

server <- function(input, output, session) {

  # Load model once at startup
  model <- tryCatch({
    load_wp_model()
  }, error = function(e) {
    showNotification("Win probability model not found. Please run train_wp_model() first.",
                     type = "error", duration = NULL)
    NULL
  })

 # Auto-refresh timer (every 30 seconds)
  auto_refresh <- reactiveTimer(30000)

  # Track last refresh time
  last_refresh <- reactiveVal(Sys.time())

  # Reactive: load team ratings for selected season
  team_ratings_data <- reactive({
    req(input$season)

    withProgress(message = "Loading team ratings...", {
      team_box <- load_nba_team_box(seasons = as.integer(input$season)) %>%
        filter(season_type == 2)  # Regular season only

      # Calculate cumulative net ratings going into each game
      team_box %>%
        arrange(team_abbreviation, game_date) %>%
        mutate(
          poss = field_goals_attempted + 0.44 * free_throws_attempted -
            offensive_rebounds + turnovers
        ) %>%
        group_by(team_abbreviation) %>%
        mutate(
          cum_pts = lag(cumsum(team_score), default = NA_real_),
          cum_pts_allowed = lag(cumsum(opponent_team_score), default = NA_real_),
          cum_poss = lag(cumsum(poss), default = NA_real_),
          net_rating = ((cum_pts / cum_poss) - (cum_pts_allowed / cum_poss)) * 100
        ) %>%
        ungroup() %>%
        select(game_id, team_abbreviation, team_home_away, net_rating)
    })
  })

  # Reactive: game ratings (home and away net ratings per game)
  game_ratings <- reactive({
    req(team_ratings_data())

    ratings <- team_ratings_data()

    home_ratings <- ratings %>%
      filter(team_home_away == "home") %>%
      select(game_id, home_net_rating = net_rating)

    away_ratings <- ratings %>%
      filter(team_home_away == "away") %>%
      select(game_id, away_net_rating = net_rating)

    home_ratings %>%
      left_join(away_ratings, by = "game_id") %>%
      mutate(
        home_rating_advantage = coalesce(home_net_rating, 0) - coalesce(away_net_rating, 0)
      ) %>%
      distinct()
  })

  # Reactive: load schedule for selected season (cached)
  schedule_data <- reactive({
    req(input$season, game_ratings())

    withProgress(message = "Loading schedule...", {
      schedule <- load_nba_schedule(seasons = as.integer(input$season)) %>%
        mutate(
          game_date_only = as.Date(game_date),
          is_in_progress = status_type_name == "STATUS_IN_PROGRESS",
          is_completed = status_type_completed == TRUE,
          is_scheduled = status_type_name == "STATUS_SCHEDULED",
          start_time = start_date
        ) %>%
        select(game_id, game_date, game_date_only,
               home_name = home_display_name, away_name = away_display_name,
               home_score, away_score, is_in_progress, is_completed, is_scheduled, start_time)

      # Join with game ratings
      schedule %>%
        left_join(game_ratings() %>% select(game_id, home_rating_advantage), by = "game_id") %>%
        mutate(home_rating_advantage = coalesce(home_rating_advantage, 0))
    })
  })

  # Reactive: PBP data for season (cached)
  pbp_data <- reactive({
    req(input$season)

    withProgress(message = "Loading play-by-play data...", {
      load_nba_pbp(seasons = as.integer(input$season))
    })
  })

  # Reactive: games for selected date (all statuses)
  games_today <- reactive({
    req(schedule_data(), input$game_date)

    schedule_data() %>%
      filter(game_date_only == input$game_date)
  })

  # Reactive: games with PBP data (completed or in-progress)
 games_with_pbp <- reactive({
    games_today() %>%
      filter(is_completed | is_in_progress)
  })

  # Reactive: scheduled games (not yet started)
  games_scheduled <- reactive({
    games_today() %>%
      filter(is_scheduled)
  })

  # Display game count
  output$game_count <- renderText({
    games <- games_today()
    with_pbp <- games_with_pbp()
    scheduled <- games_scheduled()
    if (nrow(scheduled) > 0 && nrow(with_pbp) == 0) {
      sprintf("%d games scheduled", nrow(scheduled))
    } else if (nrow(scheduled) > 0) {
      sprintf("%d games (%d scheduled)", nrow(games), nrow(scheduled))
    } else {
      sprintf("%d games", nrow(games))
    }
  })

  # Display refresh status for in-progress games
  output$refresh_status <- renderUI({
    games <- games_with_pbp()
    if (nrow(games) > 0 && any(games$is_in_progress)) {
      auto_refresh()  # Trigger reactivity
      div(
        style = "margin-top: 10px; font-size: 12px; color: #666;",
        icon("sync", class = "fa-spin"),
        " Auto-refreshing",
        br(),
        sprintf("Last: %s", format(last_refresh(), "%H:%M:%S"))
      )
    } else {
      NULL
    }
  })

  # Loading message
  output$loading_message <- renderUI({
    if (is.null(model)) {
      div(class = "alert alert-danger",
          "Win probability model not found. Please run train_wp_model() first.")
    } else {
      NULL
    }
  })

  # Dynamic UI for chart grid
  output$charts_ui <- renderUI({
    req(model)
    games <- games_with_pbp()
    scheduled <- games_scheduled()

    ui_elements <- list()

    # Show scheduled games if any
    if (nrow(scheduled) > 0) {
      scheduled_info <- scheduled %>%
        mutate(
          # Parse UTC time and convert to local
          start_dt = as.POSIXct(start_time, format = "%Y-%m-%dT%H:%MZ", tz = "UTC"),
          start_local = format(start_dt, "%I:%M %p", tz = Sys.timezone())
        )

      scheduled_ui <- div(
        style = "background: #f8f9fa; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
        h4(sprintf("%d game(s) scheduled", nrow(scheduled)), style = "margin-top: 0;"),
        tags$ul(
          lapply(1:nrow(scheduled_info), function(i) {
            tags$li(sprintf("%s @ %s - %s",
                            scheduled_info$away_name[i],
                            scheduled_info$home_name[i],
                            scheduled_info$start_local[i]))
          })
        )
      )
      ui_elements <- append(ui_elements, list(scheduled_ui))
    }

    # Show message if no games with PBP
    if (nrow(games) == 0 && nrow(scheduled) == 0) {
      return(div(
        style = "text-align: center; padding: 50px;",
        h3("No games on this date"),
        p("Try selecting a different date during the regular season or playoffs.")
      ))
    }

    if (nrow(games) == 0) {
      ui_elements <- append(ui_elements, list(
        div(style = "text-align: center; padding: 20px; color: #666;",
            p("Win probability charts will appear once games start."))
      ))
      return(do.call(tagList, ui_elements))
    }

    # Create plot outputs for each game with PBP
    n_games <- nrow(games)

    # Create rows with 2 columns each
    rows <- lapply(seq(1, n_games, by = 2), function(i) {
      cols <- list(
        column(6, plotOutput(paste0("chart_", i), height = "320px"))
      )
      if (i + 1 <= n_games) {
        cols <- append(cols, list(
          column(6, plotOutput(paste0("chart_", i + 1), height = "320px"))
        ))
      }
      fluidRow(cols)
    })

    ui_elements <- append(ui_elements, rows)
    do.call(tagList, ui_elements)
  })

  # Render charts - use observeEvent to avoid reactive context issues
  # Include auto_refresh to update in-progress games
  shiny::observeEvent(list(games_with_pbp(), pbp_data(), auto_refresh()), {
    req(model)
    games <- isolate(games_with_pbp())
    pbp <- isolate(pbp_data())

    # Update last refresh time if there are in-progress games
    if (nrow(games) > 0 && any(games$is_in_progress)) {
      last_refresh(Sys.time())
    }

    if (nrow(games) == 0) return()

    lapply(1:nrow(games), function(i) {
      local({
        idx <- i
        game <- games[idx, ]

        output[[paste0("chart_", idx)]] <- renderPlot({
          generate_wp_chart(
            game_id = game$game_id,
            home_team = game$home_name,
            away_team = game$away_name,
            home_score = game$home_score,
            away_score = game$away_score,
            game_date = game$game_date,
            pbp_data = pbp,
            model = model,
            is_in_progress = game$is_in_progress,
            home_rating_advantage = game$home_rating_advantage
          )
        }, res = 96)
      })
    })
  }, ignoreNULL = TRUE)
}

# ------------------------------------------------------------------------------
# Run the app
# ------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
