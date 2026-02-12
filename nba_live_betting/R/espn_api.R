# ESPN API Client for Live NBA Scores
# Fetches live game data from ESPN's public API

library(httr2)
library(jsonlite)
library(tidyverse)

# ESPN NBA Scoreboard endpoint
ESPN_SCOREBOARD_URL <- "https://site.api.espn.com/apis/site/v2/sports/basketball/nba/scoreboard"

# Team abbreviation mapping (ESPN display name to standard abbreviation)
ESPN_TEAM_ABBREVS <- c(
  "Atlanta Hawks" = "ATL",
  "Boston Celtics" = "BOS",
  "Brooklyn Nets" = "BKN",
  "Charlotte Hornets" = "CHA",
  "Chicago Bulls" = "CHI",
  "Cleveland Cavaliers" = "CLE",
  "Dallas Mavericks" = "DAL",
  "Denver Nuggets" = "DEN",
  "Detroit Pistons" = "DET",
  "Golden State Warriors" = "GSW",
  "Houston Rockets" = "HOU",
  "Indiana Pacers" = "IND",
  "LA Clippers" = "LAC",
  "Los Angeles Lakers" = "LAL",
  "Memphis Grizzlies" = "MEM",
  "Miami Heat" = "MIA",
  "Milwaukee Bucks" = "MIL",
  "Minnesota Timberwolves" = "MIN",
  "New Orleans Pelicans" = "NOP",
  "New York Knicks" = "NYK",
  "Oklahoma City Thunder" = "OKC",
  "Orlando Magic" = "ORL",
  "Philadelphia 76ers" = "PHI",
  "Phoenix Suns" = "PHX",
  "Portland Trail Blazers" = "POR",
  "Sacramento Kings" = "SAC",
  "San Antonio Spurs" = "SAS",
  "Toronto Raptors" = "TOR",
  "Utah Jazz" = "UTA",
  "Washington Wizards" = "WAS"
)

#' Fetch NBA scoreboard from ESPN
#' @param date Date to fetch (defaults to today)
#' @return List with games and metadata
get_nba_scoreboard <- function(date = Sys.Date()) {
  date_str <- format(as.Date(date), "%Y%m%d")

  response <- tryCatch({
    request(ESPN_SCOREBOARD_URL) %>%
      req_url_query(dates = date_str) %>%
      req_headers(
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
        "Accept" = "application/json"
      ) %>%
      req_perform() %>%
      resp_body_json()
  }, error = function(e) {
    warning(sprintf("Failed to fetch ESPN scoreboard: %s", e$message))
    return(NULL)
  })

  if (is.null(response)) {
    return(list(games = list(), date = date))
  }

  # Parse events into structured game data
  games <- lapply(response$events, parse_espn_game)
  games <- games[!sapply(games, is.null)]

  list(
    games = games,
    date = date,
    fetched_at = Sys.time()
  )
}

#' Parse a single ESPN game event
#' @param event ESPN event object
#' @return Parsed game data
parse_espn_game <- function(event) {
  tryCatch({
    competition <- event$competitions[[1]]

    # Get competitors (home/away)
    competitors <- competition$competitors
    home_team <- competitors[[which(sapply(competitors, function(x) x$homeAway == "home"))]]
    away_team <- competitors[[which(sapply(competitors, function(x) x$homeAway == "away"))]]

    # Get status
    status <- competition$status
    status_type <- status$type$name %||% "UNKNOWN"
    status_state <- status$type$state %||% "pre"  # "pre", "in", "post"
    status_completed <- status$type$completed %||% FALSE
    status_description <- status$type$shortDetail %||% status$type$detail %||% ""

    # Determine game state using both status_type and status_state
    # ESPN status types: STATUS_SCHEDULED, STATUS_IN_PROGRESS, STATUS_HALFTIME,
    #                    STATUS_END_PERIOD, STATUS_FINAL, STATUS_POSTPONED, etc.
    is_final <- status_completed || status_type == "STATUS_FINAL" || status_state == "post"
    is_scheduled <- status_type == "STATUS_SCHEDULED" || status_state == "pre"
    is_in_progress <- !is_final && !is_scheduled && (
      status_state == "in" ||
      status_type %in% c("STATUS_IN_PROGRESS", "STATUS_HALFTIME", "STATUS_END_PERIOD")
    )

    # Parse clock and period for any non-scheduled game
    period <- status$period %||% 0
    clock_minutes <- 0
    clock_seconds <- 0
    clock_display <- status$displayClock %||% "0:00"

    # Get scores FIRST (before using them in debug messages)
    home_score <- as.integer(home_team$score %||% 0)
    away_score <- as.integer(away_team$score %||% 0)

    if (!is_scheduled && period > 0) {
      # Parse clock (format: "5:32" or "0:45.3")
      clock_parts <- strsplit(gsub("\\..*", "", clock_display), ":")[[1]]
      if (length(clock_parts) == 2) {
        clock_minutes <- as.numeric(clock_parts[1])
        clock_seconds <- as.numeric(clock_parts[2])
      }

      # Debug: log parsed values for live games
      if (is_in_progress) {
        # Calculate seconds for debugging
        if (period <= 4) {
          sec_elapsed <- (period - 1) * 720 + (720 - (clock_minutes * 60 + clock_seconds))
          sec_remaining <- 2880 - sec_elapsed
        } else {
          sec_elapsed <- 2880 + (period - 5) * 300 + (300 - (clock_minutes * 60 + clock_seconds))
          sec_remaining <- 2880 - sec_elapsed
        }
        message(sprintf("[ESPN] %s: Q%d %s (parsed: %d:%02d) -> elapsed=%.0f, remaining=%.0f, margin=%d",
                       home_team$team$abbreviation %||% "???",
                       period, clock_display, clock_minutes, clock_seconds,
                       sec_elapsed, sec_remaining, home_score - away_score))
      }
    }

    list(
      game_id = event$id,
      home_team = home_team$team$displayName,
      away_team = away_team$team$displayName,
      home_team_abbrev = home_team$team$abbreviation,
      away_team_abbrev = away_team$team$abbreviation,
      home_score = home_score,
      away_score = away_score,
      home_margin = home_score - away_score,
      period = period,
      clock_minutes = clock_minutes,
      clock_seconds = clock_seconds,
      clock_display = clock_display,
      status = status_type,
      status_detail = status$type$shortDetail %||% status$type$detail %||% "",
      is_in_progress = is_in_progress,
      is_final = is_final,
      is_scheduled = is_scheduled,
      start_time = event$date,
      venue = competition$venue$fullName %||% NA_character_,
      broadcast = if (length(competition$broadcasts) > 0) {
        competition$broadcasts[[1]]$names[[1]] %||% NA_character_
      } else NA_character_
    )
  }, error = function(e) {
    warning(sprintf("Failed to parse game: %s", e$message))
    NULL
  })
}

#' Convert ESPN game to game state format for KalshiPredictor
#' @param espn_game Game data from get_nba_scoreboard
#' @param home_net_rating Home team net rating (optional)
#' @param away_net_rating Away team net rating (optional)
#' @param home_rest_days Home team rest days (optional)
#' @param away_rest_days Away team rest days (optional)
#' @return Game state list compatible with KalshiPredictor
espn_to_game_state <- function(espn_game,
                                home_net_rating = 0,
                                away_net_rating = 0,
                                home_rest_days = 1,
                                away_rest_days = 1) {

  # Default game state for errors/invalid input
  default_state <- list(
    home_score = 0, away_score = 0, home_margin = 0,
    seconds_remaining = 2880, seconds_elapsed = 0, minutes_elapsed = 0,
    period = 1L, clock_minutes = 12, clock_seconds = 0,
    home_net_rating = 0, away_net_rating = 0, home_rating_advantage = 0,
    home_rest_days = 1, away_rest_days = 1, rest_advantage = 0, net_rating_advantage = 0
  )

  # Safety check - must be a list
  if (!is.list(espn_game)) {
    return(default_state)
  }

  # Wrap in tryCatch for any unexpected errors
  tryCatch({
    # Extract and coerce values with type safety
    period <- as.integer(espn_game$period %||% 1L)
    if (is.na(period) || period < 1) period <- 1L

    clock_minutes <- as.numeric(espn_game$clock_minutes %||% 12)
    if (is.na(clock_minutes)) clock_minutes <- 12

    clock_seconds <- as.numeric(espn_game$clock_seconds %||% 0)
    if (is.na(clock_seconds)) clock_seconds <- 0

    # Calculate seconds remaining
    # Each quarter is 12 minutes (720 seconds)
    # Overtime is 5 minutes (300 seconds)
    if (period <= 4) {
      # Regulation
      seconds_elapsed <- (period - 1) * 720 + (720 - (clock_minutes * 60 + clock_seconds))
      seconds_remaining <- 2880 - seconds_elapsed
    } else {
      # Overtime
      seconds_elapsed <- 2880 + (period - 5) * 300 + (300 - (clock_minutes * 60 + clock_seconds))
      seconds_remaining <- 2880 - seconds_elapsed  # Can be negative in OT
    }

    home_score <- as.numeric(espn_game$home_score %||% 0)
    if (is.na(home_score)) home_score <- 0

    away_score <- as.numeric(espn_game$away_score %||% 0)
    if (is.na(away_score)) away_score <- 0

    home_margin <- as.numeric(espn_game$home_margin %||% (home_score - away_score))
    if (is.na(home_margin)) home_margin <- home_score - away_score

    minutes_elapsed <- as.numeric(seconds_elapsed / 60)

    # Coerce rating inputs
    home_net_rating <- as.numeric(home_net_rating)
    if (is.na(home_net_rating)) home_net_rating <- 0

    away_net_rating <- as.numeric(away_net_rating)
    if (is.na(away_net_rating)) away_net_rating <- 0

    home_rest_days <- as.numeric(home_rest_days)
    if (is.na(home_rest_days)) home_rest_days <- 1

    away_rest_days <- as.numeric(away_rest_days)
    if (is.na(away_rest_days)) away_rest_days <- 1

    # Calculate rating/rest advantages based on who's leading
    if (home_margin > 0) {
      rest_advantage <- home_rest_days - away_rest_days
      net_rating_advantage <- home_net_rating - away_net_rating
    } else if (home_margin < 0) {
      rest_advantage <- away_rest_days - home_rest_days
      net_rating_advantage <- away_net_rating - home_net_rating
    } else {
      rest_advantage <- 0
      net_rating_advantage <- 0
    }

    list(
      home_score = home_score,
      away_score = away_score,
      home_margin = home_margin,
      seconds_remaining = seconds_remaining,
      seconds_elapsed = seconds_elapsed,
      minutes_elapsed = minutes_elapsed,
      period = period,
      clock_minutes = clock_minutes,
      clock_seconds = clock_seconds,
      home_net_rating = home_net_rating,
      away_net_rating = away_net_rating,
      home_rating_advantage = home_net_rating - away_net_rating,
      home_rest_days = home_rest_days,
      away_rest_days = away_rest_days,
      rest_advantage = rest_advantage,
      net_rating_advantage = net_rating_advantage
    )
  }, error = function(e) {
    # Return default state on any error
    default_state
  })
}

#' Get live game states for all in-progress games
#' @param date Date to fetch
#' @param team_ratings Optional tibble with team net ratings
#' @return List of game states keyed by game_id
get_live_game_states <- function(date = Sys.Date(), team_ratings = NULL) {
  scoreboard <- get_nba_scoreboard(date)

  # Safety checks
  if (!is.list(scoreboard) || !is.list(scoreboard$games) || length(scoreboard$games) == 0) {
    return(list())
  }

  # Filter to in-progress games (use isTRUE for safe boolean check)
  live_games <- scoreboard$games[sapply(scoreboard$games, function(g) {
    is.list(g) && isTRUE(g$is_in_progress)
  })]

  if (length(live_games) == 0) {
    return(list())
  }

  # Convert to game states
  game_states <- lapply(live_games, function(game) {
    tryCatch({
      # Look up team ratings if provided
      home_rating <- 0
      away_rating <- 0
      home_abbrev <- game$home_team_abbrev %||% "???"
      away_abbrev <- game$away_team_abbrev %||% "???"

      if (!is.null(team_ratings) && is.data.frame(team_ratings) && nrow(team_ratings) > 0) {
        if (!is.null(home_abbrev) && is.character(home_abbrev)) {
          home_row <- team_ratings[team_ratings$team_abbreviation == home_abbrev, , drop = FALSE]
          if (nrow(home_row) > 0) {
            home_rating <- home_row$net_rating[1]
            if (is.na(home_rating)) home_rating <- 0
          }
        }

        if (!is.null(away_abbrev) && is.character(away_abbrev)) {
          away_row <- team_ratings[team_ratings$team_abbreviation == away_abbrev, , drop = FALSE]
          if (nrow(away_row) > 0) {
            away_rating <- away_row$net_rating[1]
            if (is.na(away_rating)) away_rating <- 0
          }
        }

        message(sprintf("[RATINGS] %s vs %s: home_rating=%.1f, away_rating=%.1f, advantage=%.1f",
                       home_abbrev, away_abbrev, home_rating, away_rating, home_rating - away_rating))
      } else {
        message(sprintf("[RATINGS] %s vs %s: NO RATINGS AVAILABLE (using 0)", home_abbrev, away_abbrev))
      }

      state <- espn_to_game_state(
        game,
        home_net_rating = home_rating,
        away_net_rating = away_rating
      )

      # Add game metadata (with null checks)
      state$game_id <- game$game_id %||% "unknown"
      state$home_team <- game$home_team %||% "Home"
      state$away_team <- game$away_team %||% "Away"
      state$home_team_abbrev <- game$home_team_abbrev %||% "HOM"
      state$away_team_abbrev <- game$away_team_abbrev %||% "AWY"
      state$is_final <- isTRUE(game$is_final)
      state$status <- game$status %||% "UNKNOWN"

      state
    }, error = function(e) {
      # Return a minimal valid state on error
      list(
        game_id = game$game_id %||% "unknown",
        home_score = 0, away_score = 0, home_margin = 0,
        seconds_remaining = 2880, period = 1,
        home_team = "Home", away_team = "Away",
        is_final = FALSE, status = "ERROR"
      )
    })
  })

  # Key by game_id
  names(game_states) <- sapply(game_states, function(s) s$game_id)

  game_states
}

#' Get games that have ended (for settlement)
#' @param date Date to check
#' @return List of final games with winners
get_final_games <- function(date = Sys.Date()) {
  scoreboard <- get_nba_scoreboard(date)

  # Safety checks
  if (!is.list(scoreboard) || !is.list(scoreboard$games) || length(scoreboard$games) == 0) {
    return(list())
  }

  # Filter to final games (use isTRUE for safe boolean check)
  final_games <- scoreboard$games[sapply(scoreboard$games, function(g) {
    is.list(g) && isTRUE(g$is_final)
  })]

  if (length(final_games) == 0) {
    return(list())
  }

  # Add winner info
  lapply(final_games, function(game) {
    if (!is.list(game)) return(NULL)

    home_score <- game$home_score %||% 0
    away_score <- game$away_score %||% 0

    game$home_won <- home_score > away_score
    game$winner <- if (game$home_won) (game$home_team %||% "Home") else (game$away_team %||% "Away")
    game$winner_abbrev <- if (game$home_won) (game$home_team_abbrev %||% "HOM") else (game$away_team_abbrev %||% "AWY")
    game
  })
}

# ESPN Game Summary endpoint for play-by-play
ESPN_SUMMARY_URL <- "https://site.api.espn.com/apis/site/v2/sports/basketball/nba/summary"

#' Get recent scoring run differential from ESPN play-by-play
#' Calculates points scored by each team in the last N minutes
#' @param game_id ESPN game ID
#' @param minutes_back How many minutes of plays to analyze (default 5)
#' @return List with trailer_pts, leader_pts, and run_diff, or NULL on error
get_recent_scoring <- function(game_id, minutes_back = 5) {
  tryCatch({
    response <- request(ESPN_SUMMARY_URL) %>%
      req_url_query(event = game_id) %>%
      req_headers(
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
        "Accept" = "application/json"
      ) %>%
      req_perform() %>%
      resp_body_json()

    # Get current scores to determine who's leading
    boxscore <- response$boxscore
    if (is.null(boxscore)) return(NULL)

    teams <- boxscore$teams
    if (is.null(teams) || length(teams) < 2) return(NULL)

    # Find home and away teams
    home_team <- NULL
    away_team <- NULL
    for (team in teams) {
      if (isTRUE(team$homeAway == "home")) {
        home_team <- team
      } else {
        away_team <- team
      }
    }

    if (is.null(home_team) || is.null(away_team)) return(NULL)

    home_score <- as.numeric(home_team$score %||% 0)
    away_score <- as.numeric(away_team$score %||% 0)
    home_margin <- home_score - away_score
    home_leading <- home_margin > 0

    # Get play-by-play data
    plays <- response$plays
    if (is.null(plays) || length(plays) == 0) return(NULL)

    # Get current game clock info
    current_period <- response$header$competitions[[1]]$status$period %||% 1

    # Calculate cutoff time (minutes_back minutes ago)
    # Each period is 12 minutes (720 seconds), overtime is 5 minutes (300 seconds)

    # Parse recent plays and sum points
    home_pts_recent <- 0
    away_pts_recent <- 0
    plays_counted <- 0

    # Plays are typically in reverse chronological order (most recent first)
    # We need to look at the clock time to determine if play is within our window
    for (play in plays) {
      # Get play details
      play_period <- play$period$number %||% 1
      clock_text <- play$clock$displayValue %||% "0:00"

      # Parse clock (format: "5:32" or "0:45.3")
      clock_parts <- strsplit(gsub("\\..*", "", clock_text), ":")[[1]]
      if (length(clock_parts) == 2) {
        play_minutes <- as.numeric(clock_parts[1])
        play_seconds <- as.numeric(clock_parts[2])
      } else {
        next
      }

      # Calculate total seconds elapsed at this play
      if (play_period <= 4) {
        play_elapsed <- (play_period - 1) * 720 + (720 - (play_minutes * 60 + play_seconds))
      } else {
        play_elapsed <- 2880 + (play_period - 5) * 300 + (300 - (play_minutes * 60 + play_seconds))
      }

      # Calculate current elapsed time
      current_clock <- response$header$competitions[[1]]$status$displayClock %||% "0:00"
      current_parts <- strsplit(gsub("\\..*", "", current_clock), ":")[[1]]
      if (length(current_parts) == 2) {
        current_minutes <- as.numeric(current_parts[1])
        current_seconds <- as.numeric(current_parts[2])
      } else {
        current_minutes <- 0
        current_seconds <- 0
      }

      if (current_period <= 4) {
        current_elapsed <- (current_period - 1) * 720 + (720 - (current_minutes * 60 + current_seconds))
      } else {
        current_elapsed <- 2880 + (current_period - 5) * 300 + (300 - (current_minutes * 60 + current_seconds))
      }

      # Check if play is within our time window
      seconds_ago <- current_elapsed - play_elapsed
      if (seconds_ago < 0 || seconds_ago > (minutes_back * 60)) {
        next  # Play is outside our window
      }

      # Check if this play has scoring
      score_value <- play$scoreValue %||% 0
      if (score_value > 0) {
        # Determine which team scored
        team_id <- play$team$id %||% ""
        home_team_id <- home_team$team$id %||% ""

        if (team_id == home_team_id) {
          home_pts_recent <- home_pts_recent + score_value
        } else {
          away_pts_recent <- away_pts_recent + score_value
        }
        plays_counted <- plays_counted + 1
      }
    }

    # Determine trailer/leader points based on current score
    if (home_leading) {
      trailer_pts <- away_pts_recent
      leader_pts <- home_pts_recent
    } else if (home_margin < 0) {
      trailer_pts <- home_pts_recent
      leader_pts <- away_pts_recent
    } else {
      # Tied - just return both
      trailer_pts <- away_pts_recent
      leader_pts <- home_pts_recent
    }

    run_diff <- trailer_pts - leader_pts

    list(
      trailer_pts = trailer_pts,
      leader_pts = leader_pts,
      run_diff = run_diff,
      home_pts_recent = home_pts_recent,
      away_pts_recent = away_pts_recent,
      plays_counted = plays_counted,
      minutes_analyzed = minutes_back
    )

  }, error = function(e) {
    # Silently return NULL on any error
    NULL
  })
}

#' Format game status for display
#' @param game ESPN game object
#' @return Formatted status string
format_game_status <- function(game) {
  # Handle scheduled games
 if (isTRUE(game$is_scheduled)) {
    # Parse and format start time
    start_dt <- tryCatch({
      as.POSIXct(game$start_time, format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
    }, error = function(e) {
      # Try alternative format
      as.POSIXct(game$start_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    })
    if (!is.na(start_dt)) {
      start_local <- format(start_dt, "%I:%M %p", tz = Sys.timezone())
      return(start_local)
    }
    return("Scheduled")
  }

  # Handle final games
  if (isTRUE(game$is_final)) {
    return("FINAL")
  }

  # Handle in-progress games (including halftime, end of period, etc.)
  if (isTRUE(game$is_in_progress)) {
    # Check for special statuses
    status <- game$status %||% ""

    if (grepl("HALFTIME", status, ignore.case = TRUE)) {
      return("HALFTIME")
    }

    if (grepl("END", status, ignore.case = TRUE)) {
      period_str <- if (game$period <= 4) {
        paste0("End Q", game$period)
      } else {
        paste0("End OT", game$period - 4)
      }
      return(period_str)
    }

    # Regular in-progress
    period <- game$period %||% 1
    period_str <- if (period <= 4) {
      paste0("Q", period)
    } else {
      paste0("OT", period - 4)
    }

    clock_min <- game$clock_minutes %||% 0
    clock_sec <- game$clock_seconds %||% 0
    clock_str <- sprintf("%d:%02d", clock_min, clock_sec)

    return(paste(period_str, clock_str))
  }

  # Try status_detail from ESPN (e.g., "Q3 5:32", "Halftime", "Final")
  status_detail <- game$status_detail %||% ""
  if (nchar(status_detail) > 0) {
    return(status_detail)
  }

  # Fallback - try to extract info from status field
  status <- game$status %||% ""
  if (nchar(status) > 0 && status != "UNKNOWN") {
    # Clean up status for display
    clean_status <- gsub("STATUS_", "", status)
    clean_status <- gsub("_", " ", clean_status)
    return(tools::toTitleCase(tolower(clean_status)))
  }

  # Last resort
  "TBD"
}
