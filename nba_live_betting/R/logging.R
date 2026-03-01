# Logging utilities for NBA Live Betting Platform

library(R6)

#' Logger R6 Class
#' Provides structured logging to file and console
Logger <- R6::R6Class(
  "Logger",

  public = list(
    log_dir = NULL,
    log_file = NULL,
    log_level = NULL,

    #' Initialize the logger
    #' @param log_dir Directory for log files
    #' @param level Minimum log level: "DEBUG", "INFO", "WARN", "ERROR"
    initialize = function(log_dir = "logs", level = "INFO") {
      self$log_dir <- log_dir
      self$log_level <- level

      # Create log directory if it doesn't exist
      if (!dir.exists(log_dir)) {
        dir.create(log_dir, recursive = TRUE)
      }

      # Set log file path with date
      self$log_file <- file.path(
        log_dir,
        paste0("trading_", format(Sys.Date(), "%Y-%m-%d"), ".log")
      )
    },

    #' Get numeric level for comparison
    level_num = function(level) {
      levels <- c("DEBUG" = 1, "INFO" = 2, "WARN" = 3, "ERROR" = 4)
      levels[level]
    },

    #' Write log entry
    #' @param level Log level
    #' @param message Log message
    #' @param ... Additional named values to include
    log = function(level, message, ...) {
      # Skip if below minimum level
      if (self$level_num(level) < self$level_num(self$log_level)) {
        return(invisible(NULL))
      }

      # Build log entry
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      extra <- list(...)

      # Format extra data
      extra_str <- ""
      if (length(extra) > 0) {
        extra_parts <- sapply(names(extra), function(n) {
          sprintf("%s=%s", n, as.character(extra[[n]]))
        })
        extra_str <- paste0(" | ", paste(extra_parts, collapse = ", "))
      }

      # Build log line
      log_line <- sprintf("[%s] [%s] %s%s", timestamp, level, message, extra_str)

      # Write to console
      cat(log_line, "\n")

      # Write to file
      tryCatch({
        cat(log_line, "\n", file = self$log_file, append = TRUE)
      }, error = function(e) {
        # Silently fail file logging
      })

      invisible(log_line)
    },

    #' Debug level log
    debug = function(message, ...) {
      self$log("DEBUG", message, ...)
    },

    #' Info level log
    info = function(message, ...) {
      self$log("INFO", message, ...)
    },

    #' Warning level log
    warn = function(message, ...) {
      self$log("WARN", message, ...)
    },

    #' Error level log
    error = function(message, ...) {
      self$log("ERROR", message, ...)
    },

    #' Log a trade event
    log_trade = function(game_id, strategy, signal, price, size, ...) {
      self$info(
        sprintf("TRADE: %s %s", strategy, signal),
        game_id = game_id,
        price = round(price, 3),
        size = size,
        ...
      )
    },

    #' Log a settlement event
    log_settlement = function(game_id, position_id, result, pnl, ...) {
      self$info(
        sprintf("SETTLEMENT: %s %s", position_id, result),
        game_id = game_id,
        pnl = round(pnl, 2),
        ...
      )
    },

    #' Get recent log entries
    #' @param n Number of entries to return
    get_recent = function(n = 50) {
      if (!file.exists(self$log_file)) {
        return(character(0))
      }

      lines <- readLines(self$log_file, warn = FALSE)
      tail(lines, n)
    }
  )
)

#' Create a global logger instance
#' @param log_dir Directory for log files
#' @param level Minimum log level
create_logger <- function(log_dir = "logs", level = "INFO") {
  Logger$new(log_dir = log_dir, level = level)
}
