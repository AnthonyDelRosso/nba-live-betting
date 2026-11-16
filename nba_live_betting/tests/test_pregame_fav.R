# Parity + settlement tests for R/pregame_fav_strategy.R against the
# backtest data it was frozen from (analyze_favorite_deep.R).
# Run from nba_live_betting/:  Rscript tests/test_pregame_fav.R
suppressMessages({ library(tidyverse); library(R6); library(httr2); library(jsonlite) })
source("R/trading_state.R"); source("R/kalshi_api.R"); source("R/paper_trading.R")
source("R/pregame_fav_strategy.R")
tmp_log <- tempfile(fileext = ".rds")
fails <- 0
check <- function(ok, msg) { cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) fails <<- fails + 1 }

cal <- readRDS("calibration_v2_KXNBASPREAD.rds") %>%
  filter(game_date >= "2026-10-20", snapshot_method == "pregame_fixed", !is.na(snapshot_mid)) %>%
  mutate(team = str_extract(ticker, "(?<=-)[A-Z]+(?=\\d+$)"), bid = 2 * snapshot_mid - snapshot_price)
deep <- readRDS("favorite_deep_results.rds")

# Expected decisions from the backtest: big fav (implied >= 8), leg closest to 70c
exp_legs <- deep$fav %>% filter(implied_median >= 8) %>% group_by(event_ticker) %>%
  slice(which.min(abs(snapshot_price - 0.70))) %>% ungroup() %>%
  select(event_ticker, exp_ticker = ticker, exp_ask = snapshot_price)

mk_markets <- function(ev) {
  cal %>% filter(event_ticker == ev) %>% pmap(function(ticker, threshold, team, bid, snapshot_price, ...) list(
    ticker = ticker, status = "active", title = sprintf("%s wins by over %.1f points?", team, threshold),
    yes_bid_dollars = sprintf("%.4f", bid), yes_ask_dollars = sprintf("%.4f", snapshot_price),
    yes_ask_size_fp = "100.00", volume_fp = "1000"))
}
results_store <- list()
mock_client <- list(
  public_request = function(endpoint) list(markets = mk_markets(current_ev)),
  get_market = function(ticker) list(market = list(ticker = ticker, result = results_store[[ticker]] %||% ""))
)

state <- TradingState$new(mode = "paper", paper_balance = 100000)
strat <- PregameFavStrategy$new(state, mock_client, contracts = 10, log_path = tmp_log)
start_str <- format(Sys.time() + 55 * 60, "%Y-%m-%dT%H:%MZ", tz = "UTC")

events <- unique(cal$event_ticker)
for (i in seq_along(events)) {
  current_ev <- events[i]
  row1 <- cal %>% filter(event_ticker == current_ev) %>% slice(1)
  strat$evaluate_game(list(game_id = sprintf("G%04d", i), is_scheduled = TRUE, start_time = start_str,
                           away_team_abbrev = row1$away_team, home_team_abbrev = row1$home_team))
  strat$decisions$event_ticker[nrow(strat$decisions)] <- current_ev  # map back (fake tip date)
}
d <- strat$decisions
cmp <- full_join(d %>% filter(decision == "entered") %>% select(event_ticker, leg_ticker, leg_ask),
                 exp_legs, by = "event_ticker")
cat(sprintf("Games evaluated: %d | entered: %d | backtest big-fav games: %d\n",
            nrow(d), sum(d$decision == "entered"), nrow(exp_legs)))
print(count(d, decision))
check(nrow(cmp) == nrow(exp_legs) && all(!is.na(cmp$leg_ticker)) && all(!is.na(cmp$exp_ticker)),
      "same set of games entered as backtest rule")
mism <- cmp %>% filter(leg_ticker != exp_ticker)
check(all(abs(mism$leg_ask - 0.70) == abs(mism$exp_ask - 0.70)),
      sprintf("same leg chosen (%d differ, all exact ties at equal distance from 70c)", nrow(mism)))

imp_cmp <- d %>% filter(!is.na(implied_median)) %>%
  inner_join(deep$implied %>% select(event_ticker, exp_imp = implied_median), by = "event_ticker")
check(nrow(imp_cmp) > 90 && max(abs(imp_cmp$implied_median - imp_cmp$exp_imp)) < 1e-9,
      sprintf("implied median matches backtest on %d games", nrow(imp_cmp)))

# Re-evaluation is a no-op (one decision per game)
n_before <- nrow(strat$decisions); current_ev <- events[1]
strat$evaluate_game(list(game_id = "G0001", is_scheduled = TRUE, start_time = start_str,
                         away_team_abbrev = "X", home_team_abbrev = "Y"))
check(nrow(strat$decisions) == n_before, "game is evaluated only once")

# Timing window
s2 <- PregameFavStrategy$new(TradingState$new(), mock_client, log_path = tempfile(fileext = ".rds"))
s2$evaluate_game(list(game_id = "early", is_scheduled = TRUE, start_time = format(Sys.time() + 90*60, "%Y-%m-%dT%H:%MZ", tz = "UTC")))
s2$evaluate_game(list(game_id = "late", is_scheduled = TRUE, start_time = format(Sys.time() + 20*60, "%Y-%m-%dT%H:%MZ", tz = "UTC"),
                      away_team_abbrev = "A", home_team_abbrev = "B"))
check(!"early" %in% s2$decisions$game_id && s2$decisions$decision[s2$decisions$game_id == "late"] == "missed_window",
      "too-early skipped silently; inside 45 min logged as missed_window")

# Event ticker date uses ET: 00:30Z on Nov 12 is 7:30pm ET Nov 11
check(toupper(format(as.POSIXct("2026-11-12 00:30", tz = "UTC"), "%y%b%d", tz = "America/New_York")) == "26NOV11",
      "event ticker date uses US/Eastern game date")

# Settlement: spread result, not moneyline; and paper engine's moneyline settle skips it
entered <- d %>% filter(decision == "entered")
truth <- readRDS("calibration_v2_KXNBASPREAD.rds") %>% select(ticker, result)
for (tk in entered$leg_ticker) results_store[[tk]] <- truth$result[truth$ticker == tk]
eng <- PaperTradingEngine$new(state = state)
eng$settle_game(entered$game_id[1], home_won = TRUE)
check(state$positions$status[state$positions$position_id == entered$position_id[1]] == "open",
      "paper engine moneyline settlement leaves spread position open")
cash0 <- state$cash_balance
strat$check_settlements()
d2 <- strat$decisions %>% filter(decision == "entered")
check(all(!is.na(d2$result)) && all(state$positions$status != "open"), "all positions settled from market result")
exp_pnl <- deep$fav %>% semi_join(tibble(ticker = d2$leg_ticker), by = "ticker") %>% summarise(sum(pnl_repo)) %>% pull()
check(abs(sum(d2$pnl) / 10 - exp_pnl) < 1e-6,
      sprintf("P&L matches backtest (per contract: $%.2f vs $%.2f)", sum(d2$pnl) / 10, exp_pnl))
print(strat$summary())

# Fix 1: polled in window but no ladder ever listed -> no_market_by_window (not missed_window)
empty_client <- list(public_request = function(endpoint) list(markets = list()),
                     get_market = function(ticker) list(market = list()))
s3 <- PregameFavStrategy$new(TradingState$new(mode = "paper"), empty_client, log_path = tempfile(fileext = ".rds"))
s3$evaluate_game(list(game_id = "nomkt", is_scheduled = TRUE, away_team_abbrev = "A", home_team_abbrev = "B",
                      start_time = format(Sys.time() + 50*60, "%Y-%m-%dT%H:%MZ", tz = "UTC")))
check(nrow(s3$decisions) == 0, "no ladder in window: not recorded yet (retries)")
s3$evaluate_game(list(game_id = "nomkt", is_scheduled = TRUE, away_team_abbrev = "A", home_team_abbrev = "B",
                      start_time = format(Sys.time() + 40*60, "%Y-%m-%dT%H:%MZ", tz = "UTC")))
check(identical(s3$decisions$decision, "no_market_by_window"), "no ladder by T-45 logged as no_market_by_window")

# Fix 3: voided market -> refund at cost, P&L 0, position closed
st4 <- TradingState$new(mode = "paper", paper_balance = 1000)
void_store <- list(result = "", status = "finalized")
mc4 <- mock_client; mc4$get_market <- function(ticker) list(market = c(list(ticker = ticker), void_store))
current_ev <- entered$event_ticker[1]
s4 <- PregameFavStrategy$new(st4, mc4, log_path = tempfile(fileext = ".rds"))
row1 <- cal %>% filter(event_ticker == current_ev) %>% slice(1)
s4$evaluate_game(list(game_id = "V1", is_scheduled = TRUE, start_time = start_str,
                      away_team_abbrev = row1$away_team, home_team_abbrev = row1$home_team))
cash_after_entry <- st4$cash_balance
void_store$status <- "active"; s4$check_settlements()
check(st4$positions$status[1] == "open", "unresolved active market stays open")
void_store$status <- "finalized"; s4$check_settlements()
check(st4$positions$status[1] == "void" && abs(st4$cash_balance - 1000) < 1e-9 &&
        s4$decisions$result[1] == "void" && s4$decisions$pnl[1] == 0 && cash_after_entry < 1000,
      "finalized market with no yes/no result is voided and refunded at cost")
check(s4$summary()$voided == 1 && s4$summary()$settled == 0, "void excluded from settled scorecard")

cat(if (fails == 0) "\nALL TESTS PASSED\n" else sprintf("\n%d TEST(S) FAILED\n", fails))
