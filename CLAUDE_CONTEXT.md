# Kalshi NBA Live Betting System - Context Document

Use this document to initialize Claude sessions with full project context.

---

## Project Overview

A live NBA betting system for Kalshi prediction markets. Combines win probability models with market data to identify trading opportunities on game outcomes.

**Tech Stack**: R, Shiny, R6 classes, hoopR package, ESPN API

---

## Directory Structure

```
testing/
├── kalshi_predictor.R          # Core prediction engine (R6 class)
├── win_probability_model.R     # Baseline WP model
├── nba_largest_lead.R          # Blown lead model (v2)
├── blown_lead_model_v2.rds     # Trained blown lead model
├── kalshi_nba_full_data.rds    # Historical Kalshi market data
├── nba_live_betting/           # Shiny app
│   ├── app.R                   # Main app server/UI
│   ├── global.R                # Shared setup, sources predictor
│   ├── R/
│   │   ├── kalshi_api.R        # Kalshi API wrapper
│   │   ├── espn_api.R          # ESPN scoreboard + play-by-play
│   │   ├── paper_trading.R     # Paper trading engine
│   │   ├── automated_trader.R  # AutomatedTrader R6 class
│   │   ├── trading_state.R     # TradingState R6 class
│   │   └── logging.R           # Trade logging utilities
│   └── modules/
│       ├── mod_live_games.R    # Live games dashboard
│       ├── mod_positions.R     # Position management
│       ├── mod_trade_log.R     # Trade history
│       ├── mod_performance.R   # P&L tracking
│       └── mod_controls.R      # Trading controls
├── backtest_*.R                # Backtest scripts
└── analyze_ingame_thresholds.R # Empirical threshold analysis
```

---

## Trading Strategies

### Strategy 1: WITH LEADER
Bet on leading team to hold their lead.

| Parameter | Value |
|-----------|-------|
| Entry | Q3/Q4 @ 15+ pts OR any period @ 21+ pts |
| Exit | Hold to settlement (no stop-loss) |
| Max positions | 1 per game |
| Backtest | 95.4% win rate, +$31 P&L, 0.48 Sharpe |

### Strategy 2: FADE V2 (Market-Based, Period-Filtered)
Bet on trailing team when market shows uncertainty. **NO HEDGING** - backtests showed hedging destroys value.

| Parameter | Value |
|-----------|-------|
| Entry | 7+ pt lead AND trailer price **35c-45c** |
| Exit | Hold to settlement |
| Max positions | 3 per game |
| Min between entries | 2 minutes |

**Period Filters** (based on backtest analysis):

| Price Tier | Allowed Periods | Bet Size | Rationale |
|------------|-----------------|----------|-----------|
| 35-40c | All (Q1-Q4) | 3.0% | Net profitable across all periods |
| 40-45c | **Q3/Q4 only** | 2.0% | Q1/Q2 loses (-$840), Q3/Q4 wins (+$529) |

**Backtest by Period (40-45c tier):**
| Period | Trades | Win Rate | P&L | vs Breakeven (47%) |
|--------|--------|----------|-----|-------------------|
| Q1 | 89 | 41.6% | -$369 | ❌ -5.4% below |
| Q2 | 61 | 37.7% | -$472 | ❌ -9.3% below |
| Q3 | 36 | **61.1%** | +$437 | ✅ +14.1% above |
| Q4 | 4 | **75.0%** | +$92 | ✅ +28.0% above |

**Why the period filter works:**
- Early game (Q1/Q2): Leads can easily be overcome, market skepticism is warranted
- Late game (Q3/Q4): Less time to recover, fading at 40-45c has real edge
- Simulated filter: 142 trades, 47.2% WR, **+$555 P&L** (vs -$286 unfiltered)

---

## In-Game Adjustments (Empirically Validated)

### Price Velocity (ENABLED - contrarian/mean-reversion)
| Condition | Adjustment |
|-----------|------------|
| Price drops >4.5c/min | +20% boost (buy the dip) |
| Price spikes >4.5c/min | -20% penalty |
| Sharp spike >6c/min | **BLOCKS ENTRY** |

**Empirical finding**: At 4.5c/min threshold, dips show **50% higher win rate** than spikes.
Threshold determined by grid search in `analyze_ingame_thresholds.R`.

### Run Differential (DISABLED)
Analysis found **no predictive signal**. Code retained but disabled (`USE_RUN_DIFFERENTIAL = FALSE`).

### Bet Sizing Tiers

**With Leader:**
| Tier | Size | Condition |
|------|------|-----------|
| TIER 1 (MAX) | 5% | Q4 + 20+ pts |
| TIER 2 (LARGE) | 4% | Q4 + 15+ pts OR any 21+ pts |
| TIER 3 (MEDIUM) | 3% | Standard entry |

**Fade V2:**
| Tier | Size | Periods | Condition |
|------|------|---------|-----------|
| 35-40c | 3% | All | Best risk/reward |
| 40-45c | 2% | **Q3/Q4 only** | Period filter required |

---

## Model Calibration

The blown lead model overestimates by ~11% on average. Calibration blends model with market:

```r
calibrated_prob <- alpha * model_prob + (1 - alpha) * market_price
```

| Market Price | Alpha | Reasoning |
|--------------|-------|-----------|
| < 18c | 0.3 | Trust market (model overconfident) |
| 18-35c | 0.5 | Balanced blend |
| ≥ 35c | 0.8 | Trust model (accurate here) |
| Q4 | ×0.1 | Leads rarely blown in Q4 |

---

## Data Sources

### Kalshi Market Data (`kalshi_nba_full_data.rds`)
- ~140,879 observations
- Columns: `timestamp`, `price`, `ticker`, `game_date`, `home_team`, `away_team`, `market_team`, `result`
- Collected by polling Kalshi API during live games

### hoopR Package
- `load_nba_schedule()` - Game schedule with final scores
- `load_nba_pbp()` - Play-by-play with timestamps
- `load_nba_team_box()` - Team box scores for net ratings

### ESPN API (`espn_api.R`)
- `get_live_scoreboard()` - Current scores, game status
- `get_recent_scoring()` - Play-by-play for run differential

---

## Backtesting Methodology

### Data Join
Kalshi market timestamps joined to hoopR play-by-play by **clock time** (not score):

```r
diffs <- abs(difftime(game_pbp$pbp_timestamp, kalshi_ts, units = "secs"))
idx <- which.min(diffs)
if (as.numeric(diffs[idx]) > 600) next  # Skip if >10 min apart
```

Hybrid join (time + game state) tested but only corrected 1.3% of joins - original time-only join is reliable.

### Spread Simulation
```r
BASE_SPREAD <- 0.02  # 2 cents
# Widens in Q4 crunch time, close games
# Tightens for blowouts
```

### Key Backtest Files
| File | Purpose |
|------|---------|
| `backtest_with_spread_2026.R` | Main backtest with spread simulation |
| `backtest_ingame_adjustments_2026.R` | Tests velocity/run diff impact |
| `evaluate_predictor_2026.R` | Basic predictor evaluation |

---

## Shiny App Architecture

### Initialization Flow
1. `global.R` sources `kalshi_predictor.R` and creates shared predictor
2. `app.R` initializes `TradingState` and `AutomatedTrader`
3. `reactiveTimer(5000)` triggers polling loop
4. Each poll: fetch ESPN data → update game states → check signals → execute trades

### Key R6 Classes
| Class | File | Purpose |
|-------|------|---------|
| `KalshiPredictor` | `kalshi_predictor.R` | Signal generation |
| `TradingState` | `trading_state.R` | Bankroll, positions, P&L |
| `AutomatedTrader` | `automated_trader.R` | Trade execution loop |

### Common Issues
- **Reactive context errors**: Use `shiny::` prefix for `reactiveTimer`, `observe`, etc.
- **req() guards**: Always check reactive values exist before using

---

## Team Abbreviation Mapping

```r
KALSHI_TO_HOOPR <- c(
  "MIN" = "MIN", "DAL" = "DAL", "GSW" = "GS", "UTA" = "UTAH",
  "SAS" = "SA", "HOU" = "HOU", "CHA" = "CHA", "MEM" = "MEM",
  "NYK" = "NY", "TOR" = "TOR", "ATL" = "ATL", "BOS" = "BOS",
  "LAL" = "LAL", "CLE" = "CLE", "ORL" = "ORL", "MIA" = "MIA",
  "CHI" = "CHI", "IND" = "IND", "LAC" = "LAC", "DEN" = "DEN",
  "PHX" = "PHX", "POR" = "POR", "SAC" = "SAC", "OKC" = "OKC",
  "NOP" = "NO", "MIL" = "MIL", "DET" = "DET", "WAS" = "WSH",
  "PHI" = "PHI", "BKN" = "BKN"
)
```

---

## Historical Blown Lead Rates

From 2022-2026 seasons (5,888 games):

**Q3 Examples**:
| Lead | Blown Rate |
|------|------------|
| 25+ | 0.3% |
| 21-24 | 2.4% |
| 15-17 | 7.6% |
| 10-11 | 19.1% |
| 7-9 | 25.5% |

**Q4 Examples**:
| Lead | Blown Rate |
|------|------------|
| 25+ | 0.0% |
| 21-24 | 0.1% |
| 15-17 | 1.3% |
| 10-11 | 5.8% |
| 7-9 | 11.4% |

---

## Quick Reference - Running the System

### Start Shiny App
```r
shiny::runApp("nba_live_betting")
```

### Run Backtest
```r
source("backtest_with_spread_2026.R")
```

### Test Predictor Manually
```r
source("kalshi_predictor.R")
predictor <- KalshiPredictor$new()
predictor$print_config()

game_state <- create_game_state(
  home_score = 75, away_score = 63,
  seconds_remaining = 720, period = 3
)
signal <- predictor$get_signal("test_game", game_state, market_price = 0.88)
```

---

## Known Limitations

1. **Velocity data coverage limited** - Only ~50% of trades have 5-minute velocity lookback; empirical thresholds validated but sample size modest
2. **Spread simulation is approximate** - Real Kalshi spreads vary by liquidity
3. **No live Kalshi API integration yet** - App uses paper trading
4. **Single game type** - Only NBA regular season tested
5. **Backtest period** - Jan 2026 data only; may need validation on larger sample

---

## Backtest Results Summary (Jan 2026)

**Baseline (no period filter):**

| Strategy | Trades | Win Rate | P&L |
|----------|--------|----------|-----|
| With Leader | ~65 | 95.4% | +$31 |
| Fade V2 (35-45c) | 292 | 43.5% | -$286 |

**With period filter (40-45c Q3/Q4 only):**

Simulated: 142 trades, 47.2% WR, **+$555 P&L**

**Breakdown by tier and period:**
| Tier | Period | Trades | Win Rate | P&L | Action |
|------|--------|--------|----------|-----|--------|
| 35-40c | All | 102 | 41.2% | +$26 | ✅ Keep |
| 40-45c | Q1 | 89 | 41.6% | -$369 | ❌ Skip |
| 40-45c | Q2 | 61 | 37.7% | -$472 | ❌ Skip |
| 40-45c | Q3 | 36 | **61.1%** | +$437 | ✅ Keep |
| 40-45c | Q4 | 4 | **75.0%** | +$92 | ✅ Keep |

**Why the period filter works:**
- 40-45c breakeven is ~47% after spread
- Q1/Q2: 38-42% WR → below breakeven → loses money
- Q3/Q4: 61-75% WR → well above breakeven → profitable
- Early game leads can be overcome; late game leads are safer to fade

**Velocity adjustments validated:**
- Price dips: 5-6 trades, 67-80% WR, +$100-160 P&L
- Velocity signal provides edge when triggered

---

## Session Checklist

When starting a new session:
1. Confirm working directory: `/mnt/c/Users/Anthony/OneDrive/Documents/High Dimensional Data Analytics/testing`
2. Key files exist: `kalshi_predictor.R`, `kalshi_nba_full_data.rds`, `blown_lead_model_v2.rds`
3. hoopR data loads: `load_nba_schedule(seasons = 2026)`
4. Ask user's current focus: backtesting, app development, or strategy refinement
