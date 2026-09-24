# NBA Live Betting — Kalshi Event-Contract Trading System

A live, in-game trading system for NBA win/loss event contracts on
[Kalshi](https://kalshi.com), combining a calibrated win-probability model
with market-based signal generation, backtested and evaluated on live
market and play-by-play data.

## Strategy overview

Two complementary, independently-gated strategies:

- **WITH LEADER** — bets that a team with a large lead maintains it. Enters
  when a team leads by 15+ points in Q3/Q4, or by 21+ points at any point in
  the game (at most one position per game). This strategy leans on the
  empirical fact that large leads are blown far less often than naive
  intuition suggests — the `get_actual_blown_rate()` lookup table (built
  from 5,888 games, 2022–2026) shows, for example, a **1.3% blown-lead
  rate** for a 15+ point Q4 lead.
- **FADE V2** — a contrarian, market-based strategy that bets on the
  trailing team specifically when the market is already pricing in
  meaningful uncertainty (trailer priced 35–45¢) and the actual lead is 7+
  points. Rather than fighting the market with a model-implied edge, this
  strategy aligns with the market's own signal: when the market prices a
  trailing team at 35¢+, that team's actual historical win rate is close to
  the market's own read (~53%). Up to 3 fade positions per game, no
  hedging — an earlier hedged variant was tested and found to destroy
  value, so it was removed.

Both strategies skip the final minute of regulation and overtime
(`MIN_SECONDS_FOR_SIGNAL`), to avoid signaling into end-game noise.

## Modeling components

- **Win probability model** (`win_probability_model.R`) — a logistic
  regression trained on ~10 years (2015–2024) of NBA play-by-play data
  (score changes, quarter ends, and sampled time buckets), predicting home
  win probability from score margin, time remaining, period, each team's
  net rating advantage, and a margin × time-remaining interaction term
  (since a given point margin matters very differently with 10 minutes left
  vs. 30 seconds left).
- **Blown-lead model** (`nba_largest_lead.R`) — a separate logistic
  regression (ROC-AUC **0.924**) predicting the probability that a team
  holding the largest lead in a game ultimately loses, as a function of lead
  size, elapsed time, home/away status, rest advantage, and net rating. This
  directly powers the WITH LEADER strategy's edge estimate.
- **Model/market calibration blending** (`calibrate_blown_prob()`) — rather
  than trusting the model or the market price alone, predictions are
  shrunk toward the market price with a confidence weight (`alpha`) that
  varies by how extreme the market price already is: `alpha = 0.3` when the
  market price is under 18¢ (trust the market), rising to `alpha = 0.8`
  above 35¢ (trust the model more, since the market is less informative in
  that range). A key empirical finding baked into the system: **when the
  model disagrees sharply with the market, the model is usually wrong**
  (selection bias in which situations reach a large model/market gap) —
  so the system is deliberately designed to defer to market pricing rather
  than chase its own biggest apparent edges.
- **In-game price-velocity adjustments** — trailing-team fade entries are
  boosted when the market price is falling fast (≥4.5¢/min, "buying the
  dip") and blocked entirely when it's spiking (≥6¢/min, likely a sharp
  reversal already underway). A run-differential signal was also tested and
  found to have no predictive value, so it's implemented but disabled by
  default.

## Implementation

`kalshi_predictor.R` implements the live decision logic as an R6 class,
`KalshiPredictor`, with methods to generate trading signals
(`get_signal()`), validate fade eligibility under both a market-based and a
model-calibrated mode (`check_fade_eligible()`), and record/settle
positions. Bet sizing follows a tiered framework (2–5% of bankroll per
position) based on signal strength — e.g. a Q4 lead of 20+ points sizes at
the maximum tier, a borderline 35–40¢ fade at the minimum.

## Backtesting & evaluation

- `fetch_kalshi_full_data.R` — pulls historical Kalshi candlestick/price
  data for settled NBA game markets.
- `backtest_ingame_adjustments_2026.R` / `backtest_with_spread_2026.R` —
  backtest the FADE V2 strategy (with and without the price-velocity
  adjustments described above) against January 2026 market and play-by-play
  data, simulating realistic bid-ask spread and Kalshi's 7% fee on winning
  positions.
- `evaluate_predictor_2026.R` / `analyze_ingame_thresholds.R` — evaluate the
  full `KalshiPredictor` signal generation (both strategies) against
  realized outcomes, broken out by period, lead-size bucket, and model
  calibration (predicted vs. actual comeback rate).
- `wp_chart_app.R` — a Shiny app rendering live win-probability charts for
  any date's games, useful for visually auditing the win-probability model
  against how a game actually played out.

Backtests over this system's January 2026 evaluation window showed the
WITH LEADER strategy winning the large majority of entries with a modest
per-trade edge, and the market-based FADE V2 variant outperforming an
earlier, purely model-based fade approach — reinforcing the
calibration-toward-market design choice above. Exact figures vary by
strategy variant and are broken out in the evaluation scripts' output
rather than a single headline number; see those scripts for the specific
trade counts, win rates, and P&L by configuration.

## Ancillary analysis

`clutch_choke_analysis.md` — a standalone study (2002–2024, ESPN/NBA API
play-by-play) using the same blown-lead model to rank NBA franchises by how
much they over- or under-perform the model's expectation in games where
they held the largest lead — identifying statistically "clutch" and
"choke" teams over two decades, independent of the live trading system
above.

## Repo structure

```
nba-live-betting/
├── kalshi_predictor.R                    # KalshiPredictor R6 class: live signal generation
├── win_probability_model.R               # logistic regression win-probability model
├── nba_largest_lead.R                    # blown-lead probability model (ROC-AUC 0.924)
├── fetch_kalshi_full_data.R              # Kalshi historical market data pull
├── backtest_ingame_adjustments_2026.R    # FADE V2 backtest, with velocity adjustments
├── backtest_with_spread_2026.R           # FADE V2 backtest, realistic spread simulation
├── evaluate_predictor_2026.R             # full predictor evaluation vs. realized outcomes
├── analyze_ingame_thresholds.R           # threshold sensitivity analysis
├── wp_chart_app.R                        # Shiny app: live win-probability charts
├── clutch_choke_analysis.md              # standalone clutch/choke franchise study
├── *.rds                                 # fitted models and cached datasets (see below)
└── README.md
```

The `.rds` files are R's native serialized-object format — cached fitted
models (`win_probability_model.rds`, `blown_lead_model_v2.rds`) and pulled
datasets (`kalshi_nba_full_data.rds`, `backtest_ingame_results_2026.rds`,
`ingame_threshold_analysis.rds`), included so the evaluation/backtest
scripts can be re-run without re-fetching or re-fitting from scratch.

## Data sources

- **Kalshi API** — NBA game-outcome market prices (candlestick data), no
  authentication required for the read endpoints used here.
- **NBA play-by-play, schedule, and box score data** — via the `hoopR` and
  `nbastatR` R packages.

## Running this project

Requires R with `tidyverse`, `R6`, `tidymodels` (for the logistic
regression workflows), `hoopR`, `nbastatR`, and `shiny` (for
`wp_chart_app.R`). No API credentials are required for the Kalshi or
NBA data endpoints used. Fit or load the win-probability and blown-lead
models first, then run the backtest/evaluation scripts, which expect the
cached `.rds` datasets described above to be present in the repo root.

## Status

This is a paper-trading / backtesting system — it has not been connected
to live capital execution on Kalshi.
