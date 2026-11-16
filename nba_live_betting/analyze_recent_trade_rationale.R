args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) setwd(args[1])
library(tidyverse)

state <- readRDS("data/trading_state.rds")
positions <- state$positions
trade_log <- state$trade_log

cat(sprintf("Total positions: %d | Total trade_log entries: %d\n", nrow(positions), nrow(trade_log)))
cat(sprintf("Saved at: %s\n\n", format(state$saved_at)))

recent <- positions %>% arrange(desc(entry_time)) %>% head(15)

cat("=== 15 most recent positions: entry rationale ===\n")
print(as.data.frame(recent %>%
  select(position_id, game_id, strategy, signal, entry_time, entry_price,
         lead_at_entry, period_at_entry, edge_at_entry, status, pnl)),
  row.names = FALSE, digits = 3)

cat("\n=== Matching trade_log notes (OPEN + SETTLE) for those positions ===\n")
notes <- trade_log %>% filter(position_id %in% recent$position_id) %>%
  arrange(timestamp) %>%
  select(position_id, timestamp, action, strategy, signal, price, contracts, amount, notes)
print(as.data.frame(notes), row.names = FALSE)

cat("\n=== Strategy/signal rationale summary across ALL positions (win rate by strategy) ===\n")
print(positions %>%
  filter(status %in% c("settled_win", "settled_loss")) %>%
  group_by(strategy, signal) %>%
  summarise(n = n(), win_rate = mean(status == "settled_win"),
            mean_edge_at_entry = mean(edge_at_entry, na.rm = TRUE),
            mean_lead_at_entry = mean(lead_at_entry, na.rm = TRUE),
            total_pnl = sum(pnl, na.rm = TRUE), .groups = "drop"))
