suppressMessages({library(tidymodels)})
r <- readRDS("wp_residuals/raw.rds")
train <- tibble(home_win = factor(r$y, levels=c(0,1), labels=c("away","home")),
  home_margin = r$margin, seconds_remaining = r$secs, home_rating_advantage = r$rating,
  period_factor = factor(r$period, levels=c("Q1","Q2","Q3","Q4","OT")))
rm(r); invisible(gc())
# Features: margin scaled by sqrt(time left in current segment) — Brownian-motion style WP
rec <- recipe(home_win ~ ., data = train) %>%
  step_mutate(
    t_left = ifelse(seconds_remaining > 0, seconds_remaining, 300 - ((-seconds_remaining) %% 300)),
    t_left = ifelse(seconds_remaining <= 0 & ((-seconds_remaining) %% 300) == 0,
                    ifelse(home_margin == 0, 300, 0), t_left),
    ot = as.numeric(seconds_remaining <= 0 & !(seconds_remaining == 0 & home_margin != 0)),
    z = home_margin / sqrt(t_left + 10),
    st = sqrt(t_left / 2880),
    rst = home_rating_advantage * st) %>%
  step_rm(seconds_remaining, period_factor, t_left)
wf <- workflow() %>% add_recipe(rec) %>%
  add_model(logistic_reg() %>% set_engine("glm", family = binomial(link = "probit")))
fit <- suppressWarnings(fit(wf, data = train))
print(coef(extract_fit_engine(fit)))
# strip training data so the file is small (predict.glm on newdata only needs coefs/terms/family)
g <- fit$fit$fit$fit
for (nm in c("data","model","y","fitted.values","residuals","weights","prior.weights","linear.predictors","effects")) g[[nm]] <- NULL
g$qr$qr <- NULL; fit$fit$fit$fit <- g
saveRDS(fit, "win_probability_model_v2.rds")
cat("saved, size MB:", round(file.size("win_probability_model_v2.rds")/1e6,2), "\n")
