suppressMessages(source("win_probability_model.R"))
v1 <- load_wp_model("win_probability_model.rds"); v2 <- load_wp_model("win_probability_model_v2.rds")
st <- tibble(home_margin=c(0,0,5,-4,8,3,1,0), seconds_remaining=c(2880,1440,1800,600,90,30,-200,0),
             period=c(1,3,2,4,4,4,5,4), home_rating_advantage=c(5,0,-2,3,0,0,0,0))
out <- predict_wp_states(v1, st) %>% rename(wp_v1=home_wp) %>% mutate(wp_v2 = predict_wp_states(v2, st)$home_wp)
print(out %>% mutate(across(starts_with("wp"), ~round(.,3))))
cat("single call:", predict_wp(v2, home_margin=6, seconds_remaining=60, period=4, home_rating_advantage=0), "\n")
