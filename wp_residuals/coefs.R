suppressMessages(library(tidymodels))
m <- readRDS("win_probability_model.rds"); g <- extract_fit_engine(m); st <- extract_recipe(m)$steps[[3]]
b <- coef(g); s <- st$sds[names(b)[-1]]; mu <- st$means[names(b)[-1]]
raw <- b[-1]/s; int <- b[1]-sum(raw*mu)
print(round(c(intercept=unname(int), raw),6))
