suppressMessages({library(tidymodels); library(dplyr)})
m <- readRDS("win_probability_model.rds")
g <- extract_fit_engine(m); rec <- extract_recipe(m)
norm <- rec$steps[[3]]; mu <- norm$means; sdv <- norm$sds
d <- g$data
raw <- tibble(
  margin = d$home_margin*sdv["home_margin"]+mu["home_margin"],
  secs   = d$seconds_remaining*sdv["seconds_remaining"]+mu["seconds_remaining"],
  rating = d$home_rating_advantage*sdv["home_rating_advantage"]+mu["home_rating_advantage"],
  y = as.numeric(d$..y=="home"),
  p = fitted(g))
rm(d, m); invisible(gc())
raw <- raw %>% mutate(margin=round(margin), secs=round(secs),
  period = case_when(secs>2160~"Q1",secs>1440~"Q2",secs>720~"Q3",secs>0~"Q4",TRUE~"OT"),
  r = y-p, ll = -(y*log(pmax(p,1e-15))+(1-y)*log(pmax(1-p,1e-15))))
saveRDS(raw, "wp_residuals/raw.rds")
cat(sprintf("N=%d  Brier=%.4f  LogLoss=%.4f  base=%.3f\n", nrow(raw), mean(raw$r^2), mean(raw$ll), mean(raw$y)))
summ <- function(df, ...) df %>% group_by(...) %>% summarise(n=n(), pred=mean(p), act=mean(y),
   bias=act-pred, z=(act-pred)/sqrt(sum(p*(1-p)))*n, brier=mean(r^2), .groups="drop")
cat("\n== Calibration by predicted decile ==\n")
print(summ(raw, bin=cut(p, seq(0,1,.1))), n=20)
cat("\n== Extreme tails ==\n")
print(summ(raw, bin=cut(p, c(0,.01,.03,.05,.95,.97,.99,1))), n=20)
cat("\n== By period ==\n"); print(summ(raw, period))
cat("\n== Time remaining (min) ==\n")
print(summ(raw, tbin=cut(secs/60, c(-30,0,1,2,3,5,8,12,18,24,36,48.1))), n=20)
cat("\n== Margin x time: bias (actual - predicted), cells n>=2000 ==\n")
mt <- summ(raw, mbin=cut(margin, c(-60,-20,-15,-10,-6,-3,-1,0,2,5,9,14,19,60)),
                tbin=cut(secs/60, c(-30,0,2,5,12,24,36,48.1)))
w <- mt %>% filter(n>=2000) %>% select(mbin,tbin,bias) %>% mutate(bias=round(bias*100,1)) %>%
  tidyr::pivot_wider(names_from=tbin, values_from=bias)
print(as.data.frame(w))
cat("\n== Worst cells by |z| (n>=2000) ==\n")
print(as.data.frame(mt %>% filter(n>=2000) %>% arrange(desc(abs(z))) %>% head(15) %>% mutate(across(c(pred,act,bias,brier),~round(.,3)), z=round(z,1))))
cat("\n== Rating advantage ==\n")
print(summ(raw, rb=cut(rating, c(-Inf,-10,-6,-3,-1,1,3,6,10,Inf))))
cat("\n== Rating x time (bias %) ==\n")
rt <- summ(raw, rb=cut(rating, c(-Inf,-6,-2,2,6,Inf)), tbin=cut(secs/60,c(-30,0,12,24,36,48.1)))
print(as.data.frame(rt %>% select(rb,tbin,bias) %>% mutate(bias=round(bias*100,1)) %>% tidyr::pivot_wider(names_from=tbin, values_from=bias)))
cat("\n== Tied games by time ==\n")
print(summ(raw %>% filter(margin==0), tbin=cut(secs/60, c(-30,0,1,3,6,12,24,48.1))))
cat("\n== Rating NA-ish / early-season (|rating|>15) ==\n")
print(summ(raw, big=abs(rating)>15))
cat("\n== Contribution to excess logloss: compare vs flexible model ==\n")
library(mgcv)
set.seed(1); s <- raw %>% mutate(ot=secs<=0, tr=pmax(secs,0)) %>% sample_n(400000)
gm <- bam(y ~ te(margin, tr, k=c(15,15)) + s(rating,k=8) + ti(rating,tr,k=c(5,5)) + ot + s(margin, by=as.numeric(ot), k=8),
          family=binomial, data=s, discrete=TRUE)
hold <- raw %>% mutate(ot=secs<=0, tr=pmax(secs,0)) %>% sample_n(400000)
hold$pg <- predict(gm, hold, type="response")
llf <- function(y,p) -(y*log(pmax(p,1e-15))+(1-y)*log(pmax(1-p,1e-15)))
hold <- hold %>% mutate(llg=llf(y,pg), dll=ll-llg)
cat(sprintf("Holdout: glm LL=%.4f Brier=%.4f | GAM LL=%.4f Brier=%.4f\n", mean(hold$ll), mean(hold$r^2), mean(hold$llg), mean((hold$y-hold$pg)^2)))
cat("\nWhere the GAM gains (share of total logloss improvement):\n")
print(as.data.frame(hold %>% group_by(tbin=cut(secs/60, c(-30,0,2,5,12,24,36,48.1))) %>% summarise(n=n(), gain=sum(dll)) %>% mutate(share=round(gain/sum(gain),3))))
print(as.data.frame(hold %>% group_by(mbin=cut(abs(margin), c(-1,0,3,6,10,15,20,60))) %>% summarise(n=n(), gain=sum(dll)) %>% mutate(share=round(gain/sum(gain),3))))
cat("\nExample: GAM vs glm WP for home leading by margin at minutes left (rating 0)\n")
ex <- expand.grid(margin=c(3,6,10,15,20), mins=c(36,24,12,6,2,0.5)) %>% mutate(secs=mins*60, tr=secs, rating=0, ot=FALSE)
ex$gam <- round(predict(gm, ex, type="response"),3)
X <- ex %>% transmute(home_margin=(margin-mu["home_margin"])/sdv["home_margin"],
  seconds_remaining=(secs-mu["seconds_remaining"])/sdv["seconds_remaining"],
  home_rating_advantage=(0-mu["home_rating_advantage"])/sdv["home_rating_advantage"],
  home_margin_x_seconds_remaining=(margin*secs-mu["home_margin_x_seconds_remaining"])/sdv["home_margin_x_seconds_remaining"],
  period_factor_Q2=(mins<=36&mins>24-mu["period_factor_Q2"])*0, period_factor_Q3=0, period_factor_Q4=0, period_factor_OT=0)
# simple: compute glm logit manually with period dummies
cf <- coef(g); pf <- function(mins) c(Q2=mins<=36&mins>24, Q3=mins<=24&mins>12, Q4=mins<=12)
eta <- sapply(seq_len(nrow(ex)), function(i){ mins<-ex$mins[i]; pd<-pf(mins)
  z <- c(1, X$home_margin[i], X$seconds_remaining[i], X$home_rating_advantage[i], X$home_margin_x_seconds_remaining[i],
    (pd["Q2"]-mu["period_factor_Q2"])/sdv["period_factor_Q2"], (pd["Q3"]-mu["period_factor_Q3"])/sdv["period_factor_Q3"],
    (pd["Q4"]-mu["period_factor_Q4"])/sdv["period_factor_Q4"], (0-mu["period_factor_OT"])/sdv["period_factor_OT"]); sum(cf*z)})
ex$glm <- round(plogis(eta),3)
print(ex %>% select(margin,mins,glm,gam) %>% arrange(margin, desc(mins)))
