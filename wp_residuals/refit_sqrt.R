suppressMessages({library(dplyr); library(tidyr)})
r <- readRDS("wp_residuals/raw.rds")
r$gid <- cumsum(c(TRUE, diff(r$secs) > 0))
# time left in the current segment (regulation, or the current 5-min OT)
seg_left <- function(secs, margin) {
  t <- ifelse(secs > 0, secs, 300 - ((-secs) %% 300))
  # exactly at a segment boundary (secs = 0, -300, ...): game over if not tied, else a fresh OT starts
  at_end <- secs <= 0 & ((-secs) %% 300) == 0
  ifelse(at_end, ifelse(margin == 0, 300, 0), t)
}
r <- r %>% mutate(t = seg_left(secs, margin), ot = as.numeric(secs <= 0 & !(secs == 0 & margin != 0)),
                  pf = factor(period, levels=c("Q1","Q2","Q3","Q4","OT")))
G <- max(r$gid); set.seed(7)
splits <- list(random = r$gid %in% sample(G, round(.2*G)),       # random 20% of games
               last20 = r$gid > round(.8*G))                     # last 20% of games in file order
llf <- function(y,p){p <- pmin(pmax(p,1e-12),1-1e-12); -mean(y*log(p)+(1-y)*log(1-p))}
auc <- function(y,p){rk <- rank(p); n1 <- sum(y); (sum(rk[y==1]) - n1*(n1+1)/2)/(n1*(length(y)-n1))}

feats <- function(d, c) d %>% mutate(z = margin/sqrt(t + c), st = sqrt(t/2880), rst = rating*st)
specs <- list(
  current      = list(f = y ~ margin*secs + rating + pf, link="logit"),
  sqrt_logit   = list(f = y ~ z + margin + st + rating + rst + ot, link="logit"),
  sqrt_probit  = list(f = y ~ z + margin + st + rating + rst + ot, link="probit"),
  sqrt_probit2 = list(f = y ~ z + I(z^3/100) + margin + st + rating + rst + ot, link="probit"))

C <- 10; r <- feats(r, C)
suppressMessages(library(mgcv))
fitall <- function(tr) {
  out <- lapply(specs, function(s) glm(s$f, binomial(s$link), tr))
  set.seed(1); sub <- tr[sample(nrow(tr), 500000),]
  out$gam_bench <- bam(y ~ te(margin, t, k=c(15,15)) + s(rating,k=8) + ti(rating,t,k=c(5,5)) + ot,
                       family=binomial, data=sub, discrete=TRUE)
  out
}
tbins <- c(-1,0,60,120,300,720,1440,2160,2880)
res <- list()
for (sp in names(splits)) {
  te <- splits[[sp]]; tr <- r[!te,]; ts <- r[te,]
  fits <- suppressWarnings(fitall(tr))
  P <- sapply(fits, function(f) as.numeric(predict(f, ts, type="response")))
  cat(sprintf("\n=========== Split: %s  (train %d games, test %d games, %d rows) ===========\n",
      sp, n_distinct(tr$gid), n_distinct(ts$gid), nrow(ts)))
  print(data.frame(model=colnames(P), logloss=apply(P,2,\(p) llf(ts$y,p)), brier=apply(P,2,\(p) mean((ts$y-p)^2)),
        auc=apply(P,2,\(p) auc(ts$y,p))) %>% mutate(across(-model, ~round(.,5))), row.names=FALSE)
  if (sp == "random") { res$P <- P; res$ts <- ts; res$fits <- fits }
}
P <- res$P; ts <- res$ts; fits <- res$fits
long <- bind_cols(ts %>% select(y, margin, t, secs, rating), as_tibble(P)) %>%
  pivot_longer(c(current, sqrt_logit, sqrt_probit, sqrt_probit2, gam_bench), names_to="model", values_to="p")
cat("\n== Log-loss by time left (random split) ==\n")
print(as.data.frame(long %>% mutate(tb=cut(ifelse(secs<=0,-0.5,t), tbins, labels=c("OT","0-1m","1-2m","2-5m","5-12m","12-24m","24-36m","36-48m"))) %>%
  group_by(tb, model) %>% summarise(ll=llf(y,p), .groups="drop") %>% pivot_wider(names_from=model, values_from=ll) %>% mutate(across(-tb, ~round(.,4)))))
cat("\n== Calibration error in margin x time cells (test rows, cells n>=500): actual - predicted, pts ==\n")
cells <- long %>% mutate(mb=cut(margin, c(-60,-15,-10,-6,-3,-1,0,2,5,9,14,60)),
     tb=cut(ifelse(secs<=0,-0.5,t), tbins, labels=c("OT","0-1m","1-2m","2-5m","5-12m","12-24m","24-36m","36-48m"))) %>%
  group_by(model, mb, tb) %>% summarise(n=n(), bias=100*(mean(y)-mean(p)), .groups="drop") %>% filter(n>=500)
print(as.data.frame(cells %>% group_by(model) %>% summarise(cells=n(), mean_abs_bias=mean(abs(bias)), wtd_abs_bias=weighted.mean(abs(bias),n),
      max_abs_bias=max(abs(bias))) %>% mutate(across(-c(model,cells), ~round(.,2)))))
cat("\nWorst 8 cells for sqrt_probit:\n"); print(as.data.frame(cells %>% filter(model=="sqrt_probit") %>% arrange(desc(abs(bias))) %>% head(8) %>% mutate(bias=round(bias,1))))
cat("\n== Rating x time bias (pts), current vs sqrt_probit ==\n")
print(as.data.frame(long %>% filter(model %in% c("current","sqrt_probit")) %>%
  mutate(rb=cut(rating,c(-Inf,-6,-2,2,6,Inf)), tb=cut(ifelse(secs<=0,-0.5,t), c(-1,0,720,1440,2160,2880), labels=c("OT","Q4","Q3","Q2","Q1"))) %>%
  group_by(model, rb, tb) %>% summarise(b=round(100*(mean(y)-mean(p)),1), .groups="drop") %>% pivot_wider(names_from=tb, values_from=b)))
cat("\n== Example WPs, home leading, rating 0 (regulation) ==\n")
ex <- expand.grid(margin=c(0,3,6,10,15), mins=c(48,36,24,12,6,2,0.5)) %>% mutate(secs=mins*60, t=secs, rating=0, ot=0,
   pf=factor(case_when(mins>36~"Q1",mins>24~"Q2",mins>12~"Q3",TRUE~"Q4"), levels=c("Q1","Q2","Q3","Q4","OT"))) %>% feats(C)
for (m in c("current","sqrt_probit","gam_bench")) ex[[m]] <- round(as.numeric(predict(fits[[m]], ex, type="response")),3)
print(ex %>% select(margin, mins, current, sqrt_probit, gam_bench) %>% filter(!(margin==0 & mins<48 & mins>0.5)))
cat("\n== sqrt_probit coefficients (fit on random-split train) ==\n"); print(round(coef(fits$sqrt_probit),5))
saveRDS(list(C=C, fits=fits[c("current","sqrt_logit","sqrt_probit","sqrt_probit2")] %>% lapply(\(f){f$data<-NULL;f$model<-NULL;f$y<-NULL;f$fitted.values<-NULL;f$residuals<-NULL;f$weights<-NULL;f$prior.weights<-NULL;f$linear.predictors<-NULL;f$effects<-NULL;f$qr$qr<-NULL;f}),
        cells=cells), "wp_residuals/refit_results.rds")
