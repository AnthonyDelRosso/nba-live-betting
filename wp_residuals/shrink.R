suppressMessages(library(tidymodels)); f <- readRDS("win_probability_model_v2.rds")
sz <- function(x) round(length(serialize(x, NULL))/1e6, 2)
probe <- tibble(home_margin=c(0,3,-6,10,2,0,-3), seconds_remaining=c(2880,1500,700,120,20,0,-150),
  home_rating_advantage=c(0,4,-3,1,0,2,-1), period_factor=factor(c("Q1","Q2","Q3","Q4","Q4","Q4","OT"), levels=c("Q1","Q2","Q3","Q4","OT")))
before <- predict(f, probe, type="prob")$.pred_home
g <- f$fit$fit$fit
g$family <- local(stats::binomial(link="probit"), envir=new.env(parent=baseenv()))
g$call <- NULL; environment(g$terms) <- baseenv(); g$formula <- NULL
f$fit$fit$fit <- g
f$fit$spec$eng_args$family <- rlang::quo(stats::binomial(link = "probit")) |> rlang::quo_set_env(baseenv())
f$fit$spec$method <- NULL
f$pre$mold$predictors <- f$pre$mold$predictors[0,]; f$pre$mold$outcomes <- f$pre$mold$outcomes[0,]
f$pre$actions$recipe$recipe$template <- f$pre$actions$recipe$recipe$template[0,]
if (!is.null(f$pre$mold$blueprint$recipe$template)) f$pre$mold$blueprint$recipe$template <- f$pre$mold$blueprint$recipe$template[0,]
cat("component sizes MB  pre:", sz(f$pre), " fit:", sz(f$fit), "\n")
after <- predict(f, probe, type="prob")$.pred_home
cat("max |diff| after shrink:", max(abs(before-after)), "\n"); print(round(after,4))
saveRDS(f, "win_probability_model_v2.rds"); cat("file MB:", round(file.size("win_probability_model_v2.rds")/1e6,3), "\n")
