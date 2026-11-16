source_lines <- readLines("wp_residuals/refit_sqrt.R"); eval(parse(text=source_lines[1:grep("^C <- 10", source_lines)]))
set.seed(11); fold <- sample(rep(1:5, length.out=G))[r$gid]
out <- do.call(rbind, lapply(1:5, function(k) {
  tr <- r[fold!=k,]; ts <- r[fold==k,]
  sapply(c("current","sqrt_logit","sqrt_probit"), function(m) {
    p <- predict(suppressWarnings(glm(specs[[m]]$f, binomial(specs[[m]]$link), tr)), ts, type="response"); llf(ts$y, p) })
}))
print(round(out, 5)); cat("\nMean:\n"); print(round(colMeans(out),5))
cat("sqrt_probit improvement vs current, per fold (logloss):\n"); print(round(out[,"current"]-out[,"sqrt_probit"],5))
