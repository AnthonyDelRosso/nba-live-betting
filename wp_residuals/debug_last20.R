source_lines <- readLines("wp_residuals/refit_sqrt.R"); eval(parse(text=source_lines[1:grep("^C <- 10", source_lines)]))
ts <- r[splits$last20,]; tr <- r[!splits$last20,]
cat("NA in test cols:\n"); print(colSums(is.na(ts)))
cat("rating summary test vs train:\n"); print(summary(ts$rating)); print(summary(tr$rating))
cat("y mean test:", mean(ts$y), " train:", mean(tr$y), "\n")
f2 <- suppressWarnings(glm(specs$sqrt_probit2$f, binomial("probit"), tr)); p2 <- predict(f2, ts, type="response")
bad <- which(abs(ts$y - p2) > 0.999); cat("confidently-wrong rows:", length(bad), "\n"); print(head(ts[bad, c("gid","secs","t","margin","rating","y","z")], 10))
cat("games with NA rating:", n_distinct(ts$gid[is.na(ts$rating)]), "\n")
