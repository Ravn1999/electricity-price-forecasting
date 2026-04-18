library(forecast)
library(parallel)
# Lav alle kombinationer
kombinationer <- expand.grid(
  p = 1:7,
  q = 1:6,
  P = 0:5,
  Q = 0:6
)
kerner <- detectCores() - 1
cat(sprintf("Kører på %d kerner\n", kerner))
# Kør parallelt
resultater <- mclapply(1:nrow(kombinationer), function(i) {
  p <- kombinationer$p[i]
  q <- kombinationer$q[i]
  P <- kombinationer$P[i]
  Q <- kombinationer$Q[i]
  
  tryCatch({
    fit <- Arima(dk1_daily$price,
                 order    = c(p, 1, q),
                 seasonal = list(order = c(P, 1, Q), period = 7))
    
    data.frame(p=p, d=1, q=q, P=P, D=1, Q=Q,
               AIC=AIC(fit), AICC=fit$aicc, BIC=BIC(fit))
  }, error = function(e) NULL)
  
}, mc.cores = kerner)
# Saml resultater
resultater_clean <- Filter(Negate(is.null), resultater)
sarima_tabel <- do.call(rbind, resultater_clean)
sarima_tabel <- sarima_tabel[order(sarima_tabel$AIC), ]
print(head(sarima_tabel, 10))

write.csv(sarima_tabel, "data/sarima_grid_results.csv", row.names = FALSE)
cat("Gemt!\n")
