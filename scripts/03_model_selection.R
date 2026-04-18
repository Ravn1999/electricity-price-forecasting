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


#FEJL: 
# Find indeks på de NULL resultater
fejlede_idx <- which(sapply(resultater, is.null))

# Hent de tilsvarende kombinationer
fejlede <- kombinationer[fejlede_idx, ]

cat(sprintf("Antal fejlede: %d\n", nrow(fejlede)))
print(fejlede)

fejl_info <- mclapply(fejlede_idx, function(i) {
  p <- kombinationer$p[i]; q <- kombinationer$q[i]
  P <- kombinationer$P[i]; Q <- kombinationer$Q[i]
  
  tryCatch({
    Arima(dk1_daily$price,
          order    = c(p, 1, q),
          seasonal = list(order = c(P, 1, Q), period = 7))
    NULL
  }, error = function(e) {
    data.frame(p=p, q=q, P=P, Q=Q, fejl=e$message)
  })
}, mc.cores = kerner)

fejl_tabel <- do.call(rbind, Filter(Negate(is.null), fejl_info))
print(fejl_tabel)

write.csv(fejl_tabel, "data/sarima_grid_errors.csv", row.names = FALSE)
cat("Gemt!\n")


#Coefficients chosen SARIMA model (5,1,4)(4,1,3)

# Fit den valgte model
sarima_valgt <- Arima(dk1_daily$price,
                      order    = c(5, 1, 4),
                      seasonal = list(order = c(4, 1, 3), period = 7))

summary(sarima_valgt)

# Gem summary output som tekst
sink("data/sarima_summary.txt")
summary(sarima_valgt)
sink()
cat("Gemt!\n")

# Koefficienter
koef <- coef(sarima_valgt)
se   <- sqrt(diag(vcov(sarima_valgt)))

koef_df <- data.frame(
  Coefficient = names(koef),
  Estimate    = round(koef, 4),
  Std.Error   = round(se, 4)
)

# Fejlmål fra summary
acc <- accuracy(sarima_valgt)

fejl_df <- data.frame(
  Metric = c("AIC", "AICc", "BIC", "RMSE", "MAE"),
  Value  = round(c(sarima_valgt$aic, sarima_valgt$aicc, sarima_valgt$bic,
                   acc[,"RMSE"], acc[,"MAE"]), 2)
)

write.csv(koef_df, "data/sarima_koefficienter.csv", row.names = FALSE)
write.csv(fejl_df, "data/sarima_fejlmaal.csv",      row.names = FALSE)
cat("Gemt!\n")
