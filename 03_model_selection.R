# 03_model_selection.R
library(forecast)

# ── SARIMA ──────────────────────────────────────────────────────────────────

# 03_model_selection.R
library(forecast)

# ── SARIMA grid search ───────────────────────────────────────────────────────

p_vals <- 0:2
q_vals <- 0:2
P_vals <- 0:8
Q_vals <- 0:1

resultater <- list()

for (p in p_vals) {
  for (q in q_vals) {
    for (P in P_vals) {
      for (Q in Q_vals) {
        tryCatch({
          fit <- Arima(dk1_bc,
                       order    = c(p, 1, q),
                       seasonal = list(order = c(P, 1, Q), period = 7))
          
          resultater[[length(resultater) + 1]] <- data.frame(
            p = p, d = 1, q = q,
            P = P, D = 1, Q = Q,
            AIC  = AIC(fit),
            AICC = fit$aicc,
            BIC  = BIC(fit)
          )
        }, error = function(e) NULL)
      }
    }
  }
}

# Saml og sorter efter AIC
sarima_tabel <- do.call(rbind, resultater)
sarima_tabel <- sarima_tabel[order(sarima_tabel$AIC), ]

print(head(sarima_tabel, 10))