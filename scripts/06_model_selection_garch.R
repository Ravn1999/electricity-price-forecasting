install.packages("rugarch")
library(rugarch)

# ── 1. Residualer fra SARIMA ──────────────────────────────────
residualer <- as.numeric(residuals(sarima_valgt))
n <- length(residualer)

# ── 2. Grid search ────────────────────────────────────────────
kombinationer <- expand.grid(
  m    = 0:2,
  s    = 0:2,
  dist = c("norm", "std")
)

# Fjern (0,0) kombinationen
kombinationer <- kombinationer[!(kombinationer$m == 0 & kombinationer$s == 0), ]

# ── 3. Fit alle modeller på SARIMA residualer ─────────────────
resultater <- lapply(1:nrow(kombinationer), function(i) {
  m    <- kombinationer$m[i]
  s    <- kombinationer$s[i]
  dist <- as.character(kombinationer$dist[i])
  k    <- m + s + 1
  
  tryCatch({
    spec <- ugarchspec(
      variance.model     = list(model = "sGARCH", garchOrder = c(m, s)),
      mean.model         = list(armaOrder = c(0, 0), include.mean = FALSE),
      distribution.model = dist
    )
    
    fit    <- ugarchfit(spec, data = residualer, solver = "hybrid")
    loglik <- likelihood(fit)
    
    AIC  <- -2 * loglik + 2 * k
    AICC <- -2 * loglik + 2 * k * (n / (n - k - 1))
    BIC  <- -2 * loglik + log(n) * k
    
    data.frame(m = m, s = s, dist = dist,
               AIC = round(AIC, 2), AICC = round(AICC, 2), BIC = round(BIC, 2))
    
  }, error = function(e) NULL)
})

# ── 4. Saml og sorter ─────────────────────────────────────────
garch_tabel <- do.call(rbind, Filter(Negate(is.null), resultater))
garch_tabel <- garch_tabel[order(garch_tabel$AIC), ]

print(head(garch_tabel, 10))
write.csv(garch_tabel, "data/garch_grid_results.csv", row.names = FALSE)
cat("Gemt!\n")