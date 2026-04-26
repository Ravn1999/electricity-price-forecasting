library(rugarch)

# ── 1. ARMA(1,1)+GARCH(1,1)-M på originale priser ─────────────
garch_m_direkte_spec <- ugarchspec(
  variance.model     = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model         = list(armaOrder = c(1, 1), include.mean = TRUE,
                            archm = TRUE, archpow = 2),
  distribution.model = "std"
)

garch_m_direkte <- ugarchfit(garch_m_direkte_spec, 
                             data = dk1_daily$price, 
                             solver = "solnp")

show(garch_m_direkte)

# ── Fitted sigma fra ARMA+GARCH-M ────────────────────────────
sigma_m <- as.numeric(sigma(garch_m_direkte))
fitted_mean <- as.numeric(fitted(garch_m_direkte))
lambda_m <- coef(garch_m_direkte)["archm"]

plot_df <- data.frame(
  date         = dk1_daily$date[(nrow(dk1_daily) - length(sigma_m) + 1):nrow(dk1_daily)],
  price        = dk1_daily$price[(nrow(dk1_daily) - length(sigma_m) + 1):nrow(dk1_daily)],
  sigma        = sigma_m,
  risikopraemie = lambda_m * sigma_m^2,
  mean_med     = fitted_mean,
  mean_uden    = fitted_mean - lambda_m * sigma_m^2
)

# ── Plot 2: Risikopræmie over tid ─────────────────────────────
ggplot(plot_df, aes(x = date, y = risikopraemie)) +
  geom_line() +
  labs(x = "Time", y = "Volatility effect (EUR/MWh)") +
  theme_bw()

ggsave("output/09_risikopraemie.png", width = 10, height = 5)
cat("Gemt!\n")

# ── Plot 3: Mean med og uden risikopræmie ─────────────────────
ggplot(plot_df, aes(x = date)) +
  geom_line(aes(y = price,      colour = "Actual")) +
  geom_line(aes(y = mean_med,   colour = "With Volatility effect")) +
  geom_line(aes(y = mean_uden,  colour = "Without Volatility effect")) +
  scale_colour_manual(values = c("Actual" = "black", 
                                 "With Volatility effect" = "steelblue",
                                 "Without Volatility effect" = "red")) +
  labs(x = "Time", y = "EUR/MWh", colour = "") +
  theme_bw()

ggsave("output/09_mean_sammenligning.png", width = 10, height = 5)
cat("Gemt!\n")

# ── 1. Statisk forecast ───────────────────────────────────────
fc_garch_m_direkte <- ugarchforecast(garch_m_direkte, n.ahead = h)

sigma_fc   <- as.numeric(sigma(fc_garch_m_direkte))
mean_fc    <- as.numeric(fitted(fc_garch_m_direkte))

fc_df_direkte <- data.frame(
  date   = dk1_fc_daily$date,
  mean   = mean_fc,
  lo80   = mean_fc - z80 * sigma_fc,
  hi80   = mean_fc + z80 * sigma_fc,
  lo95   = mean_fc - z95 * sigma_fc,
  hi95   = mean_fc + z95 * sigma_fc,
  actual = dk1_fc_daily$price
)

ggplot(fc_df_direkte, aes(x = date)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), fill = "steelblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = lo80, ymax = hi80), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = actual,  colour = "Actual")) +
  geom_line(aes(y = mean,    colour = "Forecast")) +
  scale_colour_manual(values = c("Actual" = "black", "Forecast" = "steelblue")) +
  labs(x = "Time", y = "EUR/MWh", colour = "") +
  theme_bw()

ggsave("output/09_static_forecast_garch_m_direkte.png", width = 10, height = 5)
cat("Gemt!\n")

rmse_direkte <- sqrt(mean((fc_df_direkte$actual - fc_df_direkte$mean)^2))
mae_direkte  <- mean(abs(fc_df_direkte$actual - fc_df_direkte$mean))
cat(sprintf("Statisk RMSE: %.2f\n", rmse_direkte))
cat(sprintf("Statisk MAE:  %.2f\n", mae_direkte))

# ── 2. Expanding window forecast ─────────────────────────────
expanding_df_direkte <- data.frame(
  date   = dk1_fc_daily$date,
  mean   = NA_real_,
  lo80   = NA_real_,
  hi80   = NA_real_,
  lo95   = NA_real_,
  hi95   = NA_real_,
  actual = dk1_fc_daily$price
)

# Tilføj kolonne
expanding_df_direkte$vol_effect <- NA_real_

for (i in 1:n_fc) {
  train <- c(dk1_daily$price, dk1_fc_daily$price[seq_len(i - 1)])
  
  tryCatch({
    spec_i <- ugarchspec(
      variance.model     = list(model = "sGARCH", garchOrder = c(1, 1)),
      mean.model         = list(armaOrder = c(1, 1), include.mean = TRUE,
                                archm = TRUE, archpow = 2),
      distribution.model = "std"
    )
    
    fit_i  <- ugarchfit(spec_i, data = train, solver = "solnp")
    fc_i   <- ugarchforecast(fit_i, n.ahead = 1)
    
    mean_i  <- as.numeric(fitted(fc_i))
    sigma_i <- as.numeric(sigma(fc_i))
    lambda_i <- coef(fit_i)["archm"]
    
    expanding_df_direkte$mean[i]       <- mean_i
    expanding_df_direkte$lo80[i]       <- mean_i - z80 * sigma_i
    expanding_df_direkte$hi80[i]       <- mean_i + z80 * sigma_i
    expanding_df_direkte$lo95[i]       <- mean_i - z95 * sigma_i
    expanding_df_direkte$hi95[i]       <- mean_i + z95 * sigma_i
    expanding_df_direkte$vol_effect[i] <- lambda_i * sigma_i^2
    
  }, error = function(e) {
    cat(sprintf("Fejl dag %d: %s\n", i, e$message))
  })
  
  if (i %% 10 == 0) cat(sprintf("Dag %d / %d\n", i, n_fc))
}

ggplot(expanding_df_direkte, aes(x = date)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), fill = "steelblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = lo80, ymax = hi80), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = actual,  colour = "Actual")) +
  geom_line(aes(y = mean,    colour = "Forecast")) +
  scale_colour_manual(values = c("Actual" = "black", "Forecast" = "steelblue")) +
  labs(x = "Time", y = "EUR/MWh", colour = "") +
  theme_bw()

ggsave("output/09_expanding_forecast_garch_m_direkte.png", width = 10, height = 5)
cat("Gemt!\n")

rmse_direkte_exp <- sqrt(mean((expanding_df_direkte$actual - expanding_df_direkte$mean)^2, na.rm = TRUE))
mae_direkte_exp  <- mean(abs(expanding_df_direkte$actual - expanding_df_direkte$mean), na.rm = TRUE)
cat(sprintf("Expanding RMSE: %.2f\n", rmse_direkte_exp))
cat(sprintf("Expanding MAE:  %.2f\n", mae_direkte_exp))


# Plot volatility effect
p1 <- ggplot(expanding_df_direkte, aes(x = date, y = vol_effect)) +
  geom_line() +
  labs(x = "", y = "Volatility effect") +
  theme_bw()

# Plot actual

p2 <- ggplot(expanding_df_direkte, aes(x = date, y = actual)) +
  geom_line() +
  labs(x = "Time", y = "EUR/MWh") +
  theme_bw()

library(gridExtra)
grid.arrange(p1, p2, nrow = 2)

ggsave("output/09_volatility_effect_expanding.png",
       grid.arrange(p1, p2, nrow = 2),
       width = 10, height = 8)
cat("Gemt!\n")