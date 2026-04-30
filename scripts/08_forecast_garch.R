library(rugarch)
library(ggplot2)

# ── 1. Statisk forecast ───────────────────────────────────────
h <- nrow(dk1_fc_daily)

# SARIMA forecast (mean)
fc_sarima <- forecast(sarima_valgt, h = h)

# GARCH volatilitet forecast
fc_garch <- ugarchforecast(garch_valgt, n.ahead = h)

sarima_mean <- as.numeric(fc_sarima$mean)
garch_sigma <- as.numeric(sigma(fc_garch))

# 80% og 95% prediction intervals med Student-t
z80 <- qdist("std", 0.90,  shape = 5.238)
z95 <- qdist("std", 0.975, shape = 5.238)

fc_df <- data.frame(
  date   = dk1_fc_daily$date,
  mean   = sarima_mean,
  lo80   = sarima_mean - z80 * garch_sigma,
  hi80   = sarima_mean + z80 * garch_sigma,
  lo95   = sarima_mean - z95 * garch_sigma,
  hi95   = sarima_mean + z95 * garch_sigma,
  actual = dk1_fc_daily$price
)

# ── 2. Plot ───────────────────────────────────────────────────
ggplot(fc_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), fill = "steelblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = lo80, ymax = hi80), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = actual,  colour = "Actual")) +
  geom_line(aes(y = mean,    colour = "Forecast")) +
  scale_colour_manual(values = c("Actual" = "black", "Forecast" = "steelblue")) +
  labs(x = "Time", y = "EUR/MWh", colour = "") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_bw()

ggsave("output/08_static_forecast_garch.png", width = 10, height = 5)
cat("Gemt!\n")

# ── 3. Fejlmål ────────────────────────────────────────────────
rmse <- sqrt(mean((fc_df$actual - fc_df$mean)^2))
mae  <- mean(abs(fc_df$actual - fc_df$mean))

cat(sprintf("Statisk RMSE: %.2f\n", rmse))
cat(sprintf("Statisk MAE:  %.2f\n", mae))

# ── 4. Expanding window forecast ─────────────────────────────
n_fc <- nrow(dk1_fc_daily)

expanding_df <- data.frame(
  date   = dk1_fc_daily$date,
  mean   = NA_real_,
  lo80   = NA_real_,
  hi80   = NA_real_,
  lo95   = NA_real_,
  hi95   = NA_real_,
  actual = dk1_fc_daily$price
)

for (i in 1:n_fc) {
  # Udvid træningsdata med én dag ad gangen
  train <- c(dk1_daily$price, dk1_fc_daily$price[seq_len(i - 1)])
  
  # Re-estimer SARIMA med faste parametre
  fit_sarima <- Arima(train, model = sarima_valgt)
  
  # SARIMA residualer til GARCH
  resid_i <- as.numeric(residuals(fit_sarima))
  
  # Re-estimer GARCH med faste parametre
  fit_garch <- ugarchfit(garch_spec, data = resid_i, solver = "hybrid")
  
  # Forecast én dag frem
  fc_s <- forecast(fit_sarima, h = 1)
  fc_g <- ugarchforecast(fit_garch, n.ahead = 1)
  
  mean_i  <- as.numeric(fc_s$mean)
  sigma_i <- as.numeric(sigma(fc_g))
  
  expanding_df$mean[i] <- mean_i
  expanding_df$lo80[i] <- mean_i - z80 * sigma_i
  expanding_df$hi80[i] <- mean_i + z80 * sigma_i
  expanding_df$lo95[i] <- mean_i - z95 * sigma_i
  expanding_df$hi95[i] <- mean_i + z95 * sigma_i
  
  if (i %% 10 == 0) cat(sprintf("Dag %d / %d\n", i, n_fc))
}

# ── 5. Plot ───────────────────────────────────────────────────
ggplot(expanding_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), fill = "steelblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = lo80, ymax = hi80), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = actual,  colour = "Actual")) +
  geom_line(aes(y = mean,    colour = "Forecast")) +
  scale_colour_manual(values = c("Actual" = "black", "Forecast" = "steelblue")) +
  labs(x = "Time", y = "EUR/MWh", colour = "") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_bw()

ggsave("output/08_expanding_forecast_garch.png", width = 10, height = 5)
cat("Gemt!\n")

# ── 6. Fejlmål ────────────────────────────────────────────────
rmse_exp <- sqrt(mean((expanding_df$actual - expanding_df$mean)^2))
mae_exp  <- mean(abs(expanding_df$actual - expanding_df$mean))

cat(sprintf("Expanding RMSE: %.2f\n", rmse_exp))
cat(sprintf("Expanding MAE:  %.2f\n", mae_exp))
cat(sprintf("Statisk   RMSE: %.2f\n", rmse))
cat(sprintf("Statisk   MAE:  %.2f\n", mae))
