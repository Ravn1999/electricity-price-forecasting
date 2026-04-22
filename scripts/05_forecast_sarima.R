# ── 1. Indlæs og rens forecast data ──────────────────────────
dk1_fc_raw <- read.csv("data/DK1-forecast.csv", sep = ";")

dk1_fc_raw$time  <- as.POSIXct(dk1_fc_raw$TimeDK, format = "%Y-%m-%d %H:%M:%S")
dk1_fc_raw$price <- as.numeric(gsub(",", ".", dk1_fc_raw$DayAheadPriceEUR))

dk1_fc_raw <- dk1_fc_raw[, c("time", "price")]

# Dagligt gennemsnit
dk1_fc_daily <- aggregate(price ~ as.Date(time), dk1_fc_raw, mean)
names(dk1_fc_daily) <- c("date", "price")
dk1_fc_daily <- dk1_fc_daily[order(dk1_fc_daily$date), ]

# ── 2. Statisk forecast ───────────────────────────────────────
h <- nrow(dk1_fc_daily)

fc <- forecast(sarima_valgt, h = h)

fc_df <- data.frame(
  date  = dk1_fc_daily$date,
  mean  = as.numeric(fc$mean),
  lo80  = as.numeric(fc$lower[, 1]),
  hi80  = as.numeric(fc$upper[, 1]),
  lo95  = as.numeric(fc$lower[, 2]),
  hi95  = as.numeric(fc$upper[, 2]),
  actual = dk1_fc_daily$price
)

# ── 3. Plot ───────────────────────────────────────────────────
ggplot(fc_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), fill = "steelblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = lo80, ymax = hi80), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = actual, colour = "Actual")) +
  geom_line(aes(y = mean,   colour = "Forecast")) +
  scale_colour_manual(values = c("Actual" = "black", "Forecast" = "steelblue")) +
  labs(x      = "Time",
       y      = "EUR/MWh",
       colour = "") +
  theme_bw()

ggsave("output/05_static_forecast.png", width = 10, height = 5)
cat("Gemt!\n")

# ── 4. Fejlmål ────────────────────────────────────────────────
rmse <- sqrt(mean((fc_df$actual - fc_df$mean)^2))
mae  <- mean(abs(fc_df$actual - fc_df$mean))

cat(sprintf("RMSE: %.2f\n", rmse))
cat(sprintf("MAE:  %.2f\n", mae))


# ── 5. Expanding window forecast ─────────────────────────────
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
  
  # Re-estimer med faste parametre
  fit_exp <- Arima(train, model = sarima_valgt)
  
  # Forecast én dag frem
  fc_exp <- forecast(fit_exp, h = 1)
  
  expanding_df$mean[i] <- as.numeric(fc_exp$mean)
  expanding_df$lo80[i] <- as.numeric(fc_exp$lower[, 1])
  expanding_df$hi80[i] <- as.numeric(fc_exp$upper[, 1])
  expanding_df$lo95[i] <- as.numeric(fc_exp$lower[, 2])
  expanding_df$hi95[i] <- as.numeric(fc_exp$upper[, 2])
  
  if (i %% 10 == 0) cat(sprintf("Dag %d / %d\n", i, n_fc))
}

# ── 6. Plot ───────────────────────────────────────────────────
ggplot(expanding_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), fill = "steelblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = lo80, ymax = hi80), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = actual,  colour = "Actual")) +
  geom_line(aes(y = mean,    colour = "Forecast")) +
  scale_colour_manual(values = c("Actual" = "black", "Forecast" = "steelblue")) +
  labs(x      = "Time",
       y      = "EUR/MWh",
       colour = "") +
  theme_bw()

ggsave("output/05_expanding_forecast.png", width = 10, height = 5)
cat("Gemt!\n")

# ── 7. Fejlmål ────────────────────────────────────────────────
rmse_exp <- sqrt(mean((expanding_df$actual - expanding_df$mean)^2))
mae_exp  <- mean(abs(expanding_df$actual - expanding_df$mean))

cat(sprintf("Expanding RMSE: %.2f\n", rmse_exp))
cat(sprintf("Expanding MAE:  %.2f\n", mae_exp))
cat(sprintf("Statisk   RMSE: %.2f\n", rmse))
cat(sprintf("Statisk   MAE:  %.2f\n", mae))

# ── 8. Histogram af residualer ────────────────────────────────
resid_static    <- fc_df$actual        - fc_df$mean
resid_expanding <- expanding_df$actual - expanding_df$mean

# Statisk
ggplot(data.frame(residual = resid_static), aes(x = residual)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.7) +
  stat_function(fun  = dnorm,
                args = list(mean = mean(resid_static), sd = sd(resid_static)),
                colour = "black", linewidth = 1) +
  labs(x = "Residuals (EUR/MWh)", y = "Density") +
  theme_bw()

ggsave("output/05_residuals_histogram_static.png", width = 10, height = 5)
cat("Statisk histogram gemt!\n")

# Expanding window
ggplot(data.frame(residual = resid_expanding), aes(x = residual)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.7) +
  stat_function(fun  = dnorm,
                args = list(mean = mean(resid_expanding), sd = sd(resid_expanding)),
                colour = "black", linewidth = 1) +
  labs(x = "Residuals (EUR/MWh)", y = "Density") +
  theme_bw()

ggsave("output/05_residuals_histogram_expanding.png", width = 10, height = 5)
cat("Expanding histogram gemt!\n")