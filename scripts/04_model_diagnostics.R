library(ggplot2)

# Inverse AR og MA rødder
autoplot(sarima_valgt) +
  theme_bw()

ggsave("output/04_sarima_roedder.png", width = 10, height = 5)
cat("Gemt!\n")

# Fitted værdier mod observerede
dk1_fitted <- data.frame(
  date     = dk1_daily$date,
  observed = dk1_daily$price,
  fitted   = as.numeric(fitted(sarima_valgt))
)

ggplot(dk1_fitted, aes(x = date)) +
  geom_line(aes(y = observed, colour = "Observed")) +
  geom_line(aes(y = fitted,   colour = "Fitted")) +
  scale_colour_manual(values = c("Observed" = "black", "Fitted" = "red")) +
  labs(title   = "",
       x       = "Time",
       y       = "EUR/MWh",
       colour  = "") +
  theme_bw()
ggsave("output/04_fitted_model.png", width = 10, height = 5)
cat("Gemt!\n")

# Residual diagnostik
residualer <- residuals(sarima_valgt)

n <- length(residualer)

dk1_resid <- data.frame(
  date  = dk1_daily$date[(nrow(dk1_daily) - n + 1):nrow(dk1_daily)],
  resid = as.numeric(residualer)
)

# Run-sequence plot
p1 <- ggplot(dk1_resid, aes(x = date, y = resid)) +
  geom_line() +
  labs(title = "",
       x = "Time", y = "Residuals") +
  theme_bw()
p1
ggsave("output/04_residuals_SARIMA_valgt.png", width = 10, height = 5)
cat("Gemt!\n")
# ACF plot
p2 <- ggAcf(residualer) +
  labs(title = "") +
  theme_bw()
p2
ggsave("output/04_residuals_ACF.png", width = 10, height = 5)
cat("Gemt!\n")
# Histogram
p3 <- ggplot(dk1_resid, aes(x = resid)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "grey40") +
  stat_function(fun = dnorm,
                args = list(mean = mean(dk1_resid$resid),
                            sd   = sd(dk1_resid$resid)),
                colour = "red") +
  labs(x = "Residualer", y = "Density") +
  theme_bw()
p3
ggsave("output/04_residuals_histo.png", width = 10, height = 5)
cat("Gemt!\n")


# Ljung-Box test
ljung_df <- data.frame(
  lag     = 1:20,
  p_value = sapply(1:20, function(h) Box.test(residualer, lag = h, type = "Ljung-Box")$p.value)
)

ggplot(ljung_df, aes(x = lag, y = p_value)) +
  geom_point() +
  geom_hline(yintercept = 0.05, linetype = "dashed", colour = "blue") +
  labs(title = "",
       x     = "Lag",
       y     = "p-value") +
  theme_bw()
ggsave("output/04_ljung-box.png", width = 10, height = 5)
cat("Gemt!\n")