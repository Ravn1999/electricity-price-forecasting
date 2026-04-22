library(ggplot2)

# ── 1. Fitted volatilitet vs observerede priser ───────────────
sigma_t <- sigma(garch_valgt)

volatilitet_df <- data.frame(
  date     = dk1_daily$date[(nrow(dk1_daily) - length(sigma_t) + 1):nrow(dk1_daily)],
  sigma    = as.numeric(sigma_t),
  observed = dk1_daily$price[(nrow(dk1_daily) - length(sigma_t) + 1):nrow(dk1_daily)]
)

# To paneler
p1 <- ggplot(volatilitet_df, aes(x = date, y = observed)) +
  geom_line() +
  labs(x = "", y = "EUR/MWh") +
  theme_bw()

p2 <- ggplot(volatilitet_df, aes(x = date, y = sigma)) +
  geom_line() +
  labs(x = "Time", y = expression(sigma[t])) +
  theme_bw()

library(gridExtra)
grid.arrange(p1, p2, nrow = 2)

ggsave("output/07_garch_volatilitet.png", 
       grid.arrange(p1, p2, nrow = 2),
       width = 10, height = 8)
cat("Gemt!\n")

# ── 2. Standardiserede residualer ────────────────────────────
std_resid <- as.numeric(residuals(garch_valgt, standardize = TRUE))

std_resid_df <- data.frame(
  date  = dk1_daily$date[(nrow(dk1_daily) - length(std_resid) + 1):nrow(dk1_daily)],
  resid = std_resid
)

# Run-sequence plot
p1 <- ggplot(std_resid_df, aes(x = date, y = resid)) +
  geom_line() +
  labs(x = "Time", y = "Standardised Residuals") +
  theme_bw()
p1
ggsave("output/07_std_residuals.png", width = 10, height = 5)
cat("Gemt!\n")

# ACF plot
p2 <- ggAcf(std_resid) +
  labs(title = "") +
  theme_bw()
p2
ggsave("output/07_std_residuals_ACF.png", width = 10, height = 5)
cat("Gemt!\n")

install.packages("metRology")

library(metRology)  # eller brug dt() fra base R

# Histogram med Student-t kurve
p3 <- ggplot(std_resid_df, aes(x = resid)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "grey40") +
  stat_function(fun = function(x) dt(x, df = 5.238),
                colour = "red") +
  labs(x = "Standardised Residuals", y = "Density") +
  theme_bw()
p3
ggsave("output/07_std_residuals_histo.png", width = 10, height = 5)
cat("Gemt!\n")