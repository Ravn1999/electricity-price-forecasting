# ── 1. Indlæs data ────────────────────────────────────────────
dk1_raw <- read.csv("data/DK1-2018-2025.csv", sep = ";")
no2_raw <- read.csv("data/NO2-2018-2025.csv", sep = ";")

# ── 2. Rens data ──────────────────────────────────────────────
clean_data <- function(df) {
  df$time  <- as.POSIXct(df$HourDK, format = "%Y-%m-%d %H:%M:%S")
  df$price <- as.numeric(gsub(",", ".", df$SpotPriceEUR))
  df       <- df[, c("time", "price")]
  df       <- df[order(df$time), ]
  return(df)
}

dk1 <- clean_data(dk1_raw)
no2 <- clean_data(no2_raw)

# ── 3. Dagligt gennemsnit ─────────────────────────────────────
dk1_daily <- aggregate(price ~ as.Date(time), dk1, mean)
no2_daily <- aggregate(price ~ as.Date(time), no2, mean)
names(dk1_daily) <- c("date", "price")
names(no2_daily) <- c("date", "price")

# ── 4. 30-dages rolling average ───────────────────────────────
rolling_avg <- function(x, n = 30) {
  filter(x, rep(1/n, n), sides = 2)
}

dk1_daily$rolling <- rolling_avg(dk1_daily$price)
no2_daily$rolling <- rolling_avg(no2_daily$price)

# ── 5. Plots ──────────────────────────────────────────────────
par(mfrow = c(1, 1))

# DK1 - Rå timeserie
plot(dk1$time, dk1$price, type = "l", col = "black",
     main = "DK1 - Raw Time Series", xlab = "Time", ylab = "EUR/MWh",
     xlim = as.POSIXct(c("2018-01-01", "2025-09-30")),
     xaxt = "n")
axis.POSIXct(1, at = as.POSIXct(paste0(2018:2025, "-01-01")), 
             labels = 2018:2025)
dev.copy(png, "output/01_dk1_raw.png", width = 1200, height = 400)
dev.off()

# NO2 - Rå timeserie
plot(no2$time, no2$price, type = "l", col = "black",
     main = "NO2 - Raw Time Series", xlab = "Time", ylab = "EUR/MWh",
     xlim = as.POSIXct(c("2018-01-01", "2025-09-30")),
     xaxt = "n")
axis.POSIXct(1, at = as.POSIXct(paste0(2018:2025, "-01-01")), 
             labels = 2018:2025)
dev.copy(png, "output/01_no2_raw.png", width = 1200, height = 400)
dev.off()

# DK1 - Dagligt gennemsnit
plot(dk1_daily$date, dk1_daily$price, type = "l", col = "black",
     main = "DK1 - Daily Average", xlab = "Time", ylab = "EUR/MWh",
     xlim = as.Date(c("2018-01-01", "2025-09-30")),
     xaxt = "n")
axis.Date(1, at = as.Date(paste0(2018:2025, "-01-01")), 
          labels = 2018:2025)
dev.copy(png, "output/01_dk1_daily.png", width = 1200, height = 400)
dev.off()

# NO2 - Dagligt gennemsnit
plot(no2_daily$date, no2_daily$price, type = "l", col = "black",
     main = "NO2 - Daily Average", xlab = "Time", ylab = "EUR/MWh",
     xlim = as.Date(c("2018-01-01", "2025-09-30")),
     xaxt = "n")
axis.Date(1, at = as.Date(paste0(2018:2025, "-01-01")), 
          labels = 2018:2025)
dev.copy(png, "output/01_no2_daily.png", width = 1200, height = 400)
dev.off()

# DK1 - 30-dages rolling average
plot(dk1_daily$date, dk1_daily$rolling, type = "l", col = "black",
     main = "DK1 - 30-Day Rolling Average", xlab = "Time", ylab = "EUR/MWh",
     xlim = as.Date(c("2018-01-01", "2025-09-30")),
     xaxt = "n")
axis.Date(1, at = as.Date(paste0(2018:2025, "-01-01")), 
          labels = 2018:2025)
dev.copy(png, "output/01_dk1_rolling.png", width = 1200, height = 400)
dev.off()

# NO2 - 30-dages rolling average
plot(no2_daily$date, no2_daily$rolling, type = "l", col = "black",
     main = "NO2 - 30-Day Rolling Average", xlab = "Time", ylab = "EUR/MWh",
     xlim = as.Date(c("2018-01-01", "2025-09-30")),
     xaxt = "n")
axis.Date(1, at = as.Date(paste0(2018:2025, "-01-01")), 
          labels = 2018:2025)
dev.copy(png, "output/01_no2_rolling.png", width = 1200, height = 400)
dev.off()