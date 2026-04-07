dk1_daily$month_name <- factor(format(dk1_daily$date, "%b"),
                               levels = c("Jan","Feb","Mar","Apr","May","Jun",
                                          "Jul","Aug","Sep","Oct","Nov","Dec"))
dk1_daily$weekday_name <- factor(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")[as.integer(format(dk1_daily$date, "%u"))],
                                 levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
dk1$hour <- as.integer(format(dk1$time, "%H"))


# Avg by Month
month_avg <- aggregate(price ~ month_name, dk1_daily, mean)
plot(1:12, month_avg$price, type = "b", pch = 16,
     main = "Avg by Month", xlab = "", ylab = "EUR/MWh", xaxt = "n")
axis(1, at = 1:12, labels = levels(dk1_daily$month_name))
dev.copy(png, "output/02_season_month_line.png", width = 1000, height = 500)
dev.off()

# Avg by Weekday
weekday_avg <- aggregate(price ~ weekday_name, dk1_daily, mean)
plot(1:7, weekday_avg$price, type = "b", pch = 16,
     main = "Avg by Weekday", xlab = "", ylab = "EUR/MWh", xaxt = "n")
axis(1, at = 1:7, labels = levels(dk1_daily$weekday_name))
dev.copy(png, "output/02_season_weekday_line.png", width = 1000, height = 500)
dev.off()

# Avg by Hour
hour_avg <- aggregate(price ~ hour, dk1, mean)
plot(hour_avg$hour, hour_avg$price, type = "b", pch = 16,
     main = "Avg by Hour", xlab = "", ylab = "EUR/MWh")
dev.copy(png, "output/02_season_hour_line.png", width = 1000, height = 500)
dev.off()

# Distribution by Month
boxplot(price ~ month_name, data = dk1_daily,
        main = "Distribution by Month", xlab = "", ylab = "EUR/MWh")
dev.copy(png, "output/02_season_month_box.png", width = 1000, height = 500)
dev.off()

# Distribution by Weekday
boxplot(price ~ weekday_name, data = dk1_daily,
        main = "Distribution by Weekday", xlab = "", ylab = "EUR/MWh")
dev.copy(png, "output/02_season_weekday_box.png", width = 1000, height = 500)
dev.off()

# Distribution by Hour
boxplot(price ~ hour, data = dk1,
        main = "Distribution by Hour", xlab = "Hour", ylab = "EUR/MWh")
dev.copy(png, "output/02_season_hour_box.png", width = 1000, height = 500)
dev.off()

# installer hvis nødvendigt
install.packages("forecast")
library(forecast)

dk1_ts <- msts(dk1_daily$price, seasonal.periods = c(7, 365),
               start = 2018)

dk1_mstl <- mstl(dk1_ts)

plot(dk1_mstl, main = "DK1 - MSTL Decomposition", nc = 1)
dev.copy(png, "output/02_mstl.png", width = 1000, height = 1200)
dev.off()

#ACF & PACF

# Box-Cox transformation (lambda = 0.5)
min_price <- min(dk1_daily$price)
dk1_bc <- sqrt(dk1_daily$price + abs(min_price) + 1)

# Differensering (lag 1)
dk1_diff <- diff(dk1_daily$price, lag = 1)
dk1_bc_diff <- diff(dk1_bc, lag = 1)

# Datoer efter differensering
dates_diff <- dk1_daily$date[-1]

# Plot 1: Original
par(mfrow = c(3, 1))
plot(dk1_daily$date, dk1_daily$price, type = "l",
     main = "Original Series", xlab = "Time", ylab = "Price (EUR)")
acf(dk1_daily$price, lag.max = 50, main = "ACF of Original Series")
pacf(dk1_daily$price, lag.max = 50, main = "PACF of Original Series")
dev.copy(png, "output/02_acf_pacf_raw.png", width = 1500, height = 500)
dev.off()

# Plot 2: Differenseret
par(mfrow = c(3, 1))
plot(dates_diff, dk1_diff, type = "l",
     main = "Differenced Series", xlab = "Time", ylab = "Price (EUR)")
acf(dk1_diff, lag.max = 50, main = "ACF of Differenced Series")
pacf(dk1_diff, lag.max = 50, main = "PACF of Differenced Series")
dev.copy(png, "output/02_acf_pacf_diff.png", width = 1500, height = 500)
dev.off()

# Plot 3: Box-Cox + differenseret
par(mfrow = c(3, 1))
plot(dates_diff, dk1_bc_diff, type = "l",
     main = "Box-Cox-Transformed and Differenced Series",
     xlab = "Time", ylab = "Price (EUR)")
acf(dk1_bc_diff, lag.max = 50, main = "ACF of Box-Cox-Transformed and Differenced Series")
pacf(dk1_bc_diff, lag.max = 50, main = "PACF of Box-Cox-Transformed and Differenced Series")
dev.copy(png, "output/02_acf_pacf_bc_diff.png", width = 1500, height = 500)
dev.off()

par(mfrow = c(1, 1))

#unit root testing
install.packages("tseries")
install.packages("tidyr")
install.packages("gridExtra")
install.packages("gtable")

library(tseries)
library(gridExtra)
library(grid)
library(gtable)

serier <- list(
  "Original Series"            = dk1_daily$price,
  "Differenced Series"         = dk1_diff,
  "Box-Cox-Transformed Series" = dk1_bc_diff
)

# Kør tests
rows <- list()
for (test_navn in c("ADF", "PP", "KPSS")) {
  stat_row   <- c(Test = test_navn, Type = "Statistic")
  lag_row    <- c(Test = "",        Type = "Lag order")
  pval_row   <- c(Test = "",        Type = "p-value")
  result_row <- c(Test = "",        Type = "Result")
  
  for (navn in names(serier)) {
    x <- serier[[navn]]
    
    res <- switch(test_navn,
                  "ADF"  = adf.test(x),
                  "PP"   = pp.test(x),
                  "KPSS" = kpss.test(x)
    )
    
    stat <- round(res$statistic, 4)
    pval <- res$p.value
    lag  <- ifelse(!is.null(res$parameter), as.character(res$parameter), "-")
    
    if (test_navn == "KPSS") {
      pval_str   <- ifelse(pval >= 0.1,  "> 0.1",
                           ifelse(pval <= 0.01, "< 0.01", round(pval, 4)))
      result_str <- ifelse(pval > 0.05, "Stationary", "Non-stationary")
    } else {
      pval_str   <- ifelse(pval <= 0.01, "< 0.01",
                           ifelse(pval >= 0.1,  "> 0.1",  round(pval, 4)))
      result_str <- ifelse(pval < 0.05, "Stationary", "Non-stationary")
    }
    
    stat_row[navn]   <- as.character(stat)
    lag_row[navn]    <- lag
    pval_row[navn]   <- pval_str
    result_row[navn] <- result_str
  }
  rows <- c(rows, list(stat_row, lag_row, pval_row, result_row))
}

# Byg data frame
df <- do.call(rbind, lapply(rows, function(r) as.data.frame(t(r), stringsAsFactors = FALSE)))
colnames(df) <- c("Test", "Type", names(serier))

# Lav tabel
tbl <- tableGrob(df, rows = NULL,
                 theme = ttheme_minimal(
                   colhead = list(fg_params = list(fontface = "bold"),
                                  bg_params = list(fill = "white")),
                   core    = list(bg_params = list(fill = "white"))
                 )
)

# Gem som billede
png("output/03_stationarity_tests.png", width = 1100, height = 500, res = 100)
grid.newpage()

# Vandrette linjer: top, under header, efter hver testblok, bund
hlines <- c(1, 5, 9)

for (y in hlines) {
  tbl <- gtable_add_grob(tbl,
                         grobs = segmentsGrob(x0 = 0, x1 = 1, y0 = 0, y1 = 0,
                                              gp = gpar(lwd = 1.5)),
                         t = y, b = y, l = 1, r = 5
  )
}

# Lodrette linjer
for (x in 1:4) {
  tbl <- gtable_add_grob(tbl,
                         grobs = segmentsGrob(x0 = 1, x1 = 1, y0 = 0, y1 = 1,
                                              gp = gpar(lwd = 1.5)),
                         t = 1, b = 13, l = x, r = x
  )
}

grid.draw(tbl)
dev.off()

cat("Gemt: output/03_stationarity_tests.png\n")
