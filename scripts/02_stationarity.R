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

