library(dplyr)
library(lattice)

data <- read.csv(unz("activity.zip", "activity.csv"),sep=",",na.strings = "NA",nrows=-1)


by_day <- data %>%
  group_by(date) %>%
  summarize(total_steps=sum(steps, na.rm = TRUE))

hist(by_day$total_steps,breaks=10,xlab="Steps each day", main="Number of steps taken each day")

avg_by_day <- mean(by_day$total_steps)

median_by_day <- median(by_day$total_steps)


by_5min <- data %>%
  group_by(interval) %>%
  summarize(avg_steps=mean(steps, na.rm = TRUE))

#barplot(by_5min$avg_steps)
plot(by_5min$interval,by_5min$avg_steps,type="l",xlab="Interval",ylab="Avregare steps",main="Average steps in interval")


max_avg_in_5min <- max(by_5min$avg_steps)

interval_with_max_avg <- by_5min[by_5min$avg_steps == max_avg_in_5min,]$interval


missing_values <- sum(is.na(data$steps))

imputed <- data %>%
  inner_join(by_5min,by=c("interval")) %>%
  transmute(interval, date, steps=ifelse(is.na(steps),round(avg_steps),steps))

by_day_imputed <- summarize(group_by(imputed,date),total_steps=sum(steps))

hist(by_day_imputed$total_steps,breaks=10,xlab="Steps each day", main="Number(imputed) of steps taken each day")

avg_by_day_imputed <- mean(by_day_imputed$total_steps)

median_by_day_imputed <- median(by_day_imputed$total_steps)


is_a_weekend <- function(date_str) {
  date <- as.POSIXlt(date_str,format="%Y-%m-%d")
  factor <- weekdays(date) %in% c("Sunday","Saturday")
  ifelse(factor,"weekend","workday")
}


weekend_factored <- mutate(imputed,day_type=is_a_weekend(date))

by5min_and_day_type <- weekend_factored %>%
  group_by(interval,day_type) %>%
  summarize(avg_steps=mean(steps))

xyplot(avg_steps ~ interval | day_type, data = by5min_and_day_type, type="l",layout=c(1,2))

