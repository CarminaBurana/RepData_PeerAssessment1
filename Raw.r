# Loading and preprocessing the data
activity <- read.csv(unz("activity.zip", "activity.csv"))
activity_without_NAs <- activity[complete.cases(activity),]

# What is mean total number of steps taken per day?
steps <- activity_without_NAs$steps
steps_by_date <- sapply(
  split(activity_without_NAs, activity_without_NAs$date), 
  function(x) sum(x$steps))

barplot(steps_by_date, xlab = "Date", ylab = "Number of steps")

mean <- mean(steps_by_date, na.rm = TRUE)
median <- median(steps_by_date, na.rm = TRUE)

steps_by_date <- sapply(
  split(activity, activity$date), 
  function(x) sum(x$steps))

mean <- mean(steps_by_date, na.rm=TRUE)
median <- median(steps_by_date, na.rm=TRUE)

# What is the average daily activity pattern?
mean_steps_by_interval <- sapply(
  split(activity_without_NAs, activity_without_NAs$interval), 
  function(x) mean(x$steps))

x_labels <- as.numeric(labels(mean_steps_by_interval))
plot(x_labels, 
     mean_steps_by_interval, 
     type = "l", 
     xlab = "Time (minutes)", 
     ylab = "Number of steps")

interval_with_max_number_of_steps_on_avg <- labels(which.max(mean_steps_by_interval))

# Imputing missing values
count_of_missing_values <- 17568 - nrow(activity_without_NAs)

activity_with_filled_in_data <- activity

for(i in 1:nrow(activity_with_filled_in_data))
{ 
  if (is.na(activity_with_filled_in_data[i, "steps"]))
  {
    activity_with_filled_in_data[i, "steps"] <- mean_steps_by_interval[toString(activity_with_filled_in_data[i, "interval"])]
  }
}

steps_by_date <- sapply(
  split(activity_with_filled_in_data, activity_with_filled_in_data$date), 
  function(x) sum(x$steps))

barplot(steps_by_date, xlab = "Date", ylab = "Number of steps")

mean <- mean(steps_by_date, na.rm=TRUE)
median <- median(steps_by_date, na.rm=TRUE)

# Are there differences in activity patterns between weekdays and weekends?

activity_with_filled_in_data$date <- as.Date(activity_with_filled_in_data$date)

for (i in 1:nrow(activity_with_filled_in_data)) {
  if (weekdays(activity_with_filled_in_data[i,2]) == "Saturday" | weekdays(activity_with_filled_in_data[i,2]) == "Sunday") {
    activity_with_filled_in_data[i, 4] <- "weekend"
  }
  else {
    activity_with_filled_in_data[i, 4] <- "weekday"
  }
}

colnames(activity_with_filled_in_data)[4] <- "day_type"

weekday <- activity_with_filled_in_data[activity_with_filled_in_data$day_type == "weekday",]
avg_steps_weekday <- sapply(
  split(weekday, weekday$interval),
  function(x) mean(x$steps, na.rm = TRUE))
x_labels_weekday <- as.numeric(labels(avg_steps_weekday))

weekend <- activity_with_filled_in_data[activity_with_filled_in_data$day_type == "weekend",]
avg_steps_weekend <- sapply(
  split(weekend, weekend$interval),
  function(x) mean(x$steps, na.rm = TRUE))
x_labels_weekend <- as.numeric(labels(avg_steps_weekend))

par(mfrow = c(2, 1), oma = c(1, 1, 0, 1))

plot(x_labels_weekday, avg_steps_weekday, 
     main = "Weekday", type = "l", xlab = "", ylab = "")

plot(x_labels_weekend, avg_steps_weekend, 
     main = "Weekend", type = "l", xlab = "", ylab = "")

mtext("Time Interval", outer = TRUE, side = 1)
mtext("Number of Steps", outer = TRUE, side = 2)
