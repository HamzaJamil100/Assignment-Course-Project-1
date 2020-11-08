library(readxl)
library(dplyr)
library(tidyverse)

##loading the data 

data <- read.csv("activity.csv")  

Steps Each Day
steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = paste("Steps Each Day"), col="blue",xlab="Steps")
rmean <- mean(steps_by_day$steps)
rmean
rmedian <- median(steps_by_day$steps)
rmedian
* mean: 10766.19 
* median: 10765  

#average daily activity
steps_by_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Steps",main="Average Steps Per Day by Interval")
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
max_interval
* Interval with most steps: 835  

Imputing missing values  

NATotal <- sum(!complete.cases(data))
NATotal

StepsAverage <- aggregate(steps ~ interval, data = data, FUN = mean)
fillNA <- numeric()
for (i in 1:nrow(data)) {
  obs <- data[i, ]
  if (is.na(obs$steps)) {
    steps <- subset(StepsAverage, interval == obs$interval)$steps
  } else {
    steps <- obs$steps
  }
  fillNA <- c(fillNA, steps)
}
new_activity <- data
new_activity$steps <- fillNA

#histogram number of stepseach day 
StepsTotalUnion <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)
hist(StepsTotalUnion$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Steps")
#Create Histogram to show difference. 
hist(steps_by_day$steps, main = paste("Steps Each Day"), col="blue", xlab="Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("madrian", "blue"), lwd=10)
rmeantotal <- mean(StepsTotalUnion$steps)
rmeantotal
rmediantotal <- median(StepsTotalUnion$steps)
rmediantotal
# mean:10766.19
# median: 10766.19

#Do these values differ from the estimates from the first part of the assignment?  
rmeandiff <- rmeantotal - rmean
rmeandiff
rmediandiff <- rmediantotal - rmedian
rmediandiff


#differences in activity between weekdays and weekends?
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
new_activity$dow = as.factor(ifelse(is.element(weekdays(as.Date(new_activity$date)),weekdays), "Weekday", "Weekend"))
StepsTotalUnion <- aggregate(steps ~ interval + dow, new_activity, mean)
library(lattice)
xyplot(StepsTotalUnion$steps ~ StepsTotalUnion$interval|StepsTotalUnion$dow, main="Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
