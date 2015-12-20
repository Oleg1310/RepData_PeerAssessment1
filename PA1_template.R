#Loading and preprocessing the data
activity = read.csv("activity.csv")
activity$date = as.Date(activity$date)

#What is mean total number of steps taken per day
data1 = na.omit(activity)
# list rows of data that have missing values
x = activity[!complete.cases(activity),]
unique(x$date)

#Calculate the total number of steps taken per day
totalStepsPerDay = tapply(data1$steps, data1$date, sum)
which(is.na(totalStepsPerDay)==TRUE)

#hist(totalStepsPerDay, col = "red", breaks = 12)
#Make a histogram of the total number of steps taken each day
barplot(totalStepsPerDay)

#Calculate and report the mean and median of the total number of steps taken per day
mean(totalStepsPerDay)
median(totalStepsPerDay)

#What is the average daily activity pattern?

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
avgStepInterval = tapply(data1$steps, data1$interval, mean)
Interval = as.integer(names(avgStepInterval))
plot(Interval, avgStepInterval, type = 'l')

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
as.integer(names(avgStepInterval[which(avgStepInterval == max(avgStepInterval))]))

#Imputing missing values

#Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
#number of days/intervals where there are missing values
data.na = activity[!complete.cases(activity),]
length(unique(data.na$date))
length(unique(data.na$interval))

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
nrow(data.na)

#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
data2 = activity
for (i in 1:nrow(data2)) {
  if (is.na(data2$steps[i])) {
    for (j in 1:length(avgStepInterval)) {
      if (as.integer(names(avgStepInterval)[j]) == data2$interval[i]) {
        data2$steps[i] = avgStepInterval[j]
      }
    }
  }
}

#Create a new dataset that is equal to the original dataset but with the missing data filled in.

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
totalStepsPerDay2 = tapply(data2$steps, data2$date, sum)
barplot(totalStepsPerDay2)
mean(totalStepsPerDay2)
median(totalStepsPerDay2)


#Are there differences in activity patterns between weekdays and weekends?

#For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
newVar = vector(mode = "factor", length = nrow(data2))
for (i in 1:nrow(data2)) {
  if (weekdays(data2$date[i]) == "воскресенье" | weekdays(data2$date[i]) == "суббота") {
    newVar[i] = "weekend"
  } else {
    newVar[i] = "weekday"
  }
}
data2$newVar = newVar

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
subWeekend = subset(data2, newVar == "weekend")
subWeekday = subset(data2, newVar == "weekday")

avgStepWeekend = tapply(subWeekend$steps, subWeekend$interval, mean)
intervalWeekend = as.integer(names(avgStepWeekend))
plot(intervalWeekend, avgStepWeekend, type = 'l')

avgStepWeekday = tapply(subWeekday$steps, subWeekday$interval, mean)
intervalWeekday = as.integer(names(avgStepWeekday))
plot(intervalWeekday, avgStepWeekday, type = 'l')
Transform into html.


library (knitr)
knit2html("PA1_template.Rmd")
browseURL("PA1_template.html")
