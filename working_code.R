# Reproducible Research: Peer Assessment 1

# Loading and preprocessing the data
# - What is mean total number of steps taken per day?
# - What is the average daily activity pattern?
# - Imputing missing values
# - Are there differences in activity patterns between weekdays and weekends?


# Loading and preprocessing the data
## Source data is in activity.zip file

# The variables:
  
# -  steps: Number of steps made in a 5-minute interval (missing values are coded as NA)
# - date: The date on which the measurement was taken in YYYY-MM-DD format
# - interval: Identifier in minutes in which measurement was taken

# Unzip & Load the data
unzip('activity.zip')
data <- read.csv('activity.csv', header = TRUE)

# What is mean total number of steps taken per day?

## Calculate the total number of steps taken per day
data_per_day <- aggregate(steps ~ date, data=data, FUN = 'sum')

## Make a histogram of the total number of steps taken each day

hist(data_per_day$steps, xlab = 'Number of steps per day', ylab = 'Frequency (days)', 
     main = 'Distribution of steps per day 2012-10-02 - 2012-11-29', col='lightgreen', breaks=20)
## lines(data_per_day$date, data_per_day$steps)
## Calculate and report the mean and median of the total number of steps taken per day

data_per_day_mean <- mean(data_per_day$steps)
data_per_day_median <- median(data_per_day$steps)

print("The mean is :", data_per_day_mean)
print("The median is :", data_per_day_median)

# What is the average daily activity pattern?
##   Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, 
## averaged across all days (y-axis)
data_per_interval <- aggregate(steps ~ interval, data=data, FUN = 'mean')


plot(data_per_interval$interval, data_per_interval$steps, type="l", 
     xlab = "5 min - interval", ylab = "Average steps", main = "Average Daily Activity Pattern", col = "green")


## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

data_per_interval$interval[which.max(data_per_interval$steps)]

## Imputing missing values
## Note that there are a number of days/intervals where there are missing values (coded as NA). 
## The presence of missing days may introduce bias into some calculations or summaries of the data.

## Calculate and report the total number of missing values in the dataset 
## (i.e. the total number of rows with NAs)

sum(is.na(data))#, 'in dataset')


## Devise a strategy for filling in all of the missing values in the dataset. 
## The strategy does not need to be sophisticated. For example, you could use 
## the mean/median for that day, or the mean for that 5-minute interval, etc.

## If more than 100 NAs than the device was either off so then equal == 0
## If only single NA is missing then mean of n-1 + n + 1

## Create a new dataset that is equal to the original dataset but with the 
## missing data filled in.

data_no_NAs <- data
data_no_NAs[is.na(data$steps), "steps"] <- 0


## Make a histogram of the total number of steps taken each day and Calculate 
## and report the mean and median total number of steps taken per day. 

data_per_day_no_NAs <- aggregate(steps ~ date, data=data_no_NAs, FUN = 'sum')

hist(data_per_day_no_NAs$steps, xlab = 'Number of steps per day', ylab = 'Frequency (days)', 
     main = 'Distribution of steps per day 2012-10-02 - 2012-11-29', col='lightblue', breaks=20)

## Do these values differ from the estimates from the first part of the assignment? 

data_per_day_mean_no_NAs <- mean(data_per_day_no_NAs$steps)

data_per_day_median_no_NAs <- median(data_per_day_no_NAs$steps)

## What is the impact of imputing missing data on the estimates of the total daily number of steps?

data_per_day_mean - data_per_day_mean_no_NAs
data_per_day_median - data_per_day_median_no_NAs


## Are there differences in activity patterns between weekdays and weekends?
##  For this part the weekdays function may be of some help here. 
## Use the dataset with the filled-in missing values for this part.

## Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
## indicating whether a given date is a weekday or weekend day.

data_no_NAs$day <- as.POSIXlt(data_no_NAs$date)$wday
data_no_NAs$dayType <- as.factor(ifelse(data_no_NAs$day == 0 | data_no_NAs$day == 6, "weekend", "weekday"))

head(data_no_NAs)

## Make a panel plot containing a time series plot (type="l") of the 5-minute interval (x-axis) 
## and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
## See the README file in the GitHub repository to see an example of what this plot should look like 
## using simulated data.

weekday_Data <- data_no_NAs[data_no_NAs$dayType == "weekday",]
weekend_Data <- data_no_NAs[data_no_NAs$dayType == "weekend",]
data_per_weekday <- aggregate(steps ~ interval, weekday_Data, mean)
data_per_weekend <- aggregate(steps ~ interval, weekend_Data, mean)

par(mfrow = c(2, 1))

plot(data_per_weekday, type = "l", col = "green", main = "Weekdays")
plot(data_per_weekend, type = "l", col = "red", main = "Weekends")
