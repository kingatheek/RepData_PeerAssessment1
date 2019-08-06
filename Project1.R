## Loading and preprocessing the data
fitnessdata <- read.csv("C:/MOAEK/14_Coursera/Reproducible_research_project1/activity.csv")

## What is mean total number of steps taken per day?
library(ggplot2)
totalsteps <- tapply(fitnessdata$steps, fitnessdata$date, FUN=sum, na.rm=TRUE)
qplot(totalsteps, binwidth=500, xlab="Total number of steps taken each day")
mean(totalsteps, na.rm=TRUE)
median(totalsteps, na.rm=TRUE)
#mean = 9354.23
#median = 10395

## What is the average daily activity pattern?
averagesteps <- aggregate(x=list(steps=fitnessdata$steps), by=list(interval=fitnessdata$interval), FUN=mean, na.rm=TRUE)
ggplot(data=averagesteps, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-min interval") +
    ylab("Average number of steps taken")
#Max average interval steps
averagesteps[which.max(averagesteps$steps),]
#Max. average steps at iterval 835 accounting to average of 206.1698

## Inputing missing values
missingdata <- is.na(fitnessdata$steps)
table(missingdata)
#missing data TRUE = 2304 and FALSE = 15264

# Replace each missing value with the mean value of its 5-minute interval
fillvalue <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averagesteps[averagesteps$interval==interval, "steps"])
    return(filled)
}
filleddata <- fitnessdata
filleddata$steps <- mapply(fillvalue, filleddata$steps, filleddata$interval)

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day
totalsteps <- tapply(filleddata$steps, filleddata$date, FUN=sum)
qplot(totalsteps, binwidth=500, xlab="total number of steps taken each day")
mean(totalsteps)
median(totalsteps)
#mean = 10766.19
#median = 10766.19

## Are there differences in activity patterns between weekdays and weekends?

difference <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filleddata$date <- as.Date(filleddata$date)
filleddata$day <- sapply(filleddata$date, FUN=difference)

#Average no. of steps taken on weekdays and weekends
averages <- aggregate(steps ~ interval + day, data=filleddata, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")

