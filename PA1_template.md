
---
title: "Peer-graded Assignment: Course Project 1 "
author: Mari van Lunenburg
date: July 10, 2017
output: md_document
---


In this report the process and results are shown of the analysis of activity 
monitor data. The data is collected in October and November 2012 of a single 
subject. Every 5 minute interval the number of steps taken in that interval 
are collected. In the following steps the data is loaded, processed and 
analysed.

##Loading and preprocessing the data

1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your 
analysis

```{r load, echo=TRUE}
activity <- read.csv2("Activity.csv", header=TRUE, sep= ",", fill = TRUE)
activity$date <- as.Date(as.character(activity$date, format = "%m/%d/%Y"))
```

## Analyzing and reporting the data 
###What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day
```{r steps/day, echo=TRUE}
stepsDate <- aggregate(steps ~ date, activity, sum)
```

2. Make a histogram of the total number of steps taken each day 
```{r histogram steps/day, echo=TRUE}
hist(stepsDate$steps, main ="Total Steps Each Day", xlab="Number of Steps")
```

3. Calculate and report the mean and median of the total number of steps taken 
per day
```{r mean and median, echo=TRUE}
stepsDatemean <- mean(stepsDate$steps)
stepsDatemedian <- median(stepsDate$steps)
``` 
The mean number of total steps taken is: `r stepsDatemean` and the median of 
total steps taken is: `r stepsDatemedian`.

###What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r steps mean, echo=TRUE}
#first calculate average number of steps taken each day
stepsMean <- aggregate(steps ~ interval, activity, mean)
plot(stepsMean, type= "l", main='Average number of steps per day')
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r steps max, echo=TRUE}
stepsMax <- stepsMean[which.max(stepsMean$steps),1]
```

The interval with on average the maximum number of steps is `r stepsMax`.

###Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r NAs, echo=TRUE}
SumNAs <- sum(is.na(activity))
```

The total number of missing values is: `r SumNAs`. 

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
Both steps are performed in one go.

```{r impute NAs, echo=TRUE}
library(Hmisc)
# creating new data set
activityImputed <- activity 
# imputing NA's with median value for that day, i.e. default. 
activityImputed$steps <- impute(activity$steps)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r hist2, echo=TRUE}
# configure to show numbers not scientific and with only 2 digits after the . 
options(scipen = 999, digits = "2")
stepsDateImputed <- aggregate(steps ~ date, activityImputed, sum)
stepsDateImputedmean <- mean(stepsDateImputed$steps)
stepsDateImputedmedian <- median(stepsDateImputed$steps)
hist(stepsDateImputed$steps, main ="Total Steps Each Day", col="blue", xlab="Number of Steps")
```

After imputation of NA's by de median of total steps that day, the mean number of total steps taken is: `r stepsDateImputedmean` and the median of total steps taken is: `r stepsDateImputedmedian`.Versus respectively `r stepsDatemean` and `r stepsDatemedian`, beforing imputing.
The impact of imputing is that both the mean and median decrease after imputing the NA's with/for de median number of steps taken that day. It actually adds some days with little or no steps with zero tp the dataset.

###Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r weekday, echo=TRUE}
activityImputed$dateType <- ifelse(as.POSIXlt(activityImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
```{r panel plots, echo=TRUE}
# create average per date type
activityImputedMean <- aggregate(steps~interval+ dateType, data=activityImputed, mean)
par(mfrow=c(1,2))
ggplot(activityImputedMean, aes(interval, steps)) +  geom_line() + facet_grid(dateType ~ .) +xlab("5-minute interval") +  ylab("Average number of steps")
```




