---
title: "Week 2 Assingment"
author: "Alan McPharlane"
date: "23 January 2018"
output: 
  html_document: 
    keep_md: yes
---



## Reproducible Reseach - Week 2 Assignment

This is my week 2 assignment

### Step 1

### Loading and preprocessing the data


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activitiesWithNas = read.csv("activity/activity.csv")
activities <- na.omit(activitiesWithNas)

agg <- aggregate(activities$steps, by=list(activities$date), sum)
agg$totalSteps <- agg$x
```


### Histogram of Total Number of Steps taken per day


```r
hist(agg$totalSteps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

The mean total number of steps taken each day is:

```r
mean(agg$totalSteps)
```

```
## [1] 10766.19
```
The median total number of steps taken each day is:

```r
median(agg$totalSteps)
```

```
## [1] 10765
```

### Average Daily Activity Pattern


```r
agg2 <- aggregate(activities$steps, list(activities$interval), mean)
agg2$averageStep <- agg2$x

agg2$interval = agg2$Group.1

plot(agg2$interval, agg2$averageStep, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
maxSteps <- agg2[which.max(agg2$averageStep), ]
```

The interval with the maximum number of steps on average across all the days in the dataset is:

```r
maxSteps[,1]
```

```
## [1] 835
```

### Imputing missing values


```r
nas <- is.na(activitiesWithNas)
numNas <- sum(nas)
```
The number of rows missing data is:

```r
numNas
```

```
## [1] 2304
```

To fill in missing values with the value for the previous recorded value


```r
for (i in 1:length(activitiesWithNas$steps)) {
  if (is.na(activitiesWithNas$steps[i])) {
    if (i == 1) {
      activitiesWithNas$steps[i] = 0
    }
    else {
      newSteps <- activitiesWithNas$steps[i - 1]
      activitiesWithNas$steps[i] <- newSteps
    }
  }
}

agg3 <- aggregate(activitiesWithNas$steps, by=list(activitiesWithNas$date), sum)
agg3$totalSteps <- agg3$x

hist(agg3$totalSteps)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
The mean total number of steps taken each day is (with missing values replaced):

```r
mean(agg3$totalSteps)
```

```
## [1] 9354.23
```
The median total number of steps taken each day is (with missing values replaced):

```r
median(agg3$totalSteps)
```

```
## [1] 10395
```

The impact of imputing missing data on the estimates of the total daily number of steps has reduced both the median and mean. This shows the potential impact of ignore missing data and/or imputing missing data with invalid values.


```r
activitiesWithNas$date <- as.Date(activitiesWithNas$date, "%Y-%m-%d")
activitiesWithNas$day <- weekdays(activitiesWithNas$date)
weekend <- c("Saturday", "Sunday")

for (i in 1:length(activitiesWithNas$day)) {
  if (activitiesWithNas$day[i] %in% weekend) {
    activitiesWithNas$day[i] <- "weekend"
  }
  else {
    activitiesWithNas$day[i] = "weekday"
  }
}

actWeekdays <- activitiesWithNas[activitiesWithNas$day == "weekday",]
actWeekend <- activitiesWithNas[activitiesWithNas$day == "weekend",]
agg4 <- aggregate(actWeekdays$steps, list(actWeekdays$interval), mean)
agg4$averageStep <- agg4$x

agg4$interval = agg4$Group.1

agg5 <- aggregate(actWeekend$steps, list(actWeekend$interval), mean)
agg5$averageStep <- agg5$x

agg5$interval = agg5$Group.1

par(mfrow=c(2,1))

plot(agg4$interval, agg4$averageStep, type="l", xlab = "Intervals", ylab = "Average Steps", main = "Weekdays")
plot(agg5$interval, agg5$averageStep, type="l", xlab = "Intervals", ylab = "Average Steps", main = "Weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


