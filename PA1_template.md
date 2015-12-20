# R Markdown Project 1




```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)

myData <- read.csv("activity.csv", header = T)

myData$date <- as.Date(myData$date)

group <- group_by(myData, date)

myTotal <- summarize(group, total = sum(steps, na.rm = T))

hist(myTotal$total, main = "Histogram of Total Daily Steps Taken", xlab = "Total Daily Steps" )
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png) 

```r
myMean <-  mean(myData$steps, na.rm = T)
myMedian <- median(myData$steps, na.rm = T)
```

The mean of the steps taken per day is 37.3825996. 
The median of the steps taken per day is 0. 


```r
intGroup <- group_by(myData, interval)
myInterval <- summarize(intGroup, intMean = mean(steps, na.rm = T))

plot(myInterval$interval, myInterval$intMean, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
maxRow <- myInterval[which.max(myInterval$intMea), ]
```

The interval with the most steps on average is 835.


```r
nas <- myData$steps[is.na(myData$steps)]
numNas <- length(nas)
```
The number of NA steps is 2304.
the NA steps will be filled in with the daily mean.
Below is the histogram with the missing values filled in.


```r
#uses group (above)
myMean <- summarize(group, dayMean = mean(steps, na.rm = T))
#if there are NAs in the results
myMean[is.na(myMean)] <- 0
#going to use the average of the interval, 
myMerge <- merge(myData, myMean, by.x = "date", by.y = "date", all = T)

repNa <- function(x){
  
  value <- as.integer(x[4])
  steps <- x[2]
  if(is.na(steps)) {
    value
  }
  else{
    as.integer(steps)
  }
}

steps <- apply(myMerge, 1, repNa)

myMerge$steps <- steps
filledGroup <- group_by(myMerge, date)

filledTotal <- summarize(filledGroup, total = sum(steps))
hist(filledTotal$total, main = "Histogram of Total Daily Steps Taken", xlab = "Total Daily Steps" )
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
myFilledMean <-  mean(myMerge$steps, na.rm = T)
myFilledMedian <- median(myMerge$steps, na.rm = T)
```

The filled in values differ, but not significantly because there are only `r numNas' empty values.

Below is a graph comparing weekdays to weekends.


```r
withDayOfWeek <- transform(myMerge, dayofweek = weekdays(date))

weekendFun <- function (x){
  
  w <- as.character(x[5])
  if(w == "Saturday" | w == "Sunday"){
    "weekend"
  }
  else{
    "weekday"
  }
}

weekendorweekday <- apply(withDayOfWeek, 1, weekendFun)
withDayOfWeek <- cbind(withDayOfWeek, weekendorweekday)
rm(weekendorweekday)
weekendGroup <- group_by(withDayOfWeek, interval, weekendorweekday)

weekendAverage <- summarize(weekendGroup, average = mean(steps))

g <- ggplot(weekendAverage, aes(x=interval, y = average))

avgPlot <- g + geom_line()
avgPlot <- avgPlot + facet_grid(weekendorweekday~.)
avgPlot + labs(x = "Interval", y = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
