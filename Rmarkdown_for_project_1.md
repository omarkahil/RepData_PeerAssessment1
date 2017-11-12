---
title: "Assessment 1"
author: "Omar"
date: "November 12, 2017"
output: 
  html_document: 
    keep_md: yes
---



##Project 1

This document is a submission for the first project under the Reproducible Research course

### Loading Data


```r
filedata<-read.csv("./work.csv")
filedata$date<-as.Date(filedata$date, format = "%m/%d/%Y")
```

### Mean Total Number of Steps Taken per Day


```r
stepsbyday<- tapply(filedata$steps, filedata$date, sum, na.rm=TRUE)
library("ggplot2")
qplot(stepsbyday,geom="histogram",main="Total Number of Steps per Day",xlab="Date",ylab = "Frequency",binwidth=500)
```

![](Rmarkdown_for_project_1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
Meansteps<-mean(stepsbyday)
Mediansteps<-median(stepsbyday)
```
- Mean per Day is 9354.2295082
- Median per Day is 10395

### Average Daily Activity Pattern


```r
avgint<-tapply(filedata$steps,filedata$interval,mean,na.rm=TRUE)
plot(names(avgint),avgint,xlab = "5 Min Interval", ylab = "Average Steps",type="l")
```

![](Rmarkdown_for_project_1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
maxavg<- max(avgint)
maxinterval<- as.numeric(names(avgint)[which(avgint==max(avgint))])
maxinterval<-gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", maxinterval)
```

- Interval with Average Max Steps 8:35

### Imputing Missing Values


```r
numiss<-sum(is.na(filedata$steps))
imputedata<-filedata
imputedata$steps[which(is.na(filedata$steps))]<-as.vector(avgint[as.character(filedata[which(is.na(filedata$steps)),3])])
stepseachday<- tapply(imputedata$steps, imputedata$date, sum, na.rm=TRUE)
qplot(stepseachday, xlab="No. of Steps Taken Each Day", ylab="Total Frequency", binwidth=500)
```

![](Rmarkdown_for_project_1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
medianEachDayImputed<- median(stepseachday)
meanEachDayImputed<- mean(stepseachday)
```
- Mean Total No. of Steps Taken Per Day 1.0766189\times 10^{4}
- Median Total No. of Steps Taken Per Day 1.0766189\times 10^{4}

Both mean and median of imputed data are higher than the ones calculated by removing missing values; imputed missing values has created a normal distribution where the mean equals the median

### Differences in Activity Patterns between Weekdays and Weekends


```r
imputedata$dayType<- ifelse(as.POSIXlt(imputedata$date)$wday %in% c(0,6), "weekends","weekdays")
aggregateData<- aggregate(steps ~ interval + dayType, data=imputedata, mean)
ggplot(aggregateData, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dayType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
```

![](Rmarkdown_for_project_1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
