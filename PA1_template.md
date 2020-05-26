---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv", header = TRUE)

## What is mean total number of steps taken per day?
totalsteps <- aggregate(steps ~ date, activity, sum)
hist(totalsteps$steps, main="Total Number of Steps per Day", col = "blue" ,xlab= "Number of Steps" )
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)
## What is mean total number of steps taken per day?
#Calculate and report the mean and median of the total number of steps taken per day

```r
round(mean(totalsteps$steps)
)
```

```
## [1] 10766
```

```r
median(totalsteps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
library(ggplot2)
meanstep <- aggregate(steps ~ interval, activity, mean)
ggplot(data = meanstep, aes(x=interval, y= steps))+geom_line(color="blue")+ggtitle("Average Daily Activity Pattern")+xlab("5-minute Interval")+ylab("Average Number of Steps")+theme(plot.title = element_text(hjust = 0.5))
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)

```r
maxstep <- meanstep[which.max(meanstep$steps),]
```

## Imputing missing values
#Calculate and report the total number of missing values in the dataset

```r
missing_act <- activity[!complete.cases(activity), ]
nrow(missing_act)
```

```
## [1] 2304
```


```r
missval <- is.na(activity$steps)
meanstep <- aggregate(steps ~ interval, activity, mean)
filledindata <- transform(activity, steps=ifelse(is.na(activity$steps),meanstep$steps[match(activity$interval,meanstep$interval)], activity$steps ))
filledin <- aggregate(steps ~ date, filledindata, FUN = sum)
hist(filledin$steps, main= "Total Numbers of Steps per Day - Imputed", col="green" ,xlab = "Number of Steps")
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png)

#Calculate and report the mean and median total number of steps taken per day

```r
round(mean(filledin$steps))
```

```
## [1] 10766
```

```r
median(filledin$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
meanstep <- aggregate(steps ~ interval, activity, mean)
filledindata <- transform(activity, steps=ifelse(is.na(activity$steps),meanstep$steps[match(activity$interval,meanstep$interval)], activity$steps ))
type <- function(date)  {day <- weekdays(date)
                        if (day %in% c('Monday','Tuesday', 'Wednesday','Thursday','Friday'))
                                return("weekday")
                                else if (day %in% c('Saturday','Sunday'))
                                        return("weekend")
                                else
                                        stop ("Invalid Date Format.")}
filledindata$date <- as.Date(filledindata$date)
filledindata$day <- sapply(filledindata$date, FUN = type)
meanstepbyday <- aggregate(steps ~ interval + day, filledindata, mean)
ggplot(data = meanstepbyday, aes(x=interval, y=steps))+theme_bw() +geom_line(color="blue")+facet_grid(day ~.)+ggtitle("Activity Pattern")+xlab("Interval")+ylab("Number of Steps")+theme(plot.title = element_text(hjust = 0.5))
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19-1.png)
#file

```r
library(knitr)
rmarkdown::render('PA1_template.Rmd')
```

```
## 
## 
## processing file: PA1_template.Rmd
```

```
##   |                                                                                                                                       |                                                                                                                               |   0%  |                                                                                                                                       |........                                                                                                                       |   6%
##   ordinary text without R code
## 
##   |                                                                                                                                       |................                                                                                                               |  12%
## label: unnamed-chunk-21
```

```
##   |                                                                                                                                       |........................                                                                                                       |  19%
##   ordinary text without R code
## 
##   |                                                                                                                                       |................................                                                                                               |  25%
## label: unnamed-chunk-22
##   |                                                                                                                                       |........................................                                                                                       |  31%
##   ordinary text without R code
## 
##   |                                                                                                                                       |................................................                                                                               |  38%
## label: unnamed-chunk-23
```

```
##   |                                                                                                                                       |........................................................                                                                       |  44%
##   ordinary text without R code
## 
##   |                                                                                                                                       |................................................................                                                               |  50%
## label: unnamed-chunk-24
##   |                                                                                                                                       |.......................................................................                                                        |  56%
##   ordinary text without R code
## 
##   |                                                                                                                                       |...............................................................................                                                |  62%
## label: unnamed-chunk-25
```

```
##   |                                                                                                                                       |.......................................................................................                                        |  69%
##   ordinary text without R code
## 
##   |                                                                                                                                       |...............................................................................................                                |  75%
## label: unnamed-chunk-26
##   |                                                                                                                                       |.......................................................................................................                        |  81%
##   ordinary text without R code
## 
##   |                                                                                                                                       |...............................................................................................................                |  88%
## label: unnamed-chunk-27
```

```
##   |                                                                                                                                       |.......................................................................................................................        |  94%
##   ordinary text without R code
## 
##   |                                                                                                                                       |...............................................................................................................................| 100%
## label: unnamed-chunk-28
```

```
## Quitting from lines 71-73 (PA1_template.Rmd)
```

```
## Error in sink(con, split = debug): sink stack is full
```
