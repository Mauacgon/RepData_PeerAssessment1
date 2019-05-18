---
title: "Assingment 1, Course 5"
author: "Mauro Nicolás Acuña González"
date: "18 may 2019"
output: 
  html_document: 
    keep_md: yes
---

#Download Files, packages, unzip and load them


```r
#Download files & set wd

DestFile <- paste(getwd(), "/Assingnment_5_1.zip", sep = "")

URL <- ("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip")

if (!file.exists("Assingnment_5_1.zip")) {
  download.file(url = URL, destfile = DestFile)
}

#Download required packages and install them if didn't yet

packages <- list("lubridate", "data.table", "tidyr", "dplyr", "lubridate", "ggplot2")

for (i in packages){
  if (is.na(match(i, rownames(installed.packages())))) {
    install.packages(i)
  }
}

#Load Packages

for (i in packages) {
  library(i, character.only = TRUE)
}
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```
## Warning: package 'data.table' was built under R version 3.5.2
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     hour, isoweek, mday, minute, month, quarter, second, wday,
##     week, yday, year
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:data.table':
## 
##     between, first, last
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
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
#Unzip file

unzip(DestFile)
```

#Read and process data


```r
#Read and process data

rawdata <- read.table(file = "activity.csv",  sep = ",", header = TRUE)
```

#First question: What is the mean total number of steps taken per day?


```r
#First question: What is the mean total number of steps taken per day?

#Take out the NA's

Nonadata <- rawdata[!is.na(rawdata),]
```

- **Calculate the sum of steps taken per day**


```r
daysum <- tapply(Nonadata$steps, Nonadata$date, sum)
daysum <- daysum[!is.na(daysum)]
daysum
```

```
## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##        126      11352      12116      13294      15420      11015 
## 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
##      12811       9900      10304      17382      12426      15098 
## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 
##      10139      15084      13452      10056      11829      10395 
## 2012-10-21 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 
##       8821      13460       8918       8355       2492       6778 
## 2012-10-27 2012-10-28 2012-10-29 2012-10-30 2012-10-31 2012-11-02 
##      10119      11458       5018       9819      15414      10600 
## 2012-11-03 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-11 
##      10571      10439       8334      12883       3219      12608 
## 2012-11-12 2012-11-13 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
##      10765       7336         41       5441      14339      15110 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 
##       8841       4472      12787      20427      21194      14478 
## 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      11834      11162      13646      10183       7047
```

- **Histogram of the total number of steps taken per day**


```r
hist(daysum, breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

- **Calculate the mean and the median of the total number of steps taken per day**


```r
#Mean
mean(daysum)
```

```
## [1] 10766.19
```

```r
#Median
median(daysum)
```

```
## [1] 10765
```

#What is the average daily activity pattern?

- **Average plot of steps taken across days for every interval**


```r
#Second question: What is the average dailty pattern?

 #Average plot of steps taken across days for every interval

daypattern <- tapply(Nonadata$steps, Nonadata$interval, mean)
daypattern <- data.frame(as.numeric(names(daypattern)), daypattern)
names(daypattern) <- c("interval", "daypattern")
daypattern$interval <- as.numeric(daypattern$interval)
daypattern$daypattern <- as.numeric(daypattern$daypattern)
ggplot(daypattern, aes(interval, daypattern)) +  geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

- **Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**


```r
maxdaypatt <- max(daypattern$daypattern)
daypattern[daypattern$daypattern == 206, 1]
```

```
## numeric(0)
```

#Imputing missing values

- **Total number of missing values in the dataset** 


```r
#Imputing missing values

  #Total number of NA's

length(rawdata$steps[is.na(rawdata$steps)])
```

```
## [1] 2304
```

- **Fill in all the NA's with the mean of that interval**


```r
  #Fill in all the NA's with the mean of that interval

intervals <- unique(rawdata$interval)
datafi <- rawdata

for (i in intervals) {
  datafi$steps[is.na(datafi$steps) & datafi$interval == i] = daypattern[daypattern$interval == i,2]
}
```

- **Histogram of the new dataset**


```r
  #Histogram of the new dataset

fidaysum <- tapply(datafi$steps, datafi$date, sum)

hist(fidaysum, breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

- **Mean:**


```r
mean(fidaysum)
```

```
## [1] 10766.19
```

- **Median:**


```r
median(fidaysum)
```

```
## [1] 10766.19
```

# Any differences in pattern between weekdays and weekdays?

- **New factor variable in the dataset with two levels - "weekday" and "weekend"**


```r
#Any differences in pattern between weekdays and weekdays?

 # Create a new factor variable in the dataset with two levels - "weekday" and "weekend"      indicating whether a given date is a weekday or weekend day.

datafi <- mutate(datafi, weekday = wday(datafi$date))
datafi <- mutate(datafi, weekeorweekd = 1)
datafi$weekeorweekd[datafi$weekday == 7 | datafi$weekday == 1] = "weekend"
datafi$weekeorweekd[datafi$weekeorweekd == 1] = "weekday"
```
 
- **Panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)**


```r
datafi <- mutate(datafi, weekday = wday(datafi$date))
datafi <- mutate(datafi, weekeorweekd = 1)
datafi$weekeorweekd[datafi$weekday == 7 | datafi$weekday == 1] = "weekend"
datafi$weekeorweekd[datafi$weekeorweekd == 1] = "weekday"
datafi$weekeorweekd <- as.factor(datafi$weekeorweekd)
splitdatafi <- split(datafi, datafi$weekeorweekd)
splitdatafiweekend <- tapply(X = splitdatafi$weekend$steps, INDEX =  as.factor(splitdatafi$weekend$interval),FUN = mean)
splitdatafiweekday <- tapply(X = splitdatafi$weekday$steps, INDEX =  as.factor(splitdatafi$weekday$interval),FUN = mean)
splitdatafiweekend <- data.frame(as.numeric(row.names(splitdatafiweekend)), splitdatafiweekend)
names(splitdatafiweekend) <- c("interval", "steps_mean")
splitdatafiweekday <- data.frame(as.numeric(row.names(splitdatafiweekday)), splitdatafiweekday)
names(splitdatafiweekday) <- c("interval", "steps_mean")
par(mfrow = c(1,2))
with(splitdatafiweekday, plot(interval, steps_mean, type = "l", main = "Weekdays"))
with(splitdatafiweekend, plot(interval, steps_mean, type = "l", main = "Weekends"))
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

- We can see that definetely yes, there are differences in patterns between weekends and weekdays. There isn't a pronounced peak on weekends as it is on weekdays at aprox 3:00pm and the distribution of steps is more even on weekends in general.
