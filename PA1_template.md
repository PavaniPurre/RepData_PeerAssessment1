---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(knitr)
library(ggplot2)
library(plyr)
setwd("C:/Users/Ashwath Nandan/Desktop/RepData_PeerAssessment1")
getwd()
```

```
## [1] "C:/Users/Ashwath Nandan/Desktop/RepData_PeerAssessment1"
```

```r
raw_data <- read.csv("activity/activity.csv")
activity_data <- na.omit(raw_data) #removing na
head(activity_data)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```
## What is mean total number of steps taken per day?
#### 1.Calculate the total number of steps taken per day

```r
steps_per_day <- aggregate(steps~date,activity_data,sum)  
head(steps_per_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
#### 2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
ggplot(steps_per_day,aes(x=steps))+
  geom_histogram(fill="blue",binwidth = 1000) +
  labs(title ="Histogram of the total no.of steps per day",x="no.of steps",y="frequency")
```

![](PA1_template_files/figure-html/plot histogram-1.png)<!-- -->

#### 3.Calculate and report the mean and median of the total number of steps taken per day

```r
round(mean(steps_per_day$steps))
```

```
## [1] 10766
```

```r
median(steps_per_day$steps)
```

```
## [1] 10765
```
## What is the average daily activity pattern?
#### 1.Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
step_by_interval <- aggregate(steps ~ interval, activity_data, mean)
head(step_by_interval)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
ggplot(step_by_interval,aes(x=interval,y=steps))+
  geom_line(size=1,color="red")+
  labs(title = "Avg daily steps of the 5 min interval",x="interval",y="Avg no.of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

#### 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
step_by_interval$interval[which.max(step_by_interval$steps)]
```

```
## [1] 835
```
The maximum number of steps is $206$ in the $835^{th}$ interval.

## Imputing missing values
#### 1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs) 

```r
sum(is.na(raw_data$steps))
```

```
## [1] 2304
```
#### 2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
# Fill in all of the missing values in the dataset using the the mean for that 5-minute interval, etc.
filled_data <- split(raw_data, raw_data$interval)
unique(raw_data$interval)
```

```
##   [1]    0    5   10   15   20   25   30   35   40   45   50   55  100  105  110
##  [16]  115  120  125  130  135  140  145  150  155  200  205  210  215  220  225
##  [31]  230  235  240  245  250  255  300  305  310  315  320  325  330  335  340
##  [46]  345  350  355  400  405  410  415  420  425  430  435  440  445  450  455
##  [61]  500  505  510  515  520  525  530  535  540  545  550  555  600  605  610
##  [76]  615  620  625  630  635  640  645  650  655  700  705  710  715  720  725
##  [91]  730  735  740  745  750  755  800  805  810  815  820  825  830  835  840
## [106]  845  850  855  900  905  910  915  920  925  930  935  940  945  950  955
## [121] 1000 1005 1010 1015 1020 1025 1030 1035 1040 1045 1050 1055 1100 1105 1110
## [136] 1115 1120 1125 1130 1135 1140 1145 1150 1155 1200 1205 1210 1215 1220 1225
## [151] 1230 1235 1240 1245 1250 1255 1300 1305 1310 1315 1320 1325 1330 1335 1340
## [166] 1345 1350 1355 1400 1405 1410 1415 1420 1425 1430 1435 1440 1445 1450 1455
## [181] 1500 1505 1510 1515 1520 1525 1530 1535 1540 1545 1550 1555 1600 1605 1610
## [196] 1615 1620 1625 1630 1635 1640 1645 1650 1655 1700 1705 1710 1715 1720 1725
## [211] 1730 1735 1740 1745 1750 1755 1800 1805 1810 1815 1820 1825 1830 1835 1840
## [226] 1845 1850 1855 1900 1905 1910 1915 1920 1925 1930 1935 1940 1945 1950 1955
## [241] 2000 2005 2010 2015 2020 2025 2030 2035 2040 2045 2050 2055 2100 2105 2110
## [256] 2115 2120 2125 2130 2135 2140 2145 2150 2155 2200 2205 2210 2215 2220 2225
## [271] 2230 2235 2240 2245 2250 2255 2300 2305 2310 2315 2320 2325 2330 2335 2340
## [286] 2345 2350 2355
```

```r
for(i in 1:288) {
  filled_data[[i]]$steps[is.na(filled_data[[i]]$steps)]=step_by_interval$steps[i]
}
```
#### 3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
new_data <- ldply(filled_data)[,-1]
```
#### 4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
# total number of steps taken per day
new_step_per_day <- aggregate(steps ~ date, new_data, sum)
ggplot(new_step_per_day,aes(x=steps))+
  geom_histogram(fill="orange",binwidth = 1000) +
  labs(title = "Histogram of the Total No.of Steps Per Day",x = "Steps Per Day")
```

![](PA1_template_files/figure-html/plot new data-1.png)<!-- -->

```r
# mean and median total number of steps taken per day for new data(imputed data)
round(mean(new_step_per_day$steps))
```

```
## [1] 10766
```

```r
median(new_step_per_day$steps)
```

```
## [1] 10766.19
```
The new mean total number of steps taken per day is $10,766$ and the new median is $10,766.19$.
The new mean and median do NOT differ from the estimates from the first part of the assignment. The imputed data does not create obvious impact.

## Are there differences in activity patterns between weekdays and weekends?
#### 1.Create a new factor variable in the dataset with two levels –“weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
# Create a function to check day in week.
checkDayInWeek <- function(date) {
  d <- weekdays(as.Date(date, '%Y-%m-%d'))
  if  (!(d == 'Saturday' || d == 'Sunday')) {
    return('Weekday') 
  } else {
    return('Weekend')
  }
}
# Create a new factor variable  with two levels – “weekday” and “weekend”.
new_data$day <- as.factor(sapply(new_data$date, checkDayInWeek))

# Take a look at new_data.
head(new_data)
```

```
##       steps       date interval     day
## 1  1.716981 2012-10-01        0 Weekday
## 2  0.000000 2012-10-02        0 Weekday
## 3  0.000000 2012-10-03        0 Weekday
## 4 47.000000 2012-10-04        0 Weekday
## 5  0.000000 2012-10-05        0 Weekday
## 6  0.000000 2012-10-06        0 Weekend
```

```r
## Calculate the average number of steps per interval by day type.
steps_by_day_type <- aggregate(steps ~ interval+day, new_data, mean)
```
#### 2.Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
panel_plot <- ggplot(steps_by_day_type, aes(x=interval, y=steps)) +
  geom_line(stat = "identity", aes(colour = day)) +
  theme_gray() +
  facet_grid(day ~ ., scales="fixed", space="fixed") +
  labs(x="5-minute Interval Indexl", y=expression("Average No. of Steps")) +
  ggtitle("No. of Steps Per Interval by Day Type")
print(panel_plot)
```

![](PA1_template_files/figure-html/panel plot-1.png)<!-- -->
