# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(data.table)
library(knitr)

#load the data from the zip file
data <- read.csv(unz("activity.zip", "activity.csv"))
#convert the data.frame to a data.table for easy manipulation
DT <- data.table(data)
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
DT_by_date <- DT[,list(sum_by_date=sum(steps, na.rm=T)),by=date]
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(DT_by_date[,sum_by_date],  main = "Histogram", xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
DT_by_date <- DT[,list(mean_by_date=mean(steps, na.rm=T), median_by_date=as.double(median(steps, na.rm=T))),by=date]
kable(DT_by_date)
```



date          mean_by_date   median_by_date
-----------  -------------  ---------------
2012-10-01             NaN               NA
2012-10-02       0.4375000                0
2012-10-03      39.4166667                0
2012-10-04      42.0694444                0
2012-10-05      46.1597222                0
2012-10-06      53.5416667                0
2012-10-07      38.2465278                0
2012-10-08             NaN               NA
2012-10-09      44.4826389                0
2012-10-10      34.3750000                0
2012-10-11      35.7777778                0
2012-10-12      60.3541667                0
2012-10-13      43.1458333                0
2012-10-14      52.4236111                0
2012-10-15      35.2048611                0
2012-10-16      52.3750000                0
2012-10-17      46.7083333                0
2012-10-18      34.9166667                0
2012-10-19      41.0729167                0
2012-10-20      36.0937500                0
2012-10-21      30.6284722                0
2012-10-22      46.7361111                0
2012-10-23      30.9652778                0
2012-10-24      29.0104167                0
2012-10-25       8.6527778                0
2012-10-26      23.5347222                0
2012-10-27      35.1354167                0
2012-10-28      39.7847222                0
2012-10-29      17.4236111                0
2012-10-30      34.0937500                0
2012-10-31      53.5208333                0
2012-11-01             NaN               NA
2012-11-02      36.8055556                0
2012-11-03      36.7048611                0
2012-11-04             NaN               NA
2012-11-05      36.2465278                0
2012-11-06      28.9375000                0
2012-11-07      44.7326389                0
2012-11-08      11.1770833                0
2012-11-09             NaN               NA
2012-11-10             NaN               NA
2012-11-11      43.7777778                0
2012-11-12      37.3784722                0
2012-11-13      25.4722222                0
2012-11-14             NaN               NA
2012-11-15       0.1423611                0
2012-11-16      18.8923611                0
2012-11-17      49.7881944                0
2012-11-18      52.4652778                0
2012-11-19      30.6979167                0
2012-11-20      15.5277778                0
2012-11-21      44.3993056                0
2012-11-22      70.9270833                0
2012-11-23      73.5902778                0
2012-11-24      50.2708333                0
2012-11-25      41.0902778                0
2012-11-26      38.7569444                0
2012-11-27      47.3819444                0
2012-11-28      35.3576389                0
2012-11-29      24.4687500                0
2012-11-30             NaN               NA

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
DT_by_interval <- DT[,list(avg_by_interval=mean(steps, na.rm=T)),by=interval]
plot(DT_by_interval,type = "l", xlab="Interval", ylab="Average Number Of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
DT_by_interval[which.max(DT_by_interval[,avg_by_interval]),interval]
```

```
## [1] 835
```

The 5-minute interval with the highest average across all the days in the dataset is 835.

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(!complete.cases(DT)) 
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

My strategy is to replace the NA values by the mean of the value for that 5 minute interval.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
setkey(DT, interval)
setkey(DT_by_interval, interval)
filled_DT<-DT[DT_by_interval,list(date,interval,steps=ifelse(is.na(steps),avg_by_interval,steps))]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
DT_by_date <- filled_DT[,list(sum_by_date=sum(steps)),by=date]
hist(DT_by_date[,sum_by_date],  main = "Histogram", xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
DT_by_date <- filled_DT[,list(mean_by_date=mean(steps, na.rm=T), median_by_date=as.double(median(steps, na.rm=T))),by=date]
kable(DT_by_date)
```



date          mean_by_date   median_by_date
-----------  -------------  ---------------
2012-10-01      37.3825996         34.11321
2012-10-02       0.4375000          0.00000
2012-10-03      39.4166667          0.00000
2012-10-04      42.0694444          0.00000
2012-10-05      46.1597222          0.00000
2012-10-06      53.5416667          0.00000
2012-10-07      38.2465278          0.00000
2012-10-08      37.3825996         34.11321
2012-10-09      44.4826389          0.00000
2012-10-10      34.3750000          0.00000
2012-10-11      35.7777778          0.00000
2012-10-12      60.3541667          0.00000
2012-10-13      43.1458333          0.00000
2012-10-14      52.4236111          0.00000
2012-10-15      35.2048611          0.00000
2012-10-16      52.3750000          0.00000
2012-10-17      46.7083333          0.00000
2012-10-18      34.9166667          0.00000
2012-10-19      41.0729167          0.00000
2012-10-20      36.0937500          0.00000
2012-10-21      30.6284722          0.00000
2012-10-22      46.7361111          0.00000
2012-10-23      30.9652778          0.00000
2012-10-24      29.0104167          0.00000
2012-10-25       8.6527778          0.00000
2012-10-26      23.5347222          0.00000
2012-10-27      35.1354167          0.00000
2012-10-28      39.7847222          0.00000
2012-10-29      17.4236111          0.00000
2012-10-30      34.0937500          0.00000
2012-10-31      53.5208333          0.00000
2012-11-01      37.3825996         34.11321
2012-11-02      36.8055556          0.00000
2012-11-03      36.7048611          0.00000
2012-11-04      37.3825996         34.11321
2012-11-05      36.2465278          0.00000
2012-11-06      28.9375000          0.00000
2012-11-07      44.7326389          0.00000
2012-11-08      11.1770833          0.00000
2012-11-09      37.3825996         34.11321
2012-11-10      37.3825996         34.11321
2012-11-11      43.7777778          0.00000
2012-11-12      37.3784722          0.00000
2012-11-13      25.4722222          0.00000
2012-11-14      37.3825996         34.11321
2012-11-15       0.1423611          0.00000
2012-11-16      18.8923611          0.00000
2012-11-17      49.7881944          0.00000
2012-11-18      52.4652778          0.00000
2012-11-19      30.6979167          0.00000
2012-11-20      15.5277778          0.00000
2012-11-21      44.3993056          0.00000
2012-11-22      70.9270833          0.00000
2012-11-23      73.5902778          0.00000
2012-11-24      50.2708333          0.00000
2012-11-25      41.0902778          0.00000
2012-11-26      38.7569444          0.00000
2012-11-27      47.3819444          0.00000
2012-11-28      35.3576389          0.00000
2012-11-29      24.4687500          0.00000
2012-11-30      37.3825996         34.11321

*Do these values differ from the estimates from the first part of the assignment?* 

Yes both of them differ.

*What is the impact of imputing missing data on the estimates of the total daily number of steps?*

By imputing using the mean value for the interval, the histogram becomes more nicely distributed. It is no longer skewed towards 0.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
# Needed for the weekday code to work correctly on my Japanese machine
Sys.setlocale("LC_TIME","English")
```

```
## [1] "English_United States.1252"
```

```r
filled_DT$date <- as.Date(filled_DT$date)
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
filled_DT$weekday <-  factor((weekdays(filled_DT$date) %in% weekdays1)+1L,levels=1:2, labels=c('weekend', 'weekday'))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
DT_by_weekday_by_interval <- filled_DT[,list(avg_by_interval=mean(steps, na.rm=T)),by=list(weekday,interval)]

library(lattice)
xyplot(avg_by_interval~interval | weekday, data=DT_by_weekday_by_interval,layout=c(1,2), type = "l", ylab = "Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 


