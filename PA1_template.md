# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data



```r
data<-read.csv("activity.csv")
grp_date <- group_by(data, date)
grp_int <- group_by(data, interval)
```
## What is mean total number of steps taken per day?

```r
# 1. Calculate the total number of steps taken per day
steps.per.day <- summarise(grp_date, num.steps=sum(steps) )

# 2.Make a histogram of the total number of steps taken each day
histogram(~num.steps,data=steps.per.day,
           type="percent",
           #ylab="p"
           xlab="Number of steps",
           main="Total number of steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
# 3.Calculate and report the mean and median of the total number of steps taken per day
mean(steps.per.day$num.steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(steps.per.day$num.steps, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps.per.interval <- summarise(grp_int, avg.steps=mean(steps, na.rm = TRUE) )

plot(steps.per.interval$interval, steps.per.interval$avg.steps, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 
# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
filter(steps.per.interval, avg.steps == max(avg.steps))$interval
```

```
## [1] 835
```



## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nrow(filter(data, is.na(date) | is.na(interval) | is.na(steps)))
```

```
## [1] 2304
```


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

* Strategy is simle: we will use mean of steps from 5-minute interval where NA appears.

3 .Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
# add a column with 'avg(steps) per interval'
for_imp <- inner_join(data, steps.per.interval, by = "interval")

# if NA then use 'avg(steps) per interval' as steps
imp_data<-mutate(for_imp, steps = ifelse(is.na(steps), avg.steps,         steps), avg.steps = NULL)

imp_data <- arrange(imp_data, date, interval)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
imp_data <- group_by(imp_data, date)
imp.steps.per.day <- summarise(imp_data, num.steps=sum(steps) )

# 2.Make a histogram of the total number of steps taken each day
histogram(~num.steps,data=imp.steps.per.day,
           type="percent",
           #ylab="p"
           xlab="Number of steps",
           main="Total number of steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
# 3.Calculate and report the mean and median of the total number of steps taken per day
mean(imp.steps.per.day$num.steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(imp.steps.per.day$num.steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
imp_data<-mutate(imp_data, weekday = ifelse(wday(as.POSIXct(date))-1 > 5, "weekend", "weekday") )
```


Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
imp_data <- group_by(imp_data, weekday, interval)
for_plot<-summarise(imp_data, avg.steps = mean(steps))
ggplot(for_plot) + geom_line(aes(interval, avg.steps)) + facet_grid(weekday~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

