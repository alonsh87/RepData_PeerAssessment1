---
title: "Untitled"
output: html_document
---

Loading and preprocessing the data


```r
  data<-read.csv("activity.csv") 
  data$date<-as.Date(data$date)
  data$weekday<-weekdays(data$date)
  Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
  data$weekday<-weekdays(data$date)
```

What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

Make a histogram of the total number of steps taken each day

Calculate and report the mean and median total number of steps taken per day



```r
  tot_steps<-aggregate(steps~date,data=data,FUN=sum,na.rm=TRUE)
  hist(tot_steps$steps,xlab = "Total Steps",main = "Total steps per day",col = "red")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
  mean(tot_steps$steps)
```

```
## [1] 10766.19
```

```r
  median(tot_steps$steps)
```

```
## [1] 10765
```
-------------------------------------------------------------------------------------------------------
What is the average daily activity pattern?





```r
  #1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of
  #steps taken, averaged across all days (y-axis)
  avg_steps<-tapply(data$steps,data$interval,mean,na.rm=TRUE)
  plot(avg_steps,type="l",xlab = "Time",ylab = "Average Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
  sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
 #2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of
  #steps?
  max_steps<-avg_steps[which(avg_steps==max(avg_steps))]
  max_steps
```

```
##      835 
## 206.1698
```

-------------------------------------------------------------------------------------------------------
Imputing missing values
Instead of NA's I added the average number of steps of the interval in the day


```r
 # 1. Calculate and report the total number of missing values in the dataset 
 # (i.e. the total number of rows with NAs)
 sum(is.na(data$steps))  
```

```
## [1] 2304
```

```r
  # 2. Devise a strategy for filling in all of the missing values in the dataset. 
  # The strategy does not need to be sophisticated. For example, you could use the mean/median for that day,
  # or the mean for that 5-minute interval, etc. 
  day_int_average<-aggregate(steps~interval+weekday,data=data,FUN=mean,na.rm=TRUE)
  
 # 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
   for (i in which(is.na(data$steps)))
  {
     data$steps[i]<-day_int_average$steps[which(day_int_average$interval==data$interval[i] & 
                                                day_int_average$weekday==data$weekday[i])]
  }
  tot_steps_new<-tapply(data$steps,data$date,sum,na.rm=TRUE)
#  4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
  
  hist(tot_steps_new,xlab = "Total Steps",main = "Total steps per day",col = "red")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
  mean(tot_steps_new)
```

```
## [1] 10821.21
```

```r
  median(tot_steps_new)
```

```
## [1] 11015
```
we can see that the values of the mean and median are now different than the previous ones since we inserted data instead of NAs. It looks like the median has changed a lot more than the mean which makes sense since we have more data now.
----------------------------------------------------------------------------------------------------
Are there differences in activity patterns between weekdays and weekends?  
  

```r
#  1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day. 
  
  for (i in 1:length(data$steps)) 
    {  
         if(data$weekday[i]=="Saturday")
                data$daytype[i]<-"weekend"
          else if (data$weekday[i]=="Sunday")
                  data$daytype[i]<-"weekend"
            else data$daytype[i]<-"weekday"
              
       }
  steps_weekday<-tapply(data[which(data$daytype=="weekday"),]$steps,data[which(data$daytype=="weekday"),]$interval,mean)
  steps_weekend<-tapply(data[which(data$daytype=="weekend"),]$steps,data[which(data$daytype=="weekend"),]$interval,mean)

#  2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

par(mar=c(2,2,2,2))
plot(steps_weekday,type="l",xlab = "Interval", ylab = "Number of Steps",main = "Weekdays")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
plot(steps_weekend,type="l",xlab = "Interval", ylab = "Number of Steps",main = "Weekends")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-2.png) 

