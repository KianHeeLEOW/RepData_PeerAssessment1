# Reproducible Research: Peer Assessment 1


#### Loading and preprocessing the data

```r
data <-  read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")
```

 Converting the "date" variable to a Date class and "interval" variable to factor

```r
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$interval <- factor(data$interval)
```

#### Remove NA Values

```r
NA_index <- is.na(as.character(data$steps))
data_no_NA <- data[!NA_index,]
```

#### Creating a data frame with the steps taken for each day

```r
steps_per_day <- aggregate(steps ~ date, data = data_no_NA, sum)
```
#### Adding column names to the created data frame

```r
colnames(steps_per_day) <- c("date", "steps")
```
# What is mean total number of steps taken per day?
![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

## Mean total number of steps taken per day

```r
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```
## Median total number of steps taken per day

```r
median(steps_per_day$steps)
```

```
## [1] 10765
```

# What is the average daily activity pattern?
#### Calculating the average

```r
steps_per_interval <- aggregate(data_no_NA$steps, by=list(interval=data_no_NA$interval), FUN=mean)
```

#### Adding columns names

```r
colnames(steps_per_interval) <- c("interval", "average_steps")
```

#### ploting the average daily activity pattern 

```r
plot(as.integer(levels(steps_per_interval$interval)), steps_per_interval$average_steps, type="l",
     xlab = "Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern",  col ="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

#### The 5-minute interval that contains the maximum number of steps

```r
interval_max_steps<-steps_per_interval[which.max(steps_per_interval$average_steps),]$interval
interval_max_steps
```

```
## [1] 835
## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 ... 2355
```

# Imputing missing values
#### Number of missing values

```r
missing_values<-sum(is.na(data))
missing_values
```

```
## [1] 2304
```
#### Fill up missing values with mean
#### Finding the indices of missing values (NAs)

```r
NA_index <- which(is.na(as.character(data$steps)))
complete_data <- data
```
#### Inputing missing values using the mean for that 5-minute interval

```r
complete_data[NA_index, ]$steps<-unlist(lapply(NA_index, FUN=function(NA_index){
                steps_per_interval[data[NA_index,]$interval==steps_per_interval$interval,]$average_steps
                }))
```
## Displaying the new dataset that is equal to the original dataset but with the missing data filled in.

```r
summary(complete_data)
```

```
##      steps             date               interval    
##  Min.   :  0.00   Min.   :2012-10-01   0      :   61  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   5      :   61  
##  Median :  0.00   Median :2012-10-31   10     :   61  
##  Mean   : 37.38   Mean   :2012-10-31   15     :   61  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   20     :   61  
##  Max.   :806.00   Max.   :2012-11-30   25     :   61  
##                                        (Other):17202
```

```r
str(complete_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```
#### Creating a data frame with the steps taken for each day

```r
steps_each_day_complete <- aggregate(steps ~ date, data = complete_data, sum)
```
#### Adding column names to the created data frame

```r
colnames(steps_each_day_complete) <- c("date", "steps")
```
## Making the histogram and report mean and median
![](PA1_template_files/figure-html/unnamed-chunk-19-1.png)<!-- -->
#### Mean

```r
mean(steps_each_day_complete$steps)
```

```
## [1] 10766.19
```
##### Median

```r
median(steps_each_day_complete$steps)
```

```
## [1] 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?
 
#### Creating a factor "day "to store the day of the week

```r
complete_data$day <- as.factor(weekdays(complete_data$date))
```
#### Creating a logical variable "is_weekday" where weekday=TRUE, weekend = FALE

```r
complete_data$is_weekday <- ifelse(!(complete_data$day %in% c("Saturday","Sunday")), TRUE, FALSE) 
```

#### Calculating average number of steps for weekdays

```r
weekdays_data <- complete_data[complete_data$is_weekday,]
steps_per_interval_weekdays <- aggregate(weekdays_data$steps, by=list(interval=weekdays_data$interval), FUN=mean)
```

#### Calculating average number of steps for weekends

```r
weekends_data <- complete_data[!complete_data$is_weekday,]
steps_per_interval_weekends <- aggregate(weekends_data$steps, by=list(interval=weekends_data$interval), FUN=mean)
```
#### Adding columns names

```r
colnames(steps_per_interval_weekdays) <- c("interval", "average_steps")
colnames(steps_per_interval_weekends) <- c("interval", "average_steps")
```
#### Adding a column to indicate the day

```r
steps_per_interval_weekdays$day <- "Weekday"
steps_per_interval_weekends$day <- "Weekend"
```
#### Merging the two together

```r
week_data <- rbind(steps_per_interval_weekends, steps_per_interval_weekdays)
```
#### Converting the day variable to a factor

```r
week_data$day <- as.factor(week_data$day)
```
## Making the plot

```r
library(lattice)
xyplot(average_steps ~  interval | day, data = week_data, layout = c(1,2), type ="l", ylab="Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-30-1.png)<!-- -->
