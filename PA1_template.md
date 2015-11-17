# Reproducible Research: Peer Assessment 1


```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```


## Loading and preprocessing the data
First, we want to load and preprocess the data. The data in included in this repo zipped up. We unzip activity.zip and load it into data.


```r
# unzip data
if (!file.exists("activity.csv")) {
    unzip("activity.zip", "activity.csv")
}
# load data
data <- read.csv("activity.csv")
summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

Then we preprocess:

```r
# preprocess
data$date <- as.Date(data$date, format = "%Y-%m-%d")

# data to answer total number of steps per day question
d.steps <- data %>%
           filter(!is.na(steps)) %>% 
           group_by(date) %>% 
           summarise(total_steps = sum(steps, na.rm = TRUE))

# data to answer daily pattern question
d.interval <- data %>% 
              filter(!is.na(steps)) %>%                          
              group_by(interval) %>%         
              summarise(steps_mean = mean(steps))
```

## What is mean total number of steps taken per day?

Calculate total number of steps taken per day:

```r
print(d.steps)
```

```
## Source: local data frame [53 x 2]
## 
##          date total_steps
##        (date)       (int)
## 1  2012-10-02         126
## 2  2012-10-03       11352
## 3  2012-10-04       12116
## 4  2012-10-05       13294
## 5  2012-10-06       15420
## 6  2012-10-07       11015
## 7  2012-10-09       12811
## 8  2012-10-10        9900
## 9  2012-10-11       10304
## 10 2012-10-12       17382
## ..        ...         ...
```

Then a histogram:


```r
hist(d.steps$total_steps, main="Histogram of total steps per day", xlab = "Total steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Finally here are mean and median of total number of steps per day:


```r
steps_mean <- mean(d.steps$total_steps)
steps_mean
```

```
## [1] 10766.19
```

```r
steps_median <- median(d.steps$total_steps)
steps_median
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
plot(d.interval$interval, d.interval$steps_mean, type = "l", xlab = "interval", ylab = "steps", main = "Time series of steps against interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

Interval with most number of steps:

```r
d.interval[which.max(d.interval$steps_mean),]
```

```
## Source: local data frame [1 x 2]
## 
##   interval steps_mean
##      (int)      (dbl)
## 1      835   206.1698
```

## Imputing missing values
First we count the total number of NAs in dataset:

```r
colSums(is.na(data))
```

```
##    steps     date interval 
##     2304        0        0
```


We create a new dataset d.fill_missing and fill missing values with average number of steps in 5-min interval:


```r
d.fill_missing  <- data
missing <- is.na(d.fill_missing$steps)
mean_interval <- tapply(d.fill_missing$steps, d.fill_missing$interval, mean, na.rm = TRUE)

d.fill_missing$steps[missing] <- mean_interval[as.character(d.fill_missing$interval[missing])]
```

As expected we find no NAs in this new data set:


```r
colSums(is.na(d.fill_missing))
```

```
##    steps     date interval 
##        0        0        0
```


## Are there differences in activity patterns between weekdays and weekends?
