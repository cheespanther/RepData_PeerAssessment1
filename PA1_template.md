---
Output: html_document
Title: PA1_template.Rmd
output: html_document
---

Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Data

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

Assignment

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

Loading and preprocessing the Data

The following code reads "activity.csv" which contains 3 columns: "steps", "date" and "interval". 

1. Load the data (i.e. read.csv())
```{r}
setwd("~/R/COURSERA5_RESEARCH")
activity <<- read.csv("activity.csv")
echo = TRUE
```

2. Process/transform the data (if necessary) into a format suitable for analysis
The dates must be transformed in order to add a factor column that identifies which day each activity happened. 

```{r}
## Reads and transforms "date" column in activity into date format
dates <- as.POSIXct(activity$date)
## Makes a new list of the days for each step was taken
days <- weekdays(dates)
## Loads library for plots
library("lattice")
```


What is the mean total of steps taken per day? 

1. Calculate the total number of steps taken per day

```{r}
## Combines the original data with the "days" information
activity <- cbind(activity, days)
## Makes a new data.frame that is the sum of steps by day
sumbydate <-aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)
names(sumbydate) <- c("date", "steps")
head(sumbydate)
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```{r}
totalstep_date <- hist(sumbydate$steps, breaks = 10, main = "Histogram of Total Steps Taken per Day", col = "orange", xlab = "Number of Steps")
```

3. Calculate and report the mean and median of the total number of steps taken per day

```{r}
stepmean <- mean(sumbydate$steps, na.rm = TRUE)
stepmedian <- median(sumbydate$steps, na.rm = TRUE)
echo = FALSE
stepmean
stepmedian
```

What is the average daily activity pattern? 

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
## Creates a new data.frame of the mean of the steps taken per each 5 min interval
meanbyinterval <- aggregate(activity$steps, by = list(activity$interval), FUN = mean, na.rm = TRUE) 
## Creates a time series graph of the mean steps per day
meanbyintervalplot <- plot(meanbyinterval, type = "l", main = "Steps per Interval (Time Series)", col = "blue", xlab = "5 Minute Interval", ylab = "Total Number of Steps")
names(meanbyinterval) <- c("interval", "mean")
head(meanbyinterval)
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
## Subsets the day that has the highst "steps" average
highsteps <- subset(meanbyinterval$interval, meanbyinterval$mean == max(meanbyinterval$mean))
echo = FALSE
highsteps
```

Inputting missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
missingvalues <- sum(is.na(activity$steps))
echo = FALSE
missingvalues
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The following code creates a new dataframe called "nareplace". In this new dataframe all NAs are replaced by their weekday averages. 

```{r}
## Makes a data.frame of the means by day of the week. 
meanbyday <- aggregate(activity$steps, by = list(activity$days), FUN = mean, na.rm = TRUE)
names(meanbyday) <- c("day", "mean")
meanbyday

## Creates a new data.frame based on the original data (activity) then NAs are replaced for each day with the mean for the corresponding day taken from the previous data.frame 

nareplace <- activity

for (i in 1:length(nareplace$steps)) 
{
    if (is.na(nareplace$steps[i]) && nareplace$days[i] == "lunes" ) 
        {
            nareplace$steps[i] = meanbyday[3,2]
        }
}

for (i in 1:length(nareplace$steps)) 
{
    if (is.na(nareplace$steps[i]) && nareplace$days[i] == "martes" ) 
        {
            nareplace$steps[i] = meanbyday[4,2]
        }
}

for (i in 1:length(nareplace$steps)) 
{
    if (is.na(nareplace$steps[i]) && nareplace$days[i] == "miércoles" ) 
        {
            nareplace$steps[i] = meanbyday[5,2]
        }
}

for (i in 1:length(nareplace$steps)) 
{
    if (is.na(nareplace$steps[i]) && nareplace$days[i] == "jueves" ) 
        {
            nareplace$steps[i] = meanbyday[2,2]
        }
}

for (i in 1:length(nareplace$steps)) 
{
    if (is.na(nareplace$steps[i]) && nareplace$days[i] == "viernes" ) 
        {
            nareplace$steps[i] = meanbyday[7,2]
        }
}

for (i in 1:length(nareplace$steps)) 
{
    if (is.na(nareplace$steps[i]) && nareplace$days[i] == "sábado" ) 
        {
            nareplace$steps[i] = meanbyday[6,2]
        }
}

for (i in 1:length(nareplace$steps)) 
{
    if (is.na(nareplace$steps[i]) && nareplace$days[i] == "domingo" ) 
        {
            nareplace$steps[i] = meanbyday[1,2]
        }
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

The new datbase is "nareplace". These are the first values of the original database (which are NAs) replaced with the daily means for the corresponding day. 

```{r}
head(activity)
head(nareplace)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}

sumbydate_nareplace <-aggregate(nareplace$steps, by=list(nareplace$date), FUN=sum, na.rm=TRUE)
names(sumbydate_nareplace) <- c("date", "steps")
head(sumbydate_nareplace)

totalstep_date_nareplace <- hist(sumbydate_nareplace$steps, breaks = 10, main = "Histogram of Total Steps Taken per Day with NA replaced by Daily Means", col = "orange", xlab = "Number of Steps")

stepmean_nareplace <- mean(sumbydate_nareplace$steps, na.rm = TRUE)
stepmean_nareplace
stepmedian_nareplace <- median(sumbydate_nareplace$steps, na.rm = TRUE)
echo = FALSE
stepmedian_nareplace
```

This is the difference between the mean and median with NAs and the mean and median with the NAs replaced with the daily mean.  

```{r}
echo = FALSE
stepmean - stepmean_nareplace
stepmedian - stepmedian_nareplace
```

This is a big impact. The mean and median changes by about 10% which, in some cases, is a considerable amount. 

Are there differences in activity patterns between weekdays and weekends?

```{r}
weekend <- subset(activity, activity$days == "sábado" | activity$days == "domingo")
weekday <- subset(activity, activity$days != "sábado" & activity$days != "domingo")

## Creates new dataframe with only weekday data and a factor indicating "weekday" data
avgbyinterval_weekday <-aggregate(weekday$steps, by=list(weekday$interval), FUN=mean, na.rm=TRUE)
week_factor <- as.factor(rep("weekday", nrow(avgbyinterval_weekday)))
avgbyinterval_weekday <- cbind(avgbyinterval_weekday, week_factor)

## Creates new dataframe with only weekend data and a factor indicating "weekend" data
avgbyinterval_weekend <-aggregate(weekend$steps, by=list(weekend$interval), FUN=mean, na.rm=TRUE)
week_factor <- as.factor(rep("weekend", nrow(avgbyinterval_weekend)))
avgbyinterval_weekend <- cbind(avgbyinterval_weekend, week_factor)

## Combines both "weekday" and "weekend" data
avgbyinterval_factor <- rbind(avgbyinterval_weekday, avgbyinterval_weekend)

## Names columns for the new dataframe
names(avgbyinterval_factor) <- c("date", "steps", "week_factor")

## Plots both "Weekday" and "Weekend" data in one panel plot
xyplot(steps ~ date | week_factor, data = avgbyinterval_factor, type = "l", 
       main = "Averge Steps per Day",
       xlab = "Day",
       ylab = "Steps",
       layout = c(1,2))

```
