---
title: "Personal Activity Monitoring"
author: "Kushal M Kulkarni"
date: "18/07/2020"
output: html_document
---

```{r setup, include = FALSE, cache = FALSE}
knitr::opts_chunk$set(error = TRUE)
```

# 1. Load and preprocessing the data

**Fix knitr settings**
```{r}
knitr::opts_chunk$set(echo = TRUE, fig.width = 12, fig.height = 6, warning = FALSE)
```

**Reading the Data**
```{r}
activity <- read.csv("activity.csv")
```

**Loading tidyverse package**
```{r}
library(tidyverse)
```

**View the data**
```{r}
tibble(activity)
```

**Convert date column from char to date class**
```{r}
activity$date <- as.Date(activity$date)
```


# 2. Mean total number of steps taken per day

### 2.1 Histogram
(NAs not removed)
```{r}
eachday <- activity %>% group_by(date) %>% summarise(TotalSteps = sum(steps))

ggplot(eachday, aes(x = TotalSteps)) + 
        geom_histogram(aes(y=..density..), bins = 30, color="black", fill="#00AFBB") +
        geom_density(alpha=.2, fill="#00AFBB") +
        ggtitle("Histogram with Density plot- Total number of steps taken per day") +
        ylab("Frequency") +
        xlab("Total steps taken per day")
```

### 2.2 Mean and Median of total number of steps taken per day
```{r}
data.frame("Mean" = mean(eachday$TotalSteps, na.rm = T), 
           "Median" = median(eachday$TotalSteps, na.rm = T))

```


# 3. The average daily activity pattern

### 3.1 Time Series plot
```{r}
averagesteps <- activity %>% 
        group_by(interval) %>% 
        summarise(AverageSteps = mean(steps, na.rm = T))
ggplot(averagesteps, aes(x = interval, y = AverageSteps)) +
        geom_line(colour = "#00AFBB", size = 1) + 
        ggtitle("Time Series plot of of the 5-minute interval and the average number of steps taken, averaged across all days") +
        xlab("5-minute interval") +
        ylab("Steps (averaged across all days)")
```

### 3.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
filter(averagesteps, AverageSteps == max(AverageSteps))
```


# 4. Imputing missing values

### 4.1 Calculate the Missing Values
**Total missing values in the data are** `r sum(is.na(activity$steps))`, **which is** `r round((sum(is.na(activity$steps))/nrow(activity))*100)`% **of the data**

### 4.2 Strategy
- Group the data by **"intervals"** and calculate average **"steps"** taken
- Replace the missing values with **rounded mean** values of steps of 5 minutes interval
- Name this new data set

### 4.3 Create a New Complete Dataset (NAs removed)
**New dataset that is equal to the original dataset but with the missing data filled in.**
```{r}
CompleteActivity <- activity %>% 
        group_by(interval) %>% 
        mutate(Rounded.Avg.Steps.forNA = round(mean(steps, na.rm = T))) %>% 
        ungroup() %>% 
        mutate(steps = ifelse(is.na(steps), Rounded.Avg.Steps.forNA, steps)) %>% 
        select(-(Rounded.Avg.Steps.forNA))
```

**New complete dataset**
```{r}
tibble(CompleteActivity)
```

### 4.4 Histogram - total number of steps taken per day NAs replaced
```{r}
Neweachday <- CompleteActivity %>% group_by(date) %>% summarise(TotalSteps = sum(steps))
ggplot(Neweachday, aes(x = TotalSteps)) + 
        geom_histogram(aes(y=..density..), bins = 30, color="black", fill="#E7B800") +
        geom_density(alpha=.2, fill="#E7B800") +
        ggtitle("Histogram with Density plot- Total number of steps taken per day (NAs replaced)") +
        ylab("Frequency") +
        xlab("Total steps taken per day")
```

```{r echo = FALSE}
eachday <- activity %>% group_by(date) %>% summarise(TotalSteps = sum(steps))

ggplot(eachday, aes(x = TotalSteps)) + 
        geom_histogram(aes(y=..density..), bins = 30, color="black", fill="#00AFBB") +
        geom_density(alpha=.2, fill="#00AFBB") +
        ggtitle("Histogram with Density plot- Total number of steps taken per day (NAs not removed)") +
        ylab("Frequency") +
        xlab("Total steps taken per day")
```


### 4.5 Comparing mean & median values before & after imputing missing values
```{r}
data.frame("Mean with NAs" = mean(eachday$TotalSteps, na.rm = T), 
           "Mean without NAs" = mean(Neweachday$TotalSteps, na.rm = T), 
           "Median with NAs" = median(eachday$TotalSteps, na.rm = T),
           "Median without NAs" = median(Neweachday$TotalSteps, na.rm = T))
```


# 5. Are there differences in activity patterns between weekdays and weekends?

### 5.1 Group (factor) the data by Weekdays and Weekends using weekdays() function
```{r}
Week.days.ends <- CompleteActivity %>% 
        group_by(date) %>% 
        mutate(DayType = ifelse(weekdays(date) == c("Sunday", "Saturday"), "Weekend", "Weekday")) %>% 
        ungroup() %>% 
        group_by(interval, DayType) %>% 
        summarise(AverageSTEPS = mean(steps))
```

**New factored dataset (weekday & weekend)**
```{r}
tibble(Week.days.ends)
```

### 5.2 Panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekdays or weekends.
```{r}
ggplot(Week.days.ends, aes(x = interval, y = AverageSTEPS)) +
        facet_grid(DayType~.) +
        geom_line(colour = "#E7B800", size = 1) + 
        ggtitle("Time Series plot of of the 5-minute interval and the average number of steps taken, averaged across DayType") +
        xlab("5-minute interval") +
        ylab("Steps (averaged across all days)")
```


<center> **THANK YOU!** </center>