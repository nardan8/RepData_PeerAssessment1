---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(dplyr)
library(ggplot2)
zipFile <- "activity.zip"
filename <- "activity.csv"

# unzip if file doesn't already exist
if (!file.exists(filename)) {
        unzip(zipFile)
}

# read data
df <- read.csv(filename)

# convert Date type
df <- mutate(df, date = as.Date(date, "%Y-%m-%d"))
```


## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
