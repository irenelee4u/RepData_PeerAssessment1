## Loading and preprocessing the data
activity <- read.csv("activity.csv", header = TRUE)

## What is mean total number of steps taken per day?
##Calculate the total number of steps taken per day
totalsteps <- aggregate(steps ~ date, activity, FUN = sum)

##Make a histogram of the total number of steps taken each day
hist(totalsteps$steps, main="Total Number of Steps per Day", col = "blue" ,xlab= "Number of Steps" )

##Calculate and report the mean and median of the total number of steps taken per day
round(mean(totalsteps$steps)
)
median(totalsteps$steps)

