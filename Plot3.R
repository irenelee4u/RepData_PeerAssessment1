##Imputing missing values
activity <- read.csv("activity.csv", header = TRUE)


##Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs) 
missval <- is.na(activity$steps)

##Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
##Create a new dataset that is equal to the original dataset but with the missing data filled in.
meanstep <- aggregate(steps ~ interval, activity, mean)
filledindata <- transform(activity, steps=ifelse(is.na(activity$steps),meanstep$steps[match(activity$interval,meanstep$interval)], activity$steps ))

##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
##Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
filledin <- aggregate(steps ~ date, filledindata, FUN = sum)
hist(filledin$steps, main= "Total Numbers of Steps per Day - Imputed", col="green" ,xlab = "Number of Steps")