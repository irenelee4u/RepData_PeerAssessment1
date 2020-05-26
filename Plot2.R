##What is the average daily activity pattern?
##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
## Loading and preprocessing the data
activity <- read.csv("activity.csv", header = TRUE)

library(ggplot2)
meanstep <- aggregate(steps ~ interval, activity, mean)
ggplot(data = meanstep, aes(x=interval, y= steps))+geom_line(color="blue")+ggtitle("Average Daily Activity Pattern")+xlab("5-minute Interval")+ylab("Average Number of Steps")+theme(plot.title = element_text(hjust = 0.5))

##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxstep <- meanstep[which.max(meanstep$steps),]
