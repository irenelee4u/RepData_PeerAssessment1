##Are there differences in activity patterns between weekdays and weekends?
activity <- read.csv("activity.csv", header = TRUE)
##For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

meanstep <- aggregate(steps ~ interval, activity, mean)
filledindata <- transform(activity, steps=ifelse(is.na(activity$steps),meanstep$steps[match(activity$interval,meanstep$interval)], activity$steps ))

##Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
type <- function(date)  {day <- weekdays(date)
                        if (day %in% c('Monday','Tuesday', 'Wednesday','Thursday','Friday'))
                                return("weekday")
                                else if (day %in% c('Saturday','Sunday'))
                                        return("weekend")
                                else
                                        stop ("Invalid Date Format.")}
filledindata$date <- as.Date(filledindata$date)
filledindata$day <- sapply(filledindata$date, FUN = type)

##Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
##See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
meanstepbyday <- aggregate(steps ~ interval + day, filledindata, mean)
ggplot(data = meanstepbyday, aes(x=interval, y=steps))+theme_bw() +geom_line(color="blue")+facet_grid(day ~.)+ggtitle("Activity Pattern")+xlab("Interval")+ylab("Number of Steps")+theme(plot.title = element_text(hjust = 0.5))

