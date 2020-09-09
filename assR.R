

#Load Data in the environment
unzip(zipfile="repdata_data_activity.zip")
data = read.csv("activity.csv")

##What is mean total number of steps taken per day?
library(ggplot2)
total_steps <- tapply(data$steps, data$date, FUN = sum, na.rm = TRUE)
head(total_steps)
qplot(total_steps, xlab="total number of steps taken each day", binwidth = 1000)
mean(total_steps, na.rm = TRUE)
median(total_steps, na.rm = TRUE)

##What is the average daily activity pattern?

average <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                     FUN=mean, na.rm=TRUE)

g <- ggplot(average, aes(interval, steps))
g+geom_line()+
  xlab("5-minute interval") +
  ylab("average number of steps taken")

average[which.max(average$steps),]


##Imputing missing values
missing = is.na(data$steps)
###missing values
table(missing)

###Replace missing values with MEAN of 5 - minute interval 
fill_value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else 
    filled <- average[average$interval == interval, "steps"]
  return(filled)
}

fill_data <- data
fill_data$steps <- mapply(fill_value, fill_data$steps, fill_data$interval)


total.steps <- tapply(fill_data$steps, fill_data$date, FUN = sum)
qplot(total.steps, xlab="total number of steps taken each day", binwidth = 1000)
mean(total.steps)
median(total.steps)

##Are there differences in activity patterns between weekdays and weekends?
weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
    return("weekday") else if (day %in% c("Saturday", "Sunday")) 
      return("weekend") else stop("invalid date")
}

fill_data$date <- as.Date(fill_data$date) 

fill_data$day <- sapply(fill_data$date, FUN = weekday.or.weekend)
fill_data$day <- as.factor(fill_data$day)


averages <- aggregate(steps ~ interval + day, data = fill_data, mean)

ggplot(averages, aes(interval, steps)) + 
  geom_line() + 
  facet_grid(day ~ .) + 
  xlab("5-minute interval") + 
  ylab("Number of steps")
