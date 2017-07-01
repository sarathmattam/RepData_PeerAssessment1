# Functions

# Replace each missing value with the mean value of its 5-minute interval
Fillvalue <- function(steps, interval) {
  Filled <- NA
  if (!is.na(steps))
    Filled <- c(steps)
  else
    Filled <- (AvgStepsByInterval[AvgStepsByInterval$interval==interval, "steps"])
  return(Filled)
}

# Function to determine type of day (weekday/weekend)
DayType <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}

# Download and extract file
if (!file.exists("activity.csv")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","activity.zip")
  unzip(zipfile="activity.zip")
  }

# Loading Data to variale
ActivityData <- read.csv("activity.csv")

# Load Libraries
library(ggplot2)

# Calculating Steps / Day
png(filename="plot1.png", width=480, height=480, units='px')
StepsByDate <- tapply(ActivityData$steps, ActivityData$date, sum, na.rm=TRUE)
g <- qplot(StepsByDate, binwidth=1000, xlab="Steps/day")
print(g)
dev.off()

# Mean and Median of Steps/Day
mean(StepsByDate, na.rm=TRUE)
median(StepsByDate, na.rm=TRUE)

# Calculating steps per Interval
png(filename="plot2.png", width=480, height=480, units='px')
AvgStepsByInterval <- aggregate(x=list(steps=ActivityData$steps), by=list(interval=ActivityData$interval),
                      mean, na.rm=TRUE)
g <- ggplot(data=AvgStepsByInterval, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute Interval") +
  ylab("Avg No: of Steps")
print(g)
dev.off()

AvgStepsByInterval[which.max(AvgStepsByInterval$steps),]

# Identifying missing data
MissingData <- is.na(ActivityData$steps)
table(MissingData)


FilledData <- ActivityData
FilledData$steps <- mapply(Fillvalue, FilledData$steps, FilledData$interval)

# Plotting with filled data
png(filename="plot3.png", width=480, height=480, units='px')
StepsByDate <- tapply(FilledData$steps, FilledData$date, sum)
g <- qplot(StepsByDate, binwidth=1000, xlab="Steps/Day")
print(g)
dev.off()

mean(StepsByDate)
median(StepsByDate)

FilledData$date <- as.Date(FilledData$date)
FilledData$day <- sapply(FilledData$date, FUN=DayType)

# Plotting Number of steps based on weekday and weekend data
png(filename="plot4.png", width=480, height=480, units='px')
AvgStepsByInterval <- aggregate(steps ~ interval + day, data=FilledData, mean)
g <- ggplot(AvgStepsByInterval, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
print(g)
dev.off()