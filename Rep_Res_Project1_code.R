
# 1. Code for reading in the dataset and/or processing the data
require(ggplot2)

file <- "fitbit_activity.zip"
if(!file.exists(file))
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = file)
if(!file.exists("activity.csv"))
    unzip(file)

data <- read.csv("activity.csv")
data_no_na <- data[!is.na(data$steps),]

# 2. Histogram of the total number of steps taken each day
day_data <- aggregate(steps ~ date, data = data_no_na, FUN = sum)
hist(day_data$steps, xlab = "Number of Steps", main = "Daily Steps Histogram", col = "grey")

# 3. Mean and median number of steps taken each day
as.integer(mean(day_data$steps))
as.integer(median(day_data$steps))

# 4. Time series plot of the average number of steps taken
interval_data <- aggregate(steps ~ interval, data = data, FUN = mean)
ggplot(interval_data, aes(x = interval, y = steps, group = 1, col = "red")) + 
    geom_line() +
    xlab("5 Minute Interval") + 
    ylab("Average Steps Taken in Interval") + 
    ggtitle("Average Daily Step Activity") + 
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# 5. The 5-minute interval that, on average, contains the maximum number of steps
interval_data[which.max(interval_data$steps),1]
