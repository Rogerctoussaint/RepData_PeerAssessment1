
file <- "fitbit_activity.zip"

if(!file.exists(file))
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = file)

if(!file.exists("activity.csv"))
    unzip(file)

data <- read.csv("activity.csv")
data <- data[!is.na(data$steps),]


day_data <- aggregate(steps ~ date, data = data, FUN = sum)
hist(day_data$steps, xlab = "Number of Steps", main = "Daily Steps Histogram", col = "grey")

as.integer(mean(day_data$steps))
as.integer(median(day_data$steps))

interval_data <- aggregate(steps ~ interval, data = data, FUN = mean)
ggplot(interval_data, aes(x = interval, y = steps, group = 1, col = "red")) + 
    geom_line() +
    xlab("5 Minute Interval") + 
    ylab("Average Steps Taken in Interval") + 
    ggtitle("Average Daily Step Activity") + 
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

max(interval_data$steps)
interval_data[which.max(interval_data$steps),1]
