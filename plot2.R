## This R program generates codes for Course Project 1 
## in Exploratory Data Analysis 
## Plot 2


#file path
filepath <- "exdata_data_household_power_consumption/household_power_consumption.txt"

# read the header 
header <- readLines(filepath, n=1)

#read all lines from the file 
lines <- readLines(filepath)

#filter line from desire dates 
filteredlines <- grep("^(1|2)/2/2007", lines, value=TRUE)

#combine header and filter lines 

data <- read.table(text = c(header, filteredlines),
                   sep=";", header=TRUE,
                   na.strings="?")

#check the dates pulled
unique(data$Date)

data$Date <- as.Date(data$Date, format="%d/%m/%Y")

#resize the graphical parameter
par(mfrow=c(1,1))


#####################
# Plot 2
#####################


# convert time string to full date time 
data$datetime <- as.POSIXct(paste(data$Date, data$Time, format="%Y-%m-%d %H:%M:%S"))

#manually define the start of each day to label at axis 
tick_dates <- as.POSIXct(c("2007-02-01", "2007-02-02", "2007-02-03"),
                         format="%Y-%m-%d")

#define weekday names 
tick_labels <- weekdays(tick_dates)

# plot the graph 

png("plot2.png", width=480, height=480)

plot(data$datetime, data$Global_active_power, type="l",
     xlab="days", ylab="Global Active Power",
     main="Plot 2", 
     xaxt="n")

#add axis label
axis(1,at=tick_dates, labels=tick_labels)

dev.off()


