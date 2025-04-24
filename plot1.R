## This R program generates codes for Course Project 1 
## in Exploratory Data Analysis 
## plot 1


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


#################
# plot 1
################

#print the histogram in png 

png("plot1.png", width=800, height=600)
hist(data$Global_active_power, 
     xlab="Global Active Power (kilowatts)", 
     ylab="Frequency",
     main="Global Active power")

dev.off()



