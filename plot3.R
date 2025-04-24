## This R program generates codes for Course Project 1 
## in Exploratory Data Analysis 
## Plot 3

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
# Plot 3
#####################


# convert time string to full date time 
data$datetime <- as.POSIXct(paste(data$Date, data$Time, format="%Y-%m-%d %H:%M:%S"))

#manually define the start of each day to label at axis 
tick_dates <- as.POSIXct(c("2007-02-01", "2007-02-02", "2007-02-03"),
                         format="%Y-%m-%d")

#define weekday names 
tick_labels <- weekdays(tick_dates)


# plot the graph using ggplot2 
library(ggplot2)
library(tidyr)

#reshape the data from wide to long 
data_long <- pivot_longer(data, cols=starts_with("Sub"), 
                          names_to="Submetering", values_to="Y") 

# line plot using ggplot2  

png("plot3.png", width=480, height=480)

ggplot(data_long, aes(x=datetime, y=Y, color=Submetering)) +
  geom_line() +                 #select the type of graph
  scale_color_manual(values=c(  # select the color of groups
    "Sub_metering_1" ="gray", 
    "Sub_metering_2" ="red",
    "Sub_metering_3" ="blue"
  )) +
  scale_x_datetime(             #select the tick marks and labels 
    breaks=tick_dates,
    labels=tick_labels
  )+
  
  labs(title="Plot 3", 
       x="Date", y="Energy Sub metering") + 
  theme_minimal() + 
  theme(legend.position = c(0.95, 0.95), # top right x,y coordinates
        legend.justification = c("right", "top"), 
        legend.title=element_blank())    # remove the legend title


dev.off()


