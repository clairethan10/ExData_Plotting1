## This R program generates codes for Course Project 1 
## in Exploratory Data Analysis 
## Plot 4


library(ggplot2)
library(tidyr)
library(gridExtra)

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

#resize the graphical parameter with 2 rows and 2 columns
par(mfrow=c(2,2))




# convert time string to full date time 
data$datetime <- as.POSIXct(paste(data$Date, data$Time, format="%Y-%m-%d %H:%M:%S"))

#manually define the start of each day to label at axis 
tick_dates <- as.POSIXct(c("2007-02-01", "2007-02-02", "2007-02-03"),
                         format="%Y-%m-%d")

#define weekday names 
tick_labels <- weekdays(tick_dates, abbreviate = TRUE)

# save the plots 
png("plot4.png", width=480, height=480)


# plot 1
p1 <- ggplot(data, aes(x=datetime, y=Global_active_power)) +
        geom_line(color = "black", size=1) +
  labs(x="", y="Global Active Power") +
  scale_x_datetime(             #select the tick marks and labels 
  breaks=tick_dates,
  labels=tick_labels) +
  theme_minimal()

#plot 2

p2 <- ggplot(data, aes(x=datetime, y=Voltage)) +
    geom_line(color="black", size=1) +
  labs(x="", y="Voltage") +
  scale_x_datetime(             #select the tick marks and labels 
    breaks=tick_dates,
    labels=tick_labels) +
  theme_minimal()



# plot 3 using ggplot2 
 

#reshape the data from wide to long 
data_long <- pivot_longer(data, cols=starts_with("Sub"), 
                          names_to="Submetering", values_to="Y") 

# line plot using ggplot2  

p3 <- ggplot(data_long, aes(x=datetime, y=Y, color=Submetering)) +
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
       x="", y="Energy Sub metering") + 
  theme_minimal() + 
  theme(legend.position = c(0.95, 0.95), # top right x,y coordinates
        legend.justification = c("right", "top"), 
        legend.title=element_blank())    # remove the legend title

#plot 4

p4 <- ggplot(data, aes(x=datetime, y=Global_reactive_power)) +
  geom_line(color="black", size=1) +
  labs(x="", y="Global Reactive Power") +
  scale_x_datetime(             #select the tick marks and labels 
    breaks=tick_dates,
    labels=tick_labels) +
  theme_minimal()

# arrange in 2x 2 layout 
grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)

dev.off()


