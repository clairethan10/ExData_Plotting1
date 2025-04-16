# This is the final assignment for Coursera R programming 
# Getting and Cleaning Data 

# set the directory 
setwd("~/R/Coursera projects/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")

# list the files/folders in the directory
list.files()

# read in data 
xtrain <- read.table("train/X_train.txt")
ytrain <- read.table("train/y_train.txt")
subjecttrain <- read.table("train/subject_train.txt")

xtest <- read.table("test/x_test.txt")
ytest <- read.table("test/y_test.txt")
subjecttest <- read.table("test/subject_test.txt")

#add column names 
names(subjecttest) <- "subjectID" 
names(subjecttrain) <- "subjectID"

features <- read.table("features.txt")
names(xtest) <- features$V2 
names(xtrain) <- features$V2

names(ytest) <- "activity"
names(ytrain) <- "activity"

train <- cbind(subjecttest, ytest, xtest)
test <- cbind(subjecttrain, ytrain, xtrain)

dt <- rbind(train,test)

grepl("activity", tolower(names(dt)))

#examine the counts of each subject 
addmargins(table(train$subjectID))
addmargins(table(test$subjectID))
addmargins(table(dt$subjectID))

#extract mean and sd for each measurement
meanstd <- grepl("mean", names(dt)) | grepl("std", names(dt))

#keeping the first two column names
meanstd[1:2] <- TRUE 

dt2 <- dt[,meanstd]

# add label to activity values 

dt2$activity <- factor(dt2$activity, 
                       labels=c("walking",
                                "walking_upstairs", 
                                "walking_downstairs", 
                                "sitting",
                                "standing", 
                                "laying"))
install.packages("reshape2")

library(reshape2)
wide <- melt(dt2, id=c("subjectID", "activity"))

sub_activity_mean <- dcast(melt, subjectID+activity ~ variable,mean,
              margins=c("subjectID+activity", "variable"))

#save the dataset 
write.csv(tidy,"subject_activity_mean.csv", row.names=FALSE )