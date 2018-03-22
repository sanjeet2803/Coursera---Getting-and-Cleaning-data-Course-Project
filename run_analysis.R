
##setting up working folder and loading required packages
setwd("C:/Users/User/Documents/Getting and Cleaning Data - Coursera")
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(zoo)


##Downloading files required for the course project
file_name <- "get_clean_dataset.zip"

file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

if(!file.exists(file_name)){
  download.file(file_url, file_name)
}


if(!file.exists("UCI HAR Dataset")){
  unzip(file_name)
}


##Reading training data
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")


##Reading testing data
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

## Reading activity labels
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
class(activity_labels$V2)
activity_labels$V2 <- as.character(activity_labels$V2)

##Reading feature(Variable) description
features <- read.table("./UCI HAR Dataset/features.txt")
class(features$V2)
features$V2 <- as.character(features$V2)

##Merging Training and testing data to create overall data files
x_overall <- rbind(x_train, x_test)
y_overall <- rbind(y_train, y_test)
subject_overall <- rbind(subject_train, subject_test)

##Extracting measurements for Mean and SD for individual observations
required_features <- features[grep("mean\\(\\)|std\\(\\)",features[,2]),]

for (i in 1:nrow(required_features)) {
  required_features$col[i] <- paste0("V", required_features$V1[i])
}


required_features <- select(required_features, -1)
x_overall <- x_overall[, required_features[,2]]
colnames(x_overall) <- required_features[,1]

##Substituting numbers with activity labels in Y table
y_overall <- merge(y_overall, activity_labels, by = "V1", all.x = TRUE )
y_overall <- as.data.frame(y_overall[,-1])
colnames(y_overall) <- "activity"

##Renaming colnames to get variable names in Final table
subject_overall <- as.data.frame(subject_overall)
colnames(subject_overall) <- "subject"

##creating overall table and required mean tables
overall <- cbind(x_overall, y_overall, subject_overall)
overall_mean <- overall %>% group_by(activity, subject) %>% summarise_all(funs(mean))


##writing final files
write.table(overall, "overall.txt")
write.table(overall_mean, "overall_mean.txt")
