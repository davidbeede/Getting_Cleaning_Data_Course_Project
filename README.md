# Getting_Cleaning_Data_Course_Project
##This script creates a tidy data set that summarizes some of the data
##from the Human Activity Recognition Using Smartphones Data Set found at: 
#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#This script performs the following operations:
# 0. Reads in training, test, features, activities, activity names, and subject
#        text files.
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each 
#         measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set 
#        with the average of each variable for each activity and each subject.

setwd("C:/Users/Public/Documents/Dave/JHU_DataScience/GettingAndCleaningData/Course Project")
library(dplyr)
#read in and transpose features.txt to use as column names
features <- read.table("features.txt")
str(features)
featuresonly <- select(features, V2)
str(featuresonly)
variablebad <- t(featuresonly)
#fix variable names so they are unique and legitimate in R
varnamesgood <- make.names(variablebad, unique = TRUE)
##test data section followed by train data section 
##if I had time to figure out how to do this in a loop, I would...
#read in test data and rename variables using variablelst
test_data <- read.table("X_test.txt")
str(test_data)
head(test_data, n=1)
names(test_data) <- varnamesgood
head(test_data, n=1)
#just get mean variables
test_data_mean <- select(test_data, contains("mean.."))
test_data_mean <- select(test_data_mean, -contains("gravityMean"))
#just get std variables
test_data_std <- select(test_data, contains("std.."))
#column bind mean and std variables
test_mean_std <- cbind(test_data_mean, test_data_std)
#read in subject codes for test data froom subject_test.txt
subjects <- read.table("subject_test.txt")
head(subjects)
names(subjects) <- "subjects"
str(subjects)
#read in activities for test data
activities <- read.table("Y_test.txt")
str(activities)
names(activities) <- "activities"
str(activities)
#column bind test data with subjects and activities data
test_data_all <- cbind(subjects, activities, test_mean_std)
head(test_data_all, n=1)
str(test_data_all)

#read in train data and rename variables using variablelst
train_data <- read.table("X_train.txt")
str(train_data)
head(train_data, n=1)
names(train_data) <- varnamesgood
head(train_data, n=1)
#just get mean variables
train_data_mean <- select(train_data, contains("mean.."))
train_data_mean <- select(train_data_mean, -contains("gravityMean"))
#just get std variables
train_data_std <- select(train_data, contains("std.."))
#column bind mean and std variables
train_mean_std <- cbind(train_data_mean, train_data_std)

#read in subject codes for train data froom subject_train.txt
subjects <- read.table("subject_train.txt")
head(subjects)
names(subjects) <- "subjects"
str(subjects)
#read in activities for train data
activities <- read.table("Y_train.txt")
str(activities)
names(activities) <- "activities"
str(activities)
#column bind train data with subjects and activities data
train_data_all <- cbind(subjects, activities, train_mean_std)
head(train_data_all, n=1)
str(train_data_all)
#now row bind test and train data into a single data set
data_all <- rbind(test_data_all, train_data_all)
#clean up variable names
names(data_all) <- gsub(".","",names(data_all), fixed = TRUE)
names(data_all) <- gsub("meanX","Xmean",names(data_all), fixed = TRUE)
names(data_all) <- gsub("meanY","Ymean",names(data_all), fixed = TRUE)
names(data_all) <- gsub("meanZ","Zmean",names(data_all), fixed = TRUE)
names(data_all) <- gsub("stdX","Xstd",names(data_all), fixed = TRUE)
names(data_all) <- gsub("stdY","Ystd",names(data_all), fixed = TRUE)
names(data_all) <- gsub("stdZ","Zstd",names(data_all), fixed = TRUE)

by_subs_acts <- group_by(data_all, subjects, activities)
summ_data_all <- summarise_each(by_subs_acts, funs(mean), -(subjects:activities) )
activity_labels <- read.table("activity_labels.txt")
summ_data_active <- merge(summ_data_all, activity_labels, by.x = "activities", by.y = "V1")
summ_data_active <- rename(summ_data_active, activity_desc = V2)
summ_data_active <- summ_data_active[,c(2,1,69,3:68)]
summ_data_active <- arrange(summ_data_active, subjects, activities)
write.table(summ_data_active, file = "summ_activity_recog_smartphones.txt", row.names = FALSE)
