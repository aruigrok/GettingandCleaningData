# Getting and Cleaning Data Course Project Assignment
# 23-10-2015

## Background:
## The purpose of this project is to demonstrate your ability to collect, work 
## with, and clean a data set. The goal is to prepare tidy data that can be used 
## for later analysis. You will be graded by your peers on a series of yes/no 
## questions related to the project. You will be required to submit: 
##         1) a tidy data set as described below, 
##         2) a link to a Github repository with your script for performing the 
##            analysis, and 
##         3) a code book that describes the variables, the data, and any 
##            transformations or work that you performed to clean up the data 
##            called CodeBook.md. You should also include a README.md in the repo 
##            with your scripts. This repo explains how all of the scripts work 
##            and how they are connected.  

## One of the most exciting areas in all of data science right now is wearable 
## computing - see for example this article . Companies like Fitbit, Nike, and 
## Jawbone Up are racing to develop the most advanced algorithms to attract new 
## users. The data linked to from the course website represent data collected 
## from the accelerometers from the Samsung Galaxy S smartphone. A full 
## description is available at the site where the data was obtained: 
        
##         http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

## Here are the data for the project: 
        
##         https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

## Assignment:
## You should create one R script called run_analysis.R that does the following. 
##      1. Merges the training and the test sets to create one data set.
##      2. Extracts only the measurements on the mean and standard deviation for 
##         each measurement. 
##      3. Uses descriptive activity names to name the activities in the data set
##      4. Appropriately labels the data set with descriptive variable names. 
##      5. From the data set in step 4, creates a second, independent tidy data 
##         set with the average of each variable for each activity and each subject.

################################################################################
############# MERGE TRAINING AND TEST SETS TO CREATE ONE DATA SET #############
################ AND EXTRACT ONLY MEAN AND STD DEV MEASUREMENTS ################

### Clear the workspace
rm(list=ls())

### Load packages
library(reshape2)

### Set working directory
setwd("~/Getting and Cleaning Data/Project Assignment/")


### Download data
#### Note: Always use method = “curl” when downloading a file on a mac
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!file.exists("GCD_Project_Dataset.zip")){
        print("Downloading GCD_Project_Dataset.zip")
        download.file(fileURL, destfile="GCD_Project_Dataset.zip", method="curl")
} else {print("GCD_Project_Dataset.zip is present in folder")}

### Unzip the data
if(dir.exists("./UCI HAR Dataset/")){
        print("UCI HAR Dataset Directory already exists")
} else{
        unzip("GCD_Project_Dataset.zip", exdir="./")
        }

### Set wd to UCI HAR Dataset/ and get a list of all the files in the folder
setwd("~/Getting and Cleaning Data/Project Assignment/UCI HAR Dataset/")
file_list <- list.files(recursive=TRUE)
file_list


### The README.txt file gives detailed information on the dataset.
### All the files in the subdirectories "Inertial Signals" are not used in this 
### exercise. Read data using read.table() with header=FALSE

### General data
feat <- read.table('features.txt', header=FALSE) # dataset of all direction features
actLab <- read.table('activity_labels.txt', header=FALSE) # links class with activity
#### To extract only data on mean and standard deviation, make a selection variable
extractFeat <- (grepl("-mean..|std..", feat$V2) & !grepl("-meanFreq..", feat$V2))
sum(extractFeat) # must be equal to 66 

#### Training data
subjTrain <- read.table('train/subject_train.txt', header=FALSE) # subject ids for training set
featTrain <- read.table('train/X_train.txt', header=FALSE) # features of training set
actTrain <- read.table('train/y_train.txt', header=FALSE) # activities training set
#### Test data
subjTest <- read.table('test/subject_test.txt', header=FALSE) # subject ids for test set
featTest <- read.table('test/X_test.txt', header=FALSE) # features of training set
actTest <- read.table('test/y_test.txt', header=FALSE) # activities training set

### Merge datasets
#### First concatenate tables by rows
dataSubj <- rbind(subjTrain, subjTest) # combine all subject ids into one variable
dataAct <- rbind(actTrain, actTest) # combine all activity labels into one variable
dataFeat <- rbind(featTrain, featTest) # combine all the training and testing data sets into one dataset

### Use desccriptives to name activities in dataset
dataAct[,1] = actLab[dataAct[,1],2]

##### Then set column names 
colnames(dataSubj)<-c("SubjID")
colnames(dataAct)<- c("Activity_Label")
colnames(dataFeat)<- feat$V2

#### Extract only the mean and std dev for each feature measurement
dataSelectFeat <- dataFeat[,extractFeat]

#### Lastly merge columns to get the complete final combined dataframe
dataUCIHAR <- cbind(dataSubj, dataAct, dataSelectFeat) 


################################################################################
############## LABEL THE DATASET WITH DESCRIPTIVE VARIABLE NAMES ##############

### Relabel the feature names with more descriptive labels
#### Mean & StdDev renames
names(dataUCIHAR) <- gsub("\\()", "", names(dataUCIHAR))
names(dataUCIHAR) <- gsub("-std", "StdDev", names(dataUCIHAR))
names(dataUCIHAR) <- gsub("-mean", "Mean", names(dataUCIHAR))
#### Prefixes
names(dataUCIHAR) <- gsub("^t", "time", names(dataUCIHAR)) # t = time
names(dataUCIHAR) <- gsub("^f", "freq", names(dataUCIHAR)) # f = freq (frequency)
#### Other
names(dataUCIHAR) <- gsub("Acc", "Acceleration", names(dataUCIHAR)) # Acc = Acceleration
names(dataUCIHAR) <- gsub("BodyBody", "Body", names(dataUCIHAR)) # BodyBody = Body
names(dataUCIHAR) <- gsub("Gyro", "Gyroscope", names(dataUCIHAR)) # Gyro = Gyroscope
names(dataUCIHAR) <- gsub("Mag", "Magnitude", names(dataUCIHAR)) # Mag = Magnitude


################################################################################
################### CREATE A SECOND INDEPENDENT TIDY DATASET ###################

### In the next steps a second independent tidy dataset will be created with 
### the acverate of each variable (66 variables) for each activity (6 
### activities) and each subject (30 individuals)

### First get an overview of the current dataset
str(dataUCIHAR) # must contain 68 variables

### Reshape the data to be a tall and skinny dataset by subject ID and activity
dataUCIHARmelt <- melt(dataUCIHAR, id = c("SubjID", "Activity_Label"))

### Create the second independent dataset
avgUCIHAR <- dcast(dataUCIHARmelt, SubjID + Activity_Label ~ variable, mean)

### Write out the tidy dataset
write.table(avgUCIHAR, "UCIHAR_tidy.txt", row.names = FALSE)
