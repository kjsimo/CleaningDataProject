# You should create one R script called run_analysis.R that does the following. 
# 1) Merges the training and the test sets to create one data set. 
# 2) Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3) Uses descriptive activity names to name the activities in the data set 
# 4) Appropriately labels the data set with descriptive variable names. 
# 5) From the data set in step 4, creates a second, independent tidy data set with the
#   average of each variable for each activity and each subject.

library(dplyr)
library(tidyr)
library(plyr)
library(data.table)
library(proto)
library(gsubfn)
library(httr)
library(DBI)
library(RMySQL)
library(jpeg)
library(RSQLite)
library(sqldf)
library(Hmisc)
library(tools)
library(stringr)

#############################################################################
#############################################################################
## Download the files from UCI site
    if(!file.exists("./data")){dir.create("./data")}
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL,destfile="./data/UCIdata.zip",method="curl")
## Unzip the files
    unzip("./data/UCIdata.zip", exdir = "./data/")
## Set paths variable as a file list of the data directory
    data_dir <- "./data/UCI HAR Dataset"
    paths <- list.files(data_dir, full.names = TRUE, recursive = TRUE)
#############################################################################
#   Create A Function to Create a List of files in a directory
#############################################################################
file_list <- function(path) { 
    files <- dir(path, pattern = '\\.txt', full.names = TRUE)
    filename <- dir(path, pattern = '\\.txt', full.names = FALSE)
    file.matrix <- matrix(c(filename, files), ncol = 2)
    file.matrix
}

#############################################################################
#   Create A Set of Feature Labels
#############################################################################
# Read features.txt and create column labels
    feature_labels <- read.table(paths[3])
# Label columns with Key and Description
    names(feature_labels)[1] <- "Feature_Key"
    names(feature_labels)[2] <- "Feature_Description"
# Feature Labels without Special Characters for X column names    
    feature_labels_clean <- str_replace_all(feature_labels[,2], "[^[:alnum:]]", "")
    Xcolnames <- as.character(feature_labels_clean)

#############################################################################
#   Create A Set of Activity Labels
#############################################################################
# Read activity_labels.txt and create column labels
    activity_labels <- read.table(paths[1])
# Label columns with Key and Description
    names(activity_labels)[1] <- "Activity_Key"
    names(activity_labels)[2] <- "Activity_Description"
    
#############################################################################
##  Organize the Test Data Set
# 1) Load the Test Data Files
# 2) X Test column names equal feature_labels_clean
# 3) Add activity_labels to y_test.txt
# 4) Add label to Subject
# 5) Combine subject_test, y_testNamed, X_testNamed
# 6) Rename variables in Inertial signals files, assign column names to each
# file name should be as total_acc_x1-128, total_acc_y1-128, etc.
# 7) Combine dataset and each inertial signal text file
#############################################################################
# 1) Load the Files from the Test Directory
    test_fileList <- file_list("./data/UCI HAR Dataset/test")
    test_fileList <- rbind(test_fileList, file_list("./data/UCI HAR Dataset/test/Inertial Signals"))
    for( i in 1:length(test_fileList[,1])){
        nam <- paste(file_path_sans_ext(test_fileList[i,1]))
        assign(nam, read.table(test_fileList[i,2]))  ## read each file path and name
    }
###########################################################################
# 2) X_test column names equal feature_labels_clean
    X_testNamed <- copy(X_test)
    setnames(X_testNamed, Xcolnames)
###########################################################################
# 3) Add activity_labels to y_test
    y_test <- dplyr::rename(y_test, Activity_Key = V1)
    y_testNamed <- dplyr::left_join(y_test, activity_labels)
###########################################################################
# 4) Add heading label to Subject
    subject_test <- dplyr::rename(subject_test, Subject_Key = V1)
###########################################################################
# 5) Combine subject_test, y_testNamed, X_testNamed
    test_dataset <- cbind(subject_test, y_testNamed, X_testNamed)
###########################################################################
# 6) Rename variables in Inertial signals files
# listDF <- list(body_acc_x_test, body_acc_y_test, body_acc_z_test)    
# file_path_sans_ext(test_fileList[,1])
 
# body_acc_x_test    
    colnam <- vector(mode="character")  
    for( i in 1:ncol(body_acc_x_test)){
        colnam[i] <- paste("body_acc_x",i,sep="")
    }
    setnames(body_acc_x_test, colnam)
    
# body_acc_y_test
    colnam <- vector(mode="character")  
    for(i in 1:ncol(body_acc_y_test)){
        colnam[i] <- paste("body_acc_y", i, sep="")
    }
    setnames(body_acc_y_test, colnam) 

#body_acc_z_test
    colnam <- vector(mode="character")  
    for(i in 1:ncol(body_acc_z_test)){
        colnam[i] <- paste("body_acc_z", i, sep="")
    }
    setnames(body_acc_z_test, colnam) 

# body_gyro_x_test    
    colnam <- vector(mode="character")  
    for( i in 1:ncol(body_gyro_x_test)){
        colnam[i] <- paste("body_gyro_x",i,sep="")
    }
    setnames(body_gyro_x_test, colnam)
    
# body_gyro_y_test
    colnam <- vector(mode="character")  
    for(i in 1:ncol(body_gyro_y_test)){
        colnam[i] <- paste("body_gyro_y", i, sep="")
    }
    setnames(body_gyro_y_test, colnam) 
    
#body_gyro_z_test
    colnam <- vector(mode="character")  
    for(i in 1:ncol(body_gyro_z_test)){
        colnam[i] <- paste("body_gyro_z", i, sep="")
    }
    setnames(body_gyro_z_test, colnam) 
# total_acc_x_test    
    colnam <- vector(mode="character")  
    for( i in 1:ncol(total_acc_x_test)){
        colnam[i] <- paste("total_acc_x",i,sep="")
    }
    setnames(total_acc_x_test, colnam)

# total_acc_y_test
    colnam <- vector(mode="character")  
    for(i in 1:ncol(total_acc_y_test)){
        colnam[i] <- paste("total_acc_y", i, sep="")
    }
    setnames(total_acc_y_test, colnam) 

#total_acc_z_test
    colnam <- vector(mode="character")  
    for(i in 1:ncol(total_acc_z_test)){
        colnam[i] <- paste("total_acc_z", i, sep="")
    }
    setnames(total_acc_z_test, colnam) 
#############################################################################
# 7) Combine test dataset and each inertial signal text file
inertiaData <- cbind(body_acc_x_test, body_acc_y_test, 
                      body_acc_z_test, body_gyro_x_test, body_gyro_y_test,
                      body_gyro_z_test, total_acc_x_test, total_acc_y_test, 
                      total_acc_z_test)
test_dataset <- cbind(test_dataset, inertiaData)

#############################################################################
##  Organize the Training Data Set
# 1) Load the Training Data Files
# 3) Add activity_labels to y_train
# 4) Add label to Subject
# 5) Combine subject_train, y_trainNamed, X_trainNamed
# 6) Rename variables in Inertial signals files, assign column names to each
# file name should be as total_acc_x1-128, total_acc_y1-128, etc.
# 7) Combine dataset and each inertial signal file
#############################################################################
# 1) Load the Files from the train Directory
    train_fileList <- file_list("./data/UCI HAR Dataset/train")
    train_fileList <- rbind(train_fileList, file_list("./data/UCI HAR Dataset/train/Inertial Signals"))
    for( i in 1:length(train_fileList[,1])){
        nam <- paste(file_path_sans_ext(train_fileList[i,1]))
        assign(nam, read.table(train_fileList[i,2]))  ## read each file path and name
    }
###########################################################################
# 2) X_train column names equal feature_labels_clean
## Already set Xcolnames <- as.character(feature_labels_clean)
    X_trainNamed <- copy(X_train)
    setnames(X_trainNamed, Xcolnames)
###########################################################################
# 3) Add activity_labels to y_train.txt
    y_train <- dplyr::rename(y_train, Activity_Key = V1)
    y_trainNamed <- dplyr::left_join(y_train, activity_labels)
###########################################################################
# 4) Add heading label to Subject
    subject_train <- dplyr::rename(subject_train, Subject_Key = V1)
###########################################################################
# 5) Combine subject_train, y_trainNamed, X_trainNamed
    train_dataset <- cbind(subject_train, y_trainNamed, X_trainNamed)
###########################################################################
# 6) Rename variables in Inertial signals files

    # body_acc_x_train    
    colnam <- vector(mode="character")  
    for( i in 1:ncol(body_acc_x_train)){
        colnam[i] <- paste("body_acc_x",i,sep="")
    }
    setnames(body_acc_x_train, colnam)
    
    # body_acc_y_train
    colnam <- vector(mode="character")  
    for(i in 1:ncol(body_acc_y_train)){
        colnam[i] <- paste("body_acc_y", i, sep="")
    }
    setnames(body_acc_y_train, colnam) 
    
    #body_acc_z_train
    colnam <- vector(mode="character")  
    for(i in 1:ncol(body_acc_z_train)){
        colnam[i] <- paste("body_acc_z", i, sep="")
    }
    setnames(body_acc_z_train, colnam) 
    
    # body_gyro_x_train    
    colnam <- vector(mode="character")  
    for( i in 1:ncol(body_gyro_x_train)){
        colnam[i] <- paste("body_gyro_x",i,sep="")
    }
    setnames(body_gyro_x_train, colnam)
    
    # body_gyro_y_train
    colnam <- vector(mode="character")  
    for(i in 1:ncol(body_gyro_y_train)){
        colnam[i] <- paste("body_gyro_y", i, sep="")
    }
    setnames(body_gyro_y_train, colnam) 
    
    #body_gyro_z_train
    colnam <- vector(mode="character")  
    for(i in 1:ncol(body_gyro_z_train)){
        colnam[i] <- paste("body_gyro_z", i, sep="")
    }
    setnames(body_gyro_z_train, colnam) 
    # total_acc_x_train    
    colnam <- vector(mode="character")  
    for( i in 1:ncol(total_acc_x_train)){
        colnam[i] <- paste("total_acc_x",i,sep="")
    }
    setnames(total_acc_x_train, colnam)
    
    # total_acc_y_train
    colnam <- vector(mode="character")  
    for(i in 1:ncol(total_acc_y_train)){
        colnam[i] <- paste("total_acc_y", i, sep="")
    }
    setnames(total_acc_y_train, colnam) 
    
    #total_acc_z_train
    colnam <- vector(mode="character")  
    for(i in 1:ncol(total_acc_z_train)){
        colnam[i] <- paste("total_acc_z", i, sep="")
    }
    setnames(total_acc_z_train, colnam) 
#############################################################################
# 7) Combine dataset and each inertial signal text file
    inertiaData <- cbind(body_acc_x_train, body_acc_y_train, 
                         body_acc_z_train, body_gyro_x_train, body_gyro_y_train,
                         body_gyro_z_train, total_acc_x_train, total_acc_y_train, 
                         total_acc_z_train)
    train_dataset <- cbind(train_dataset, inertiaData)

#############################################################################
##  Combine the full Test and full Training Data Sets
#############################################################################
    dataset <- rbind(test_dataset, train_dataset)

#############################################################################
# Extract the measurements on the mean and standard deviation for each measurement
#############################################################################
    selected.data <- dataset[1:3]
    selected.data.mean <- dataset[ ,grep("mean", colnames(dataset), ignore.case = TRUE)]
    selected.data.std <- dataset[ ,grep("std", colnames(dataset), ignore.case = TRUE)]
    selected.data <- cbind(selected.data, selected.data.mean, selected.data.std)
