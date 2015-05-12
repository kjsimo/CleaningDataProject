# You should create one R script called run_analysis.R that does the following. 
# Merges the training and the test sets to create one data set. 
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set 
# Appropriately labels the data set with descriptive variable names. From the
# data set in step 4, creates a second, independent tidy data set with the
# average of each variable for each activity and each subject.

library(dplyr)
library(tidyr)
library(plyr)


#####  Read label files
    ########  read activity labels
    data_dir <- "./UCI HAR Dataset"
    paths <- dir(data_dir, pattern = "\\.txt$", full.names = TRUE)
    activity_labels <- read.table(paths[1])
    # Label columns with Key and Description
    names(activity_labels)[1] <- "Key"
    names(activity_labels)[2] <- "Activity_Description"
    head(activity_labels)

    ########  read feature least and create feature labels
    feature_labels <- read.table(paths[3])
    # Label columns with Key and Description
    names(feature_labels)[1] <- "Key"
    names(feature_labels)[2] <- "Feature_Description"
    head(feature_labels)

##########  read subject labels files 
    #### read test subject IDs
    data_dir <- "./UCI HAR Dataset/test"
    paths <- dir(data_dir, pattern = "\\.txt$", full.names = TRUE)
    testSubjectIDs <- read.table(paths[1])
    # Label columns with Key and Description
    names(testSubjectIDs)[1] <- "SubjectID"
    head(testSubjectIDs)
    tail(testSubjectIDs)

    #### read train subject IDs
    data_dir <- "./UCI HAR Dataset/train"
    paths <- dir(data_dir, pattern = "\\.txt$", full.names = TRUE)
    trainsSubjectIDs <- read.table(paths[1])
    # Label columns with Key and Description
    names(trainsSubjectIDs)[1] <- "SubjectID"
    head(trainsSubjectIDs)
    tail(trainsSubjectIDs)


#################   Test Inertial Signals
#   Combine body gyro X, Y, and Z, total acc X, Y, and Z, body acc X, Y, and Z
# 1. Read the files into a list of tables. 
# 2. For each table, add a new column
# that records the original file name (because the file name is often the value
# of an important variable). 
# 3. Combine all tables into a single table.
data_dir <- "./UCI HAR Dataset/test/Inertial Signals"
paths <- dir(data_dir, pattern = "\\.txt$", full.names = TRUE)
names(paths) <- basename(paths)
testInertialSignals <- ldply(paths, read.csv, stringsAsFactors = FALSE)
View(testInertialSignals)

####  read body acc files as a test to see them.  each have 128 columns( 128 readings per window), 2947 rows.
body_acc_x_test <- read.table(paths[1])
View(body_acc_x_test)
body_acc_y_test <- read.table(paths[2])
View(body_acc_y_test)
body_acc_z_test <- read.table(paths[3])
View(body_acc_z_test)

### Test Data Directory read subject_test, X_test, y_test
    # set data directory and paths for reading the files
    data_dir <- "./UCI HAR Dataset/test"
    paths <- dir(data_dir, pattern = "\\.txt$", full.names = TRUE)
    names(paths) <- basename(paths)
    
    # read and view each of the files
    subject <- read.table(paths[1])
    # Label columns with Key and Description
    names(subject)[1] <- "SubjectID"
    View(subject)
    testData <- read.table(paths[2])
    View(testData)
    labels <- read.table(paths[3])
    View(labels)
    
    ## join labels file to labels wth dplyr join
    lbljoin <- left_join(labels, activity_labels, by = c("V1" = "Key"))
head(lbljoin)
tail(lbljoin)
    # combine the files and label the column headings
library(dplyr)
states.df <- data.frame(name = as.character(state.name),
                        region = as.character(state.region), 
                        division = as.character(state.division))
res = mutate(states.df,
             concated_column = paste(name, region, division, sep = '_'))

