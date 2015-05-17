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
##  Merge the Training and test Sets to Create One Data Set
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
# Feature Labels without Special Characters    
    feature_labels_clean <- str_replace_all(feature_labels[,2], "[^[:alnum:]]", "")

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
# 2) X Test column names equal features.txt 1-561
# 3) Add activity_labels to y_test.txt
# 4) Add label to Subject
# 5) Combine subject_test, y_testNamed, X_testNamed
# 6) Inertial signals directory files, assign column names to each
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
# 2) X_test column names equal features.txt 1-561
Xcolnames <- as.character(feature_labels_clean)
X_testNamed <- copy(X_test)
setnames(X_testNamed, Xcolnames)

# 3) Add activity_labels to y_test.txt
y_test <- dplyr::rename(y_test, Activity_Key = V1)
y_testNamed <- dplyr::left_join(y_test, activity_labels)

# 4) Add heading label to Subject
subject_test <- dplyr::rename(subject_test, Subject_Key = V1)

# 5) Combine subject_test, y_testNamed, X_testNamed
test_dataset <- cbind(subject_test, y_testNamed, X_testNamed)

#############################################################################
##  Organize the Training Data Set
# 1) X Train column names equal features.txt 1-561
# 2) add activity_labels to y_train.txt
# 3) cbind subject_train.txt, y_train.txt, X_train.txt
# 4) Inertial signals directory files, assign column names to each
#     file name should be as total_acc_x1-128, total_acc_y1-128, etc.
# 5) cbind dataset and each inertial signal text file
#############################################################################

#############################################################################
##  Combing the Full Test and full Training Data Sets

#############################################################################



#####  Read label files
    ########  read activity labels

    

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

