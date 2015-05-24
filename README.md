# CleaningDataProject
## Course project for Getting and Cleaning Data
The run_analysis.R script can be run as long as the Samsung accelerometer data is in the working directory.  

1. Source data can be found at the links below
2. A full description is available at the site where the data was obtained:  http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
3. Here are the data for the project:  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
4. run_analysis.html is the R notebook of the R script run_analysis.
5. Codebook.md contains the codebook for the run_analysis script. 

The run_analysis.R script performs the following.

1. Merges the training and the test sets to create one data set. 
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set 
4. labels the data set with descriptive variable names. 
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject and outputs that tidy data set to txt and csv called average.activity.txt and .csv, respectively.

Refer to Codebook.md for information on the variables and transformations.
