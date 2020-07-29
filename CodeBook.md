#This is the code book for the project

##How to get to the tinyData.txt:
1. Download data from the link below and unzip it into working directory of R Studio.
2. Execute the R script.

## About the source data
The source data are from the Human Activity Recognition Using Smartphones Data Set. A full description is available at the site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
Here are the data for the project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

## About R script
File with R code "run_analysis.R" performs the 5 following steps (in accordance assigned task of course work):   
1. Reading in the files and merging the training and the test sets to create one data set.   
  1.1 Reading files    
    Reading trainings tables   
    Reading testing tables   
    Reading feature vector   
    Reading activity labels   
  1.2 Assigning variable names   
  1.3 Merging all data in one set   
2. Extracting only the measurements on the mean and standard deviation for each measurement   
3. Using descriptive activity names to name the activities in the data set   
4. Creating a second, independent tidy data set with the average of each variable for each activity and each subject   


The code assumes all the data is present in the same folder, un-compressed and without names altered.

## About variables:   
* `x_train`, `y_train`, `x_test`, `y_test`, `subject_train` and `subject_test` contain the data from the downloaded files.
* `x_data`, `y_data` and `subject_data` merge the previous datasets to further analysis.
* `features` contains the correct names for the `x_data` dataset, which are applied to the column names stored in
