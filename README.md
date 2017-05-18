# Coursera_Getting_And_Cleaning_Data
Data Science Specialization.. 3rd course


Getting and Cleaning Data - Project

Source dataset https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip.

You should create one R script called run_analysis.R that does the following.
Merges the training and the test sets to create one data set.
Extracts only the measurements on the mean and standard deviation for each measurement.
Uses descriptive activity names to name the activities in the data set
Appropriately labels the data set with descriptive activity names.
Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Notes

Only vaiables containing mean() & std() are used.
Requires the plyr & reshape2 packages.
Assumes the dataset is unzipped in the current directory.
Constructed using the following:

> version
platform       x86_64-apple-darwin12.4.0
arch           x86_64
os             darwin12.4.0
system         x86_64, darwin12.4.0
status
major          3
minor          0.1
year           2013
month          05
day            16
svn rev        62743
language       R
version.string R version 3.0.1 (2013-05-16)
nickname       Good Sport
Running

$ Rscript run_analysis.R
Yields tidy.txt & tidy.mean.txt.
