XTrain <- XTest <- NULL
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

downloadData <- function() {
  downloadDir <- "data"
  
  zipFile <- filePath(downloadDir, "dataset.zip")
  if(!file.exists(zipFile)) 
    { download.file(url, zipFile, method = "curl") }
  
  dataDir <- paste(downloadDir,"/UCI HAR Dataset", sep="")
  if(!file.exists(dataDir)) 
    { unzip(zipFile, exdir = downloadDir) }
  
  list.files(downloadDir)
  list.dirs(downloadDir, full.names = TRUE, recursive = TRUE)
  
  dataDir
}

dataDir <- downloadData()

# Merge the training and the test sets to create one data set.

readData <- function(path) {
  read.table(filePath(dataDir, path))
}

# Read and cache XTrain and XTest data
if(is.null(XTrain)) 
  { XTrain <<- readData("train/X_train.txt") }
if(is.null(XTest))  
  { XTest  <<- readData("test/X_test.txt") }
merged <- rbind(XTrain, XTest)

featureNames <- readData("features.txt")[, 2]
names(merged) <- featureNames

# Extract only the measurements on the mean and standard deviation for each measurement.
# Limit to columns with feature names matching mean() or std():
matches <- grep("(mean|std)\\(\\)", names(merged))
limited <- merged[, matches]

# Use descriptive activity names to name the activities in the data set.
# Get the activity data and map to nicer names:
yTrain <- readData("train/y_train.txt")
yTest  <- readData("test/y_test.txt")
yMerged <- rbind(yTrain, yTest)[, 1]

activityNames <-
  c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")
activities <- activityNames[yMerged]


# Appropriately label the data set with descriptive variable names.
# Change t to Time, f to Frequency, mean() to Mean and std() to StdDev
# Remove extra dashes and BodyBody naming error from original feature names
names(limited) <- gsub("^t", "Time", names(limited))
names(limited) <- gsub("^f", "Frequency", names(limited))
names(limited) <- gsub("-mean\\(\\)", "Mean", names(limited))
names(limited) <- gsub("-std\\(\\)", "StdDev", names(limited))
names(limited) <- gsub("-", "", names(limited))
names(limited) <- gsub("BodyBody", "Body", names(limited))

# Add activities and subject with nice names
subjectTrain <- readData("train/subject_train.txt")
subjectTest  <- readData("test/subject_test.txt")
subjects <- rbind(subjectTrain, subjectTest)[, 1]

tidy <- cbind(Subject = subjects, Activity = activities, limited)

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidydata_average_sub<- ddply(tidy, c("Subject","Activity"), numcolwise(mean))
write.table(tidydata_average_sub,file="tidydata.txt")


