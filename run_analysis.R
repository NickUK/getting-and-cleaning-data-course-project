##
# 0. Download and unzip the data
##

dataLocation = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

# Download the file if it doesn't exist
if (!file.exists(zipFile)) {
    download.file(dataLocation, zipFile, mode = "wb")
}

# Unzip files into the data folder (if its not already extracted)
if (!dir.exists("data")) {
    unzip(zipFile, exdir="data")
}

##
# 1. Merges the training and the test sets to create one data set.
##

# Read labels and features
activityLabels <- read.table("data/UCI HAR Dataset/activity_labels.txt")
colnames(activityLabels) <- c("activityId", "activityName")
features <- read.table("data/UCI HAR Dataset/features.txt")

# Read data (test)
xTest <- read.table("data/UCI HAR Dataset/test/X_test.txt") 
colnames(xTest) <- features[,2]
yTest <- read.table("data/UCI HAR Dataset/test/Y_test.txt")
colnames(yTest) <- c("activityId")
subjectTest <- read.table("data/UCI HAR Dataset/test/subject_test.txt")
colnames(subjectTest) <- c("subjectId")

# Read data (training)
xTrain <- read.table("data/UCI HAR Dataset/train/X_train.txt")
colnames(xTrain) <- features[,2]
yTrain <- read.table("data/UCI HAR Dataset/train/Y_train.txt")
colnames(yTrain) <- c("activityId")
subjectTrain <- read.table("data/UCI HAR Dataset/train/subject_train.txt")
colnames(subjectTrain) <- c("subjectId")

# Merge the columns together for both the test and training data
trainingData <- cbind(subjectTrain, xTrain, yTrain)
testData <- cbind(subjectTest, xTest, yTest)

# Merge the rows together so that we have one dataset
mergedData <- rbind(trainingData, testData)

##
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
##

# Logical vector indicating columns with mean or standard deviation as well as the activity and subject id
validData <- grepl("subjectId|activityId|mean\\(\\)|std\\(\\)", colnames(mergedData))
meanAndStdTable <- mergedData[ , validData]

##
# 3. Uses descriptive activity names to name the activities in the data set
##

# Merge the activity label in
meanAndStdTable <- merge(meanAndStdTable, activityLabels, all.x=TRUE)

##
# 4. Appropriately labels the data set with descriptive variable names.
## 

# Get the colnames
colNames <- colnames(meanAndStdTable)

# Replace some of the variable names.
colNames <- gsub("^t", "time", colNames)
colNames <- gsub("^f", "freq", colNames)
colNames <- gsub("-mean\\(\\)", "Mean", colNames)
colNames <- gsub("-std\\(\\)", "Std", colNames)
colNames <- gsub("BodyBody", "Body", colNames)

# Set the colnames
colnames(meanAndStdTable) <- colNames

##
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
## 

# Don't include the activity fields or subject id field in the mean and use the subject Id and activity name for the aggregation.
finalData <- aggregate(meanAndStdTable[, -c(1, 2, 69)], list(meanAndStdTable$subjectId, meanAndStdTable$activityName), mean)

# Add the column labels back in
colnames(finalData)[1] <- "subjectId"
colnames(finalData)[2] <- "activity"

# Finally write it out to 'tidy.txt'
write.table(finalData, "tidy.txt", quote = FALSE, row.names = FALSE)

