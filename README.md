# CourseProject---Getting-and-Cleaning-Data
This markdown file explains a code that creates a "tidy2.txt" file, using a zip file with datasets from Human Activity Recognition as basis for it. 

At the beginning of my code I read all the files given for the task - activity labels, all the measurements of train and test groups. Then I should merge all the train data with each other, and then - all the test data. After that we should bind it together. Then we must add the names of variables (columns) from the "features" file. Then we should add activity type for each measurement - so that they are all activity types for each person (of the 30). After that - select only mean and sd measurements. And then - make an average for all the variables, activities and subjects. 

getwd()
if(!file.exists("data")){dir.create("data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./dara.WearComp.zip", method="internal")
unzip("dara.WearComp.zip")

list.files(getwd())
setwd("./UCI HAR Dataset")




#Creating a new dataset „train“. There is one disadvantage of my method: I should manually see, #how much columns has this table, to know, what number should new columns have. 
train = read.table("./train/X_train.txt")
train[,562] = read.table("./train/Y_train.txt")
train[,563] = read.table("./train/subject_train.txt")
#In the same way I created a table for “test” data.
test = read.table("./test/X_test.txt")
test[,562] = read.table("./test/Y_test.txt")
test[,563] = read.table("./test/subject_test.txt")
# There is an alternate way to bind x data with y and subject:
# > xtest <- read.table("./test/X_test.txt")
# > ytest <- read.table("./test/y_test.txt")
# > subtest <- read.table("./test/subject_test.txt")
# > ytrain <- read.table("./train/y_train.txt")
# > xtrain <- read.table("./train/X_train.txt")
# > subtrain <- read.table("./train/subject_train.txt")
# > test <- cbind(xtest, ytest, subtest)
# > train <- cbind(xtrain, ytrain, subtrain)

# Merge train and test data
dataSet <- rbind(test, train)
# Read features and adopt a format of the variables names for our needs with a help of “gsub” function. 
features = read.table("./features.txt")
features[,2] = gsub('-mean', 'Mean', features[,2])
features[,2] = gsub('-std', 'Std', features[,2])
features[,2] = gsub('[-()]', '', features[,2])


# Extract  only the mean and standard deviation data for measurements
meanSd<- grep(".*Mean.*|.*Std.*", features[,2])
# First reduce the features table to what we want
features <- features[meanSd,]
# Now add the last two columns (subject and activity)
meanSd <- c(meanSd, 562, 563)
# And remove the unwanted columns from dataSet
dataSet <- dataSet[,meanSd]
# Add the column names from the “features” to dataSet
colnames(dataSet) <- c(features$V2, "Activity", "Subject")
colnames(dataSet) <- tolower(colnames(dataSet))

# Read activity labels
activityLabels = read.table("./activity_labels.txt")

currentActivity = 1
for (c in activityLabels$V2) {
dataSet$activity <- gsub(currentActivity, c, dataSet$activity)
currentActivity <- currentActivity + 1
}

dataSet$activity <- as.factor(dataSet$activity)
dataSet$subject <- as.factor(dataSet$subject)

tidy = aggregate(dataSet, by=list(activity = dataSet$activity, subject=dataSet$subject), mean)

write.table(tidy, "tidy.txt", sep="\t", row.name=FALSE)

# Task 5
Library(reshape)
Library(reshape2)
dataSet.melted <- melt(dataSet, id = c("subject", "activity"))
dataSet.mean <- dcast(dataSet.melted, subject + activity ~ variable, mean)

write.table(dataSet.mean, "tidy2.txt", row.names = FALSE, quote = FALSE)
