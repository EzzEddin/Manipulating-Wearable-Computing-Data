# Course 3 Project
# Manipulating Wearable Computing Data

# Done by Ezz El Din Abdullah
# Feb 21, 2018

features <- read.table("./UCI HAR Dataset/features.txt")
trainSet <- read.table("./UCI HAR Dataset/train/X_train.txt") #reading the training set
testSet <- read.table("./UCI HAR Dataset/test/X_test.txt") #reading the test set
dat <- merge(trainSet, testSet, all = TRUE) #merged data set

meanORstdmeas <- features$V2[grep("std|[Mm]ean", features$V2)] # extracting only mean and standard deviation of measurements

# Descriptive naming of activity
activNames <- read.table("./UCI HAR Dataset/activity_labels.txt") # reading the activity labels
y_test_labels <- read.table("./UCI HAR Dataset/test/y_test.txt") # reading the test labels
testSet$ActivityLabel <-  y_test_labels$V1
testSet$ActivityLabel[grep(i,testSet$ActivityLabel)] <- activNames[,2][3]
for(i in seq_along(activNames[,2]))
{
  ind <- grep(i,testSet$ActivityLabel)
  testSet$ActivityLabel[ind] <- as.character(activNames[,2][i]) # as activNames is a factor
}
# naming the train set
y_train_labels <- read.table("./UCI HAR Dataset/train/y_train.txt") # reading the training labels
trainSet$ActivityLabel <-  y_train_labels$V1
trainSet$ActivityLabel[grep(i,trainSet$ActivityLabel)] <- activNames[,2][3]
for(i in seq_along(activNames[,2]))
{
  ind <- grep(i,trainSet$ActivityLabel)
  trainSet$ActivityLabel[ind] <- as.character(activNames[,2][i]) # as activNames is a factor
}
datSet <- merge(trainSet, testSet, all = TRUE) #merged data set

# Descriptive variable names
names(datSet)[1:561] <- as.character(features[,2])

# Taking the average of each variable for each activity (walking, standing, etc) and each subject ()
avg <- data.frame()
for(i in seq_along(1:561))
{
  avg <- rbind(avg, tapply(as.numeric(datSet[,i]), datSet$ActivityLabel, mean))
}
names(avg) <- as.character(activNames[,2])
write.table(avg,"avgData.txt",sep="\t",row.names=FALSE)
