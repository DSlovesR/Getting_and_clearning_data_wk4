#file anme - run_analysis.R 
# 1-  Merges the training and the test sets to create one data set.
# 2-  Extracts only the measurements on the mean and standard deviation for each measurement.
# 3-  Uses descriptive activity names to name the activities in the data set
# 4-  Appropriately labels the data set with descriptive variable names.
# 5-  From the data set in step 4, creates a second, independent tidy data set with the  #average of each variable for each activity and each subject.


# load packages for file read and data manipulation
library(dplyr)
library(data.table)

#set working folder
setwd("D:\\DS\\DS-R\\Course3\\Week4")

############download data file

#download project file in current folder

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = ".//UCIDataset.zip", mode= "wb")


#unzip and extract contents in current path
unzip(".//UCIDataset.zip" )

#check wether file directory exists
if( !file.exists(".//UCI HAR Dataset"))
{
   Stop("No data folder found!")
}

############ Load data

#load activities
Activity <- fread( "./UCI HAR Dataset/Activity_labels.txt", col.names = c("CODE", "DESC"))

#load features
features <- fread("./UCI HAR Dataset/features.txt",  col.names = c("CODE", "DESC"))


# load training data
xTrain <- fread("./UCI HAR Dataset/train/X_train.txt")
yTrain <- fread("./UCI HAR Dataset/train/y_train.txt", col.names = "Label")
sTrain <- fread("./UCI HAR Dataset/train/subject_train.txt",col.names = "Subject")


# load testing data
xTest <- fread("./UCI HAR Dataset/test/X_test.txt")
yTest <- fread("./UCI HAR Dataset/test/y_test.txt",col.name="Label")
sTest <- fread("./UCI HAR Dataset/test/subject_test.txt",col.names = "Subject")


# 1-  Merges the training and the test sets to create one data set.
#merge training and test dataset
xDS <- rbind( xTrain, xTest)
yDS <- rbind( yTrain, yTest)
sDS <- rbind( sTrain, sTest)


# 2-  Extracts only the measurements on the mean and standard deviation for each measurement.
#grep mean and std features index only
fIndex <- grep("\\-(mean|std)[()]", features[, DESC])

#Merge data
xDS <- xDS[, ..fIndex]
names(xDS)  <- features[fIndex, DESC]

mergedDS <- cbind( sDS, yDS, xDS)

# 3-  Uses descriptive activity names to name the activities in the data set
mergedDS$Label <- Activity[ yDS$Label, DESC]


# 4-  Appropriately labels the data set with descriptive variable names.


names(mergedDS) <- gsub("Acc", "Accelerometer", names(mergedDS))
names(mergedDS) <- gsub("Gyr", "Gryoscope", names(mergedDS))
names(mergedDS) <- gsub("Mag", "Magnitude", names(mergedDS))
names(mergedDS) <- gsub("BodyBody", "Body", names(mergedDS))
names(mergedDS) <- gsub("^t", "Time", names(mergedDS))
names(mergedDS) <- gsub("^f", "Frequency", names(mergedDS))
names(mergedDS) <- gsub("[()]", "", names(mergedDS))


# 5-  From the data set in step 4, creates a second, independent tidy data set with the  #average of each variable for each activity and each subject.

#compute mean for all data points
tidy <- mergedDS %>%
         group_by ( Subject, Label) %>%
         summarise_all( funs( mean))


#Create data set as a txt file with write.table() using row.name=FALSE 
write.table( tidy, "tidyData.txt", row.names = FALSE)
