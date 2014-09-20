## Coursera: Getting and Cleaning Data
## Course Project
## Developer: Mike Cilliers
## Date: 15 Sep 2014
##    Requirements:
##    1. Merges the training and the test sets to create one data set.
##    2. Extracts only the measurements on the mean and standard deviation for each measurement. 
##    3. Uses descriptive activity names to name the activities in the data set
##    4. Appropriately labels the data set with descriptive variable names. 
##    5. From the data set in step 4, creates a second, independent tidy data set 
##       with the average of each variable for each activity and each subject.

setwd("~/R/workspace/getCleanData/Assignment/UCI HAR Dataset")

currwd <- getwd()

suppressMessages(library(dplyr))
#Get the activity lables
 activityLables.tbl <- tbl_df(read.table("activity_labels.txt", header = FALSE, col.names = c("ActivityId", "Activity")))
#Get the column names for the sensor measurements datasets
 cols.tbl <- tbl_df(read.table("features.txt", header = FALSE))
 columnNames <- cols.tbl[ ,2]

#Get the column numbers of the measurements on the mean and standard deviation for each measurement.
 cols.tbl <- filter(cols.tbl, grepl('mean|std', V2))
 columnNums <- as.vector(cols.tbl[ ,1])
 rm("cols.tbl")
 
createDataSet <- function (dataType) {
 
	#Get the data set for a particular dataType; either test or train
	 readdir <- paste(currwd, "/", dataType, sep="")
	 setwd(readdir)
	
	 filename <- paste("X_", dataType, ".txt", sep = "")
	 data.tbl <- tbl_df(read.table(filename, header = FALSE, col.names = columnNames))
	 ## Extract only the measurements on the mean and standard deviation for each measurement.
	 data.tbl <- data.tbl[ ,columnNums]
	 data.tbl <- mutate(data.tbl, rowId = row_number())
	 filename <- paste("subject_", dataType, ".txt", sep = "")
	 ## Get the subjects for the measurements dataset
	 subjects.tbl <- tbl_df(read.table(filename, header = FALSE, col.names = "Subject"))
	 subjects.tbl <- mutate(subjects.tbl, rowId = row_number())
	 
	 ## Get the activities for the measurements dataset
	 filename <- paste("y_", dataType, ".txt", sep = "")
	 activity.tbl <- tbl_df(read.table(filename, header = FALSE, col.names = "ActivityId"))
	 ## Uses descriptive activity names to name the activities in the data set
	 activity <- inner_join(activity.tbl, activityLables.tbl, by = "ActivityId")
	 activity <- select(activity, Activity)
	 activity <- mutate(activity, rowId = row_number())
	#merge or join all the test data
	ds1 <- inner_join(subjects.tbl, activity, by = "rowId")
	ds2 <- inner_join(ds1, data.tbl, by = "rowId")
	rm("ds1", "data.tbl", "activity", "activity.tbl", "subjects.tbl")
	ds2 <- select(ds2, -rowId)
	return(ds2)
}

## Get the test dataset with descriptive activity names and descriptive variable names
testDS <- createDataSet("test")

## Get the train dataset with descriptive activity names and descriptive variable names
trainDS <- createDataSet("train")
## Merge the training and the test sets to create one data set DS.
DS <- rbind(testDS, trainDS)

## A second, independent tidy data set, tidyDS, with the average of each variable for each activity and each subject
tidyDS <- DS %>% group_by(Subject, Activity) %>% summarise_each(funs(mean))
