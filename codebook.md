CODE BOOK FOR THE TIDY DATASET, tidyDS
===================================================================================================================================================
The tidy dataset, tidyDS.txt, was prepared by analysising the data provided by the UCI Machine Learning Repository, see http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Information about the data set Information provided by the UCI Machine Learning Repository
===================================================================================================================================================
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.
(Ref : http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

How the tidy dataset, tidyDS.txt, was prepared
===================================================================================================================================================
The dplyr package is used to analyse the data.
The columns names for the training and test sets is provided in the features.txt and is read into a dplyr library data table. All the rows are filter for columns names containing either std or mean, using the dplyr filter function. The row numbers of these fields are stored in a vector. The activity_labels.txt is read into a data table and is used to replace the activity id by the actual activity.

A function, createDataSet, is used to create the dataset for a particular dataset type, e.g. train or test. The function first reads the data into a data table and extracts only those columns which contain either std or mean by using the row number vector. The function then gets the subject id for each row of the data, thereafter it gets the activity id for each row of the data. The three tables are then joined, using the dplyr inner_join function, to provide a data set which has the subject, the activity and then the standard deviation or mean columns.

A test dataset, testDS, is created by calling the function, createDataSet, with parameter test. Similarly the train dataset, trainDS, is created using parameter train. The two datasets are then merged, using rbind, to form one tidy dataset, DS.

The tidy dataset, tidyDS, is an independent tidy data set with the average of each variable for each activity and each subject of the tidy dataset DS. The dataset DS are grouped by Subject and Activity, using the dplyr group_by function, and then the average of each variable is calculated using the dplyr summarise_each(funs(mean)) command.

Field names of the tidy dataset, tidyDS.txt
=================================================================================================================================================== 
Subject - An integer identifier of the subject who carried out the experiment, values ranging from 1 to 30

Activity - One of six activities: WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING

tBodyAcc-mean...X               
tBodyAcc-mean...Y               
tBodyAcc-mean...Z               
tBodyAcc-std...X               
tBodyAcc-std...Y                
tBodyAcc-std...Z               
tGravityAcc-mean...X            
tGravityAcc-mean...Y           
tGravityAcc-mean...Z           
tGravityAcc-std...X             
tGravityAcc-std...Y             
tGravityAcc-std...Z            
tBodyAccJerk-mean...X           
tBodyAccJerk-mean...Y           
tBodyAccJerk-mean...Z           
tBodyAccJerk-std...X           
tBodyAccJerk-std...Y            
tBodyAccJerk-std...Z            
tBodyGyro-mean...X              
tBodyGyro-mean...Y             
tBodyGyro-mean...Z              
tBodyGyro-std...X               
tBodyGyro-std...Y               
tBodyGyro-std...Z              
tBodyGyroJerk-mean...X          
tBodyGyroJerk-mean...Y          
tBodyGyroJerk-mean...Z          
tBodyGyroJerk-std...X          
tBodyGyroJerk-std...Y           
tBodyGyroJerk-std...Z           
tBodyAccMag-mean()              
tBodyAccMag-std()              
tGravityAccMag-mean()           
tGravityAccMag-std()            
tBodyAccJerkMag-mean()          
tBodyAccJerkMag-std()          
tBodyGyroMag-mean()            
tBodyGyroMag-std()              
tBodyGyroJerkMag-mean()         
tBodyGyroJerkMag-std()         
fBodyAcc-mean...X               
fBodyAcc-mean...Y               
fBodyAcc-mean...Z               
fBodyAcc-std...X               
fBodyAcc-std...Y                
fBodyAcc-std...Z                
fBodyAcc-meanFreq...X           
fBodyAcc-meanFreq...Y          
fBodyAcc-meanFreq...Z           
fBodyAccJerk-mean...X           
fBodyAccJerk-mean...Y           
fBodyAccJerk-mean...Z          
fBodyAccJerk-std...X            
fBodyAccJerk-std...Y            
fBodyAccJerk-std...Z            
fBodyAccJerk-meanFreq...X      
fBodyAccJerk-meanFreq...Y       
fBodyAccJerk-meanFreq...Z       
fBodyGyro-mean...X              
fBodyGyro-mean...Y             
fBodyGyro-mean...Z              
fBodyGyro-std...X               
fBodyGyro-std...Y               
fBodyGyro-std...Z              
fBodyGyro-meanFreq...X          
fBodyGyro-meanFreq...Y          
fBodyGyro-meanFreq...Z          
fBodyAccMag-mean()             
fBodyAccMag-std()               
fBodyAccMag-meanFreq()          
fBodyBodyAccJerkMag-mean()      
fBodyBodyAccJerkMag-std()      
fBodyBodyAccJerkMag-meanFreq()  
fBodyBodyGyroMag-mean()         
fBodyBodyGyroMag-std()          
fBodyBodyGyroMag-meanFreq()    
fBodyBodyGyroJerkMag-mean()     
fBodyBodyGyroJerkMag-std()      
fBodyBodyGyroJerkMag-meanFreq()