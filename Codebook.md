CodeBook
================
Sanjay Rawat
January 17, 2020

Assignment- Getting and cleaning data
=====================================

Codebook describes the steps to get data from the supplied URL and perform data clearning activities as requested below:

-   1- Merges the training and the test sets to create one data set.
-   2- Extracts only the measurements on the mean and standard deviation for each measurement.
-   3- Uses descriptive activity names to name the activities in the data set
-   4- Appropriately labels the data set with descriptive variable names.
-   5- From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

All steps are performed in separate script run\_analysis.R to produce expected result. Below few keys steps in R scripts are:

Load required libraries to perform data load and clean operations.
------------------------------------------------------------------

``` r
library( dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library( data.table)
```

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

Download data using download.file() and copy into local folder.
---------------------------------------------------------------

``` r
#download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dat#aset.zip", destfile = ".//UCIDataset.zip", mode= "wb")

#load activities
Activity <- fread( "./UCI HAR Dataset/Activity_labels.txt", col.names = c("CODE", "DESC"))
message( "Activity- Total observation - " , nrow(Activity) ," rows and ", ncol(Activity) ," varaibles.")
```

    ## Activity- Total observation - 6 rows and 2 varaibles.

``` r
#load features
features <- fread("./UCI HAR Dataset/features.txt",  col.names = c("CODE", "DESC"))
message( "features- Total observation - " , nrow(features) ," rows and ", ncol(features) ," varaibles.")
```

    ## features- Total observation - 561 rows and 2 varaibles.

``` r
# load training data
xTrain <- fread("./UCI HAR Dataset/train/X_train.txt")
message( "XTrain- Total observation - " , nrow(xTrain) ," rows and ", ncol(xTrain) ," varaibles.")
```

    ## XTrain- Total observation - 7352 rows and 561 varaibles.

``` r
yTrain <- fread("./UCI HAR Dataset/train/y_train.txt", col.names = "Label")
message( "yTrain- Total observation - " , nrow(yTrain) ," rows and ", ncol(yTrain) ," varaibles.")
```

    ## yTrain- Total observation - 7352 rows and 1 varaibles.

``` r
sTrain <- fread("./UCI HAR Dataset/train/subject_train.txt",col.names = "Subject")
message( "sTrain- Total observation - " , nrow(sTrain) ," rows and ", ncol(sTrain) ," varaibles.")
```

    ## sTrain- Total observation - 7352 rows and 1 varaibles.

``` r
# load testing data
xTest <- fread("./UCI HAR Dataset/test/X_test.txt")
message( "xTest- Total observation - " , nrow(xTest) ," rows and ", ncol(xTest) ," varaibles.")
```

    ## xTest- Total observation - 2947 rows and 561 varaibles.

``` r
yTest <- fread("./UCI HAR Dataset/test/y_test.txt",col.name="Label")
message( "yTest- Total observation - " , nrow(yTest) ," rows and ", ncol(yTest) ," varaibles.")
```

    ## yTest- Total observation - 2947 rows and 1 varaibles.

``` r
sTest <- fread("./UCI HAR Dataset/test/subject_test.txt",col.names = "Subject")
message( "sTest- Total observation - " , nrow(sTest) ," rows and ", ncol(sTest) ," varaibles.")
```

    ## sTest- Total observation - 2947 rows and 1 varaibles.

Now perform the activities as per assignment. \#\# 1- Merges the training and the test sets to create one data set.

``` r
#merge training and test dataset
xDS <- rbind( xTrain, xTest)
yDS <- rbind( yTrain, yTest)
sDS <- rbind( sTrain, sTest)
```

2- Extracts only the measurements on the mean and standard deviation for each measurement.
------------------------------------------------------------------------------------------

``` r
#grep mean and std features index only
fIndex <- grep("\\-(mean|std)[()]", features[, DESC])

#Merge data
xDS <- xDS[, ..fIndex]
names(xDS)  <- features[fIndex, DESC]

mergedDS <- cbind( sDS, yDS, xDS)
message( "mergedDS- Total observation - " , nrow(mergedDS) ," rows and ", ncol(mergedDS) ," varaibles.")
```

    ## mergedDS- Total observation - 10299 rows and 68 varaibles.

3- Uses descriptive activity names to name the activities in the data set
-------------------------------------------------------------------------

``` r
mergedDS$Label <- Activity[ yDS$Label, DESC]
```

4- Appropriately labels the data set with descriptive variable names.
---------------------------------------------------------------------

``` r
names(mergedDS) <- gsub("Acc", "Accelerometer", names(mergedDS))
names(mergedDS) <- gsub("Gyr", "Gryoscope", names(mergedDS))
names(mergedDS) <- gsub("Mag", "Magnitude", names(mergedDS))
names(mergedDS) <- gsub("BodyBody", "Body", names(mergedDS))
names(mergedDS) <- gsub("^t", "Time", names(mergedDS))
names(mergedDS) <- gsub("^f", "Frequency", names(mergedDS))
names(mergedDS) <- gsub("[()]", "", names(mergedDS))

#dataset variable names
names(mergedDS)
```

    ##  [1] "Subject"                                     
    ##  [2] "Label"                                       
    ##  [3] "TimeBodyAccelerometer-mean-X"                
    ##  [4] "TimeBodyAccelerometer-mean-Y"                
    ##  [5] "TimeBodyAccelerometer-mean-Z"                
    ##  [6] "TimeBodyAccelerometer-std-X"                 
    ##  [7] "TimeBodyAccelerometer-std-Y"                 
    ##  [8] "TimeBodyAccelerometer-std-Z"                 
    ##  [9] "TimeGravityAccelerometer-mean-X"             
    ## [10] "TimeGravityAccelerometer-mean-Y"             
    ## [11] "TimeGravityAccelerometer-mean-Z"             
    ## [12] "TimeGravityAccelerometer-std-X"              
    ## [13] "TimeGravityAccelerometer-std-Y"              
    ## [14] "TimeGravityAccelerometer-std-Z"              
    ## [15] "TimeBodyAccelerometerJerk-mean-X"            
    ## [16] "TimeBodyAccelerometerJerk-mean-Y"            
    ## [17] "TimeBodyAccelerometerJerk-mean-Z"            
    ## [18] "TimeBodyAccelerometerJerk-std-X"             
    ## [19] "TimeBodyAccelerometerJerk-std-Y"             
    ## [20] "TimeBodyAccelerometerJerk-std-Z"             
    ## [21] "TimeBodyGryoscopeo-mean-X"                   
    ## [22] "TimeBodyGryoscopeo-mean-Y"                   
    ## [23] "TimeBodyGryoscopeo-mean-Z"                   
    ## [24] "TimeBodyGryoscopeo-std-X"                    
    ## [25] "TimeBodyGryoscopeo-std-Y"                    
    ## [26] "TimeBodyGryoscopeo-std-Z"                    
    ## [27] "TimeBodyGryoscopeoJerk-mean-X"               
    ## [28] "TimeBodyGryoscopeoJerk-mean-Y"               
    ## [29] "TimeBodyGryoscopeoJerk-mean-Z"               
    ## [30] "TimeBodyGryoscopeoJerk-std-X"                
    ## [31] "TimeBodyGryoscopeoJerk-std-Y"                
    ## [32] "TimeBodyGryoscopeoJerk-std-Z"                
    ## [33] "TimeBodyAccelerometerMagnitude-mean"         
    ## [34] "TimeBodyAccelerometerMagnitude-std"          
    ## [35] "TimeGravityAccelerometerMagnitude-mean"      
    ## [36] "TimeGravityAccelerometerMagnitude-std"       
    ## [37] "TimeBodyAccelerometerJerkMagnitude-mean"     
    ## [38] "TimeBodyAccelerometerJerkMagnitude-std"      
    ## [39] "TimeBodyGryoscopeoMagnitude-mean"            
    ## [40] "TimeBodyGryoscopeoMagnitude-std"             
    ## [41] "TimeBodyGryoscopeoJerkMagnitude-mean"        
    ## [42] "TimeBodyGryoscopeoJerkMagnitude-std"         
    ## [43] "FrequencyBodyAccelerometer-mean-X"           
    ## [44] "FrequencyBodyAccelerometer-mean-Y"           
    ## [45] "FrequencyBodyAccelerometer-mean-Z"           
    ## [46] "FrequencyBodyAccelerometer-std-X"            
    ## [47] "FrequencyBodyAccelerometer-std-Y"            
    ## [48] "FrequencyBodyAccelerometer-std-Z"            
    ## [49] "FrequencyBodyAccelerometerJerk-mean-X"       
    ## [50] "FrequencyBodyAccelerometerJerk-mean-Y"       
    ## [51] "FrequencyBodyAccelerometerJerk-mean-Z"       
    ## [52] "FrequencyBodyAccelerometerJerk-std-X"        
    ## [53] "FrequencyBodyAccelerometerJerk-std-Y"        
    ## [54] "FrequencyBodyAccelerometerJerk-std-Z"        
    ## [55] "FrequencyBodyGryoscopeo-mean-X"              
    ## [56] "FrequencyBodyGryoscopeo-mean-Y"              
    ## [57] "FrequencyBodyGryoscopeo-mean-Z"              
    ## [58] "FrequencyBodyGryoscopeo-std-X"               
    ## [59] "FrequencyBodyGryoscopeo-std-Y"               
    ## [60] "FrequencyBodyGryoscopeo-std-Z"               
    ## [61] "FrequencyBodyAccelerometerMagnitude-mean"    
    ## [62] "FrequencyBodyAccelerometerMagnitude-std"     
    ## [63] "FrequencyBodyAccelerometerJerkMagnitude-mean"
    ## [64] "FrequencyBodyAccelerometerJerkMagnitude-std" 
    ## [65] "FrequencyBodyGryoscopeoMagnitude-mean"       
    ## [66] "FrequencyBodyGryoscopeoMagnitude-std"        
    ## [67] "FrequencyBodyGryoscopeoJerkMagnitude-mean"   
    ## [68] "FrequencyBodyGryoscopeoJerkMagnitude-std"

Final variable name after descriptive name <!--  [1] "Subject"                                                      --> <!--  [2] "Label"                                                        --> <!--  [3] "TimeBodyAccelerometerelerometer-mean-X"                       --> <!--  [4] "TimeBodyAccelerometerelerometer-mean-Y"                       --> <!--  [5] "TimeBodyAccelerometerelerometer-mean-Z"                       --> <!--  [6] "TimeBodyAccelerometerelerometer-std-X"                        --> <!--  [7] "TimeBodyAccelerometerelerometer-std-Y"                        --> <!--  [8] "TimeBodyAccelerometerelerometer-std-Z"                        --> <!--  [9] "TimeGravityAccelerometerelerometer-mean-X"                    --> <!-- [10] "TimeGravityAccelerometerelerometer-mean-Y"                    --> <!-- [11] "TimeGravityAccelerometerelerometer-mean-Z"                    --> <!-- [12] "TimeGravityAccelerometerelerometer-std-X"                     --> <!-- [13] "TimeGravityAccelerometerelerometer-std-Y"                     --> <!-- [14] "TimeGravityAccelerometerelerometer-std-Z"                     --> <!-- [15] "TimeBodyAccelerometerelerometerJerk-mean-X"                   --> <!-- [16] "TimeBodyAccelerometerelerometerJerk-mean-Y"                   --> <!-- [17] "TimeBodyAccelerometerelerometerJerk-mean-Z"                   --> <!-- [18] "TimeBodyAccelerometerelerometerJerk-std-X"                    --> <!-- [19] "TimeBodyAccelerometerelerometerJerk-std-Y"                    --> <!-- [20] "TimeBodyAccelerometerelerometerJerk-std-Z"                    --> <!-- [21] "TimeBodyGryoscopeo-mean-X"                                    --> <!-- [22] "TimeBodyGryoscopeo-mean-Y"                                    --> <!-- [23] "TimeBodyGryoscopeo-mean-Z"                                    --> <!-- [24] "TimeBodyGryoscopeo-std-X"                                     --> <!-- [25] "TimeBodyGryoscopeo-std-Y"                                     --> <!-- [26] "TimeBodyGryoscopeo-std-Z"                                     --> <!-- [27] "TimeBodyGryoscopeoJerk-mean-X"                                --> <!-- [28] "TimeBodyGryoscopeoJerk-mean-Y"                                --> <!-- [29] "TimeBodyGryoscopeoJerk-mean-Z"                                --> <!-- [30] "TimeBodyGryoscopeoJerk-std-X"                                 --> <!-- [31] "TimeBodyGryoscopeoJerk-std-Y"                                 --> <!-- [32] "TimeBodyGryoscopeoJerk-std-Z"                                 --> <!-- [33] "TimeBodyAccelerometerelerometerMagnitudenitude-mean"          --> <!-- [34] "TimeBodyAccelerometerelerometerMagnitudenitude-std"           --> <!-- [35] "TimeGravityAccelerometerelerometerMagnitudenitude-mean"       --> <!-- [36] "TimeGravityAccelerometerelerometerMagnitudenitude-std"        --> <!-- [37] "TimeBodyAccelerometerelerometerJerkMagnitudenitude-mean"      --> <!-- [38] "TimeBodyAccelerometerelerometerJerkMagnitudenitude-std"       --> <!-- [39] "TimeBodyGryoscopeoMagnitudenitude-mean"                       --> <!-- [40] "TimeBodyGryoscopeoMagnitudenitude-std"                        --> <!-- [41] "TimeBodyGryoscopeoJerkMagnitudenitude-mean"                   --> <!-- [42] "TimeBodyGryoscopeoJerkMagnitudenitude-std"                    --> <!-- [43] "FrequencyBodyAccelerometerelerometer-mean-X"                  --> <!-- [44] "FrequencyBodyAccelerometerelerometer-mean-Y"                  --> <!-- [45] "FrequencyBodyAccelerometerelerometer-mean-Z"                  --> <!-- [46] "FrequencyBodyAccelerometerelerometer-std-X"                   --> <!-- [47] "FrequencyBodyAccelerometerelerometer-std-Y"                   --> <!-- [48] "FrequencyBodyAccelerometerelerometer-std-Z"                   --> <!-- [49] "FrequencyBodyAccelerometerelerometerJerk-mean-X"              --> <!-- [50] "FrequencyBodyAccelerometerelerometerJerk-mean-Y"              --> <!-- [51] "FrequencyBodyAccelerometerelerometerJerk-mean-Z"              --> <!-- [52] "FrequencyBodyAccelerometerelerometerJerk-std-X"               --> <!-- [53] "FrequencyBodyAccelerometerelerometerJerk-std-Y"               --> <!-- [54] "FrequencyBodyAccelerometerelerometerJerk-std-Z"               --> <!-- [55] "FrequencyBodyGryoscopeo-mean-X"                               --> <!-- [56] "FrequencyBodyGryoscopeo-mean-Y"                               --> <!-- [57] "FrequencyBodyGryoscopeo-mean-Z"                               --> <!-- [58] "FrequencyBodyGryoscopeo-std-X"                                --> <!-- [59] "FrequencyBodyGryoscopeo-std-Y"                                --> <!-- [60] "FrequencyBodyGryoscopeo-std-Z"                                --> <!-- [61] "FrequencyBodyAccelerometerelerometerMagnitudenitude-mean"     --> <!-- [62] "FrequencyBodyAccelerometerelerometerMagnitudenitude-std"      --> <!-- [63] "FrequencyBodyAccelerometerelerometerJerkMagnitudenitude-mean" --> <!-- [64] "FrequencyBodyAccelerometerelerometerJerkMagnitudenitude-std"  --> <!-- [65] "FrequencyBodyGryoscopeoMagnitudenitude-mean"                  --> <!-- [66] "FrequencyBodyGryoscopeoMagnitudenitude-std"                   --> <!-- [67] "FrequencyBodyGryoscopeoJerkMagnitudenitude-mean"              --> <!-- [68] "FrequencyBodyGryoscopeoJerkMagnitudenitude-std"   -->

5- From the data set in step 4, creates a second, independent tidy data set with the
------------------------------------------------------------------------------------

``` r
#average of each variable for each activity and each subject.

#compute mean for all data points
tidy <- mergedDS %>%
         group_by ( Subject, Label) %>%
         summarise_all( funs( mean))
```

    ## Warning: funs() is soft deprecated as of dplyr 0.8.0
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once per session.

``` r
message( "tidy- Total observation - " , nrow(tidy) ," rows and ", ncol(tidy) ," varaibles.")
```

    ## tidy- Total observation - 180 rows and 68 varaibles.

Create data set as a txt file with write.table() using row.name=FALSE
---------------------------------------------------------------------

``` r
write.table( tidy, "tidyData.txt", row.names = FALSE)
```
