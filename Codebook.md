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
head(Activity,6)
```

    ##    CODE               DESC
    ## 1:    1            WALKING
    ## 2:    2   WALKING_UPSTAIRS
    ## 3:    3 WALKING_DOWNSTAIRS
    ## 4:    4            SITTING
    ## 5:    5           STANDING
    ## 6:    6             LAYING

``` r
#load features
features <- fread("./UCI HAR Dataset/features.txt",  col.names = c("CODE", "DESC"))
message( "features- Total observation - " , nrow(features) ," rows and ", ncol(features) ," varaibles.")
```

    ## features- Total observation - 561 rows and 2 varaibles.

``` r
head( features)
```

    ##    CODE              DESC
    ## 1:    1 tBodyAcc-mean()-X
    ## 2:    2 tBodyAcc-mean()-Y
    ## 3:    3 tBodyAcc-mean()-Z
    ## 4:    4  tBodyAcc-std()-X
    ## 5:    5  tBodyAcc-std()-Y
    ## 6:    6  tBodyAcc-std()-Z

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

``` r
head(mergedDS ,2)
```

    ##    Subject Label tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z
    ## 1:       1     5         0.2885845       -0.02029417        -0.1329051
    ## 2:       1     5         0.2784188       -0.01641057        -0.1235202
    ##    tBodyAcc-std()-X tBodyAcc-std()-Y tBodyAcc-std()-Z tGravityAcc-mean()-X
    ## 1:       -0.9952786       -0.9831106       -0.9135264            0.9633961
    ## 2:       -0.9982453       -0.9753002       -0.9603220            0.9665611
    ##    tGravityAcc-mean()-Y tGravityAcc-mean()-Z tGravityAcc-std()-X
    ## 1:           -0.1408397            0.1153749          -0.9852497
    ## 2:           -0.1415513            0.1093788          -0.9974113
    ##    tGravityAcc-std()-Y tGravityAcc-std()-Z tBodyAccJerk-mean()-X
    ## 1:          -0.9817084          -0.8776250            0.07799634
    ## 2:          -0.9894474          -0.9316387            0.07400671
    ##    tBodyAccJerk-mean()-Y tBodyAccJerk-mean()-Z tBodyAccJerk-std()-X
    ## 1:           0.005000803           -0.06783081           -0.9935191
    ## 2:           0.005771104            0.02937663           -0.9955481
    ##    tBodyAccJerk-std()-Y tBodyAccJerk-std()-Z tBodyGyro-mean()-X
    ## 1:           -0.9883600           -0.9935750       -0.006100849
    ## 2:           -0.9810636           -0.9918457       -0.016111620
    ##    tBodyGyro-mean()-Y tBodyGyro-mean()-Z tBodyGyro-std()-X
    ## 1:        -0.03136479          0.1077254        -0.9853103
    ## 2:        -0.08389378          0.1005843        -0.9831200
    ##    tBodyGyro-std()-Y tBodyGyro-std()-Z tBodyGyroJerk-mean()-X
    ## 1:        -0.9766234        -0.9922053             -0.0991674
    ## 2:        -0.9890458        -0.9891212             -0.1105028
    ##    tBodyGyroJerk-mean()-Y tBodyGyroJerk-mean()-Z tBodyGyroJerk-std()-X
    ## 1:            -0.05551737            -0.06198580            -0.9921107
    ## 2:            -0.04481873            -0.05924282            -0.9898726
    ##    tBodyGyroJerk-std()-Y tBodyGyroJerk-std()-Z tBodyAccMag-mean()
    ## 1:            -0.9925193            -0.9920553         -0.9594339
    ## 2:            -0.9972926            -0.9938510         -0.9792892
    ##    tBodyAccMag-std() tGravityAccMag-mean() tGravityAccMag-std()
    ## 1:        -0.9505515            -0.9594339           -0.9505515
    ## 2:        -0.9760571            -0.9792892           -0.9760571
    ##    tBodyAccJerkMag-mean() tBodyAccJerkMag-std() tBodyGyroMag-mean()
    ## 1:             -0.9933059            -0.9943364          -0.9689591
    ## 2:             -0.9912535            -0.9916944          -0.9806831
    ##    tBodyGyroMag-std() tBodyGyroJerkMag-mean() tBodyGyroJerkMag-std()
    ## 1:         -0.9643352              -0.9942478             -0.9913676
    ## 2:         -0.9837542              -0.9951232             -0.9961016
    ##    fBodyAcc-mean()-X fBodyAcc-mean()-Y fBodyAcc-mean()-Z fBodyAcc-std()-X
    ## 1:        -0.9947832        -0.9829841        -0.9392687       -0.9954217
    ## 2:        -0.9974507        -0.9768517        -0.9735227       -0.9986803
    ##    fBodyAcc-std()-Y fBodyAcc-std()-Z fBodyAccJerk-mean()-X
    ## 1:       -0.9831330       -0.9061650            -0.9923325
    ## 2:       -0.9749298       -0.9554381            -0.9950322
    ##    fBodyAccJerk-mean()-Y fBodyAccJerk-mean()-Z fBodyAccJerk-std()-X
    ## 1:            -0.9871699            -0.9896961           -0.9958207
    ## 2:            -0.9813115            -0.9897398           -0.9966523
    ##    fBodyAccJerk-std()-Y fBodyAccJerk-std()-Z fBodyGyro-mean()-X
    ## 1:           -0.9909363           -0.9970517         -0.9865744
    ## 2:           -0.9820839           -0.9926268         -0.9773867
    ##    fBodyGyro-mean()-Y fBodyGyro-mean()-Z fBodyGyro-std()-X
    ## 1:         -0.9817615         -0.9895148        -0.9850326
    ## 2:         -0.9925300         -0.9896058        -0.9849043
    ##    fBodyGyro-std()-Y fBodyGyro-std()-Z fBodyAccMag-mean()
    ## 1:        -0.9738861        -0.9940349         -0.9521547
    ## 2:        -0.9871681        -0.9897847         -0.9808566
    ##    fBodyAccMag-std() fBodyBodyAccJerkMag-mean() fBodyBodyAccJerkMag-std()
    ## 1:        -0.9561340                 -0.9937257                -0.9937550
    ## 2:        -0.9758658                 -0.9903355                -0.9919603
    ##    fBodyBodyGyroMag-mean() fBodyBodyGyroMag-std()
    ## 1:              -0.9801349             -0.9613094
    ## 2:              -0.9882956             -0.9833219
    ##    fBodyBodyGyroJerkMag-mean() fBodyBodyGyroJerkMag-std()
    ## 1:                  -0.9919904                 -0.9906975
    ## 2:                  -0.9958539                 -0.9963995

3- Uses descriptive activity names to name the activities in the data set
-------------------------------------------------------------------------

``` r
mergedDS$Label <- Activity[ yDS$Label, DESC]
head(mergedDS$Label,6 )
```

    ## [1] "STANDING" "STANDING" "STANDING" "STANDING" "STANDING" "STANDING"

4- Appropriately labels the data set with descriptive variable names.
---------------------------------------------------------------------

``` r
#dataset features prior to transformation
names(mergedDS)
```

    ##  [1] "Subject"                     "Label"                      
    ##  [3] "tBodyAcc-mean()-X"           "tBodyAcc-mean()-Y"          
    ##  [5] "tBodyAcc-mean()-Z"           "tBodyAcc-std()-X"           
    ##  [7] "tBodyAcc-std()-Y"            "tBodyAcc-std()-Z"           
    ##  [9] "tGravityAcc-mean()-X"        "tGravityAcc-mean()-Y"       
    ## [11] "tGravityAcc-mean()-Z"        "tGravityAcc-std()-X"        
    ## [13] "tGravityAcc-std()-Y"         "tGravityAcc-std()-Z"        
    ## [15] "tBodyAccJerk-mean()-X"       "tBodyAccJerk-mean()-Y"      
    ## [17] "tBodyAccJerk-mean()-Z"       "tBodyAccJerk-std()-X"       
    ## [19] "tBodyAccJerk-std()-Y"        "tBodyAccJerk-std()-Z"       
    ## [21] "tBodyGyro-mean()-X"          "tBodyGyro-mean()-Y"         
    ## [23] "tBodyGyro-mean()-Z"          "tBodyGyro-std()-X"          
    ## [25] "tBodyGyro-std()-Y"           "tBodyGyro-std()-Z"          
    ## [27] "tBodyGyroJerk-mean()-X"      "tBodyGyroJerk-mean()-Y"     
    ## [29] "tBodyGyroJerk-mean()-Z"      "tBodyGyroJerk-std()-X"      
    ## [31] "tBodyGyroJerk-std()-Y"       "tBodyGyroJerk-std()-Z"      
    ## [33] "tBodyAccMag-mean()"          "tBodyAccMag-std()"          
    ## [35] "tGravityAccMag-mean()"       "tGravityAccMag-std()"       
    ## [37] "tBodyAccJerkMag-mean()"      "tBodyAccJerkMag-std()"      
    ## [39] "tBodyGyroMag-mean()"         "tBodyGyroMag-std()"         
    ## [41] "tBodyGyroJerkMag-mean()"     "tBodyGyroJerkMag-std()"     
    ## [43] "fBodyAcc-mean()-X"           "fBodyAcc-mean()-Y"          
    ## [45] "fBodyAcc-mean()-Z"           "fBodyAcc-std()-X"           
    ## [47] "fBodyAcc-std()-Y"            "fBodyAcc-std()-Z"           
    ## [49] "fBodyAccJerk-mean()-X"       "fBodyAccJerk-mean()-Y"      
    ## [51] "fBodyAccJerk-mean()-Z"       "fBodyAccJerk-std()-X"       
    ## [53] "fBodyAccJerk-std()-Y"        "fBodyAccJerk-std()-Z"       
    ## [55] "fBodyGyro-mean()-X"          "fBodyGyro-mean()-Y"         
    ## [57] "fBodyGyro-mean()-Z"          "fBodyGyro-std()-X"          
    ## [59] "fBodyGyro-std()-Y"           "fBodyGyro-std()-Z"          
    ## [61] "fBodyAccMag-mean()"          "fBodyAccMag-std()"          
    ## [63] "fBodyBodyAccJerkMag-mean()"  "fBodyBodyAccJerkMag-std()"  
    ## [65] "fBodyBodyGyroMag-mean()"     "fBodyBodyGyroMag-std()"     
    ## [67] "fBodyBodyGyroJerkMag-mean()" "fBodyBodyGyroJerkMag-std()"

``` r
names(mergedDS) <- gsub("Acc", "Accelerometer", names(mergedDS))
names(mergedDS) <- gsub("Gyr", "Gryoscope", names(mergedDS))
names(mergedDS) <- gsub("Mag", "Magnitude", names(mergedDS))
names(mergedDS) <- gsub("BodyBody", "Body", names(mergedDS))
names(mergedDS) <- gsub("^t", "Time", names(mergedDS))
names(mergedDS) <- gsub("^f", "Frequency", names(mergedDS))
names(mergedDS) <- gsub("[()]", "", names(mergedDS))

#dataset variable names after trabsformation
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

tidy <- mergedDS %>%
         group_by ( Subject, Label) %>%
         summarise_all( list( mean = mean))

message( "tidy- Total observation - " , nrow(tidy) ," rows and ", ncol(tidy) ," varaibles.")
```

    ## tidy- Total observation - 180 rows and 68 varaibles.

``` r
str( tidy)
```

    ## Classes 'grouped_df', 'tbl_df', 'tbl' and 'data.frame':  180 obs. of  68 variables:
    ##  $ Subject                                          : int  1 1 1 1 1 1 2 2 2 2 ...
    ##  $ Label                                            : chr  "LAYING" "SITTING" "STANDING" "WALKING" ...
    ##  $ TimeBodyAccelerometer-mean-X_mean                : num  0.222 0.261 0.279 0.277 0.289 ...
    ##  $ TimeBodyAccelerometer-mean-Y_mean                : num  -0.04051 -0.00131 -0.01614 -0.01738 -0.00992 ...
    ##  $ TimeBodyAccelerometer-mean-Z_mean                : num  -0.113 -0.105 -0.111 -0.111 -0.108 ...
    ##  $ TimeBodyAccelerometer-std-X_mean                 : num  -0.928 -0.977 -0.996 -0.284 0.03 ...
    ##  $ TimeBodyAccelerometer-std-Y_mean                 : num  -0.8368 -0.9226 -0.9732 0.1145 -0.0319 ...
    ##  $ TimeBodyAccelerometer-std-Z_mean                 : num  -0.826 -0.94 -0.98 -0.26 -0.23 ...
    ##  $ TimeGravityAccelerometer-mean-X_mean             : num  -0.249 0.832 0.943 0.935 0.932 ...
    ##  $ TimeGravityAccelerometer-mean-Y_mean             : num  0.706 0.204 -0.273 -0.282 -0.267 ...
    ##  $ TimeGravityAccelerometer-mean-Z_mean             : num  0.4458 0.332 0.0135 -0.0681 -0.0621 ...
    ##  $ TimeGravityAccelerometer-std-X_mean              : num  -0.897 -0.968 -0.994 -0.977 -0.951 ...
    ##  $ TimeGravityAccelerometer-std-Y_mean              : num  -0.908 -0.936 -0.981 -0.971 -0.937 ...
    ##  $ TimeGravityAccelerometer-std-Z_mean              : num  -0.852 -0.949 -0.976 -0.948 -0.896 ...
    ##  $ TimeBodyAccelerometerJerk-mean-X_mean            : num  0.0811 0.0775 0.0754 0.074 0.0542 ...
    ##  $ TimeBodyAccelerometerJerk-mean-Y_mean            : num  0.003838 -0.000619 0.007976 0.028272 0.02965 ...
    ##  $ TimeBodyAccelerometerJerk-mean-Z_mean            : num  0.01083 -0.00337 -0.00369 -0.00417 -0.01097 ...
    ##  $ TimeBodyAccelerometerJerk-std-X_mean             : num  -0.9585 -0.9864 -0.9946 -0.1136 -0.0123 ...
    ##  $ TimeBodyAccelerometerJerk-std-Y_mean             : num  -0.924 -0.981 -0.986 0.067 -0.102 ...
    ##  $ TimeBodyAccelerometerJerk-std-Z_mean             : num  -0.955 -0.988 -0.992 -0.503 -0.346 ...
    ##  $ TimeBodyGryoscopeo-mean-X_mean                   : num  -0.0166 -0.0454 -0.024 -0.0418 -0.0351 ...
    ##  $ TimeBodyGryoscopeo-mean-Y_mean                   : num  -0.0645 -0.0919 -0.0594 -0.0695 -0.0909 ...
    ##  $ TimeBodyGryoscopeo-mean-Z_mean                   : num  0.1487 0.0629 0.0748 0.0849 0.0901 ...
    ##  $ TimeBodyGryoscopeo-std-X_mean                    : num  -0.874 -0.977 -0.987 -0.474 -0.458 ...
    ##  $ TimeBodyGryoscopeo-std-Y_mean                    : num  -0.9511 -0.9665 -0.9877 -0.0546 -0.1263 ...
    ##  $ TimeBodyGryoscopeo-std-Z_mean                    : num  -0.908 -0.941 -0.981 -0.344 -0.125 ...
    ##  $ TimeBodyGryoscopeoJerk-mean-X_mean               : num  -0.1073 -0.0937 -0.0996 -0.09 -0.074 ...
    ##  $ TimeBodyGryoscopeoJerk-mean-Y_mean               : num  -0.0415 -0.0402 -0.0441 -0.0398 -0.044 ...
    ##  $ TimeBodyGryoscopeoJerk-mean-Z_mean               : num  -0.0741 -0.0467 -0.049 -0.0461 -0.027 ...
    ##  $ TimeBodyGryoscopeoJerk-std-X_mean                : num  -0.919 -0.992 -0.993 -0.207 -0.487 ...
    ##  $ TimeBodyGryoscopeoJerk-std-Y_mean                : num  -0.968 -0.99 -0.995 -0.304 -0.239 ...
    ##  $ TimeBodyGryoscopeoJerk-std-Z_mean                : num  -0.958 -0.988 -0.992 -0.404 -0.269 ...
    ##  $ TimeBodyAccelerometerMagnitude-mean_mean         : num  -0.8419 -0.9485 -0.9843 -0.137 0.0272 ...
    ##  $ TimeBodyAccelerometerMagnitude-std_mean          : num  -0.7951 -0.9271 -0.9819 -0.2197 0.0199 ...
    ##  $ TimeGravityAccelerometerMagnitude-mean_mean      : num  -0.8419 -0.9485 -0.9843 -0.137 0.0272 ...
    ##  $ TimeGravityAccelerometerMagnitude-std_mean       : num  -0.7951 -0.9271 -0.9819 -0.2197 0.0199 ...
    ##  $ TimeBodyAccelerometerJerkMagnitude-mean_mean     : num  -0.9544 -0.9874 -0.9924 -0.1414 -0.0894 ...
    ##  $ TimeBodyAccelerometerJerkMagnitude-std_mean      : num  -0.9282 -0.9841 -0.9931 -0.0745 -0.0258 ...
    ##  $ TimeBodyGryoscopeoMagnitude-mean_mean            : num  -0.8748 -0.9309 -0.9765 -0.161 -0.0757 ...
    ##  $ TimeBodyGryoscopeoMagnitude-std_mean             : num  -0.819 -0.935 -0.979 -0.187 -0.226 ...
    ##  $ TimeBodyGryoscopeoJerkMagnitude-mean_mean        : num  -0.963 -0.992 -0.995 -0.299 -0.295 ...
    ##  $ TimeBodyGryoscopeoJerkMagnitude-std_mean         : num  -0.936 -0.988 -0.995 -0.325 -0.307 ...
    ##  $ FrequencyBodyAccelerometer-mean-X_mean           : num  -0.9391 -0.9796 -0.9952 -0.2028 0.0382 ...
    ##  $ FrequencyBodyAccelerometer-mean-Y_mean           : num  -0.86707 -0.94408 -0.97707 0.08971 0.00155 ...
    ##  $ FrequencyBodyAccelerometer-mean-Z_mean           : num  -0.883 -0.959 -0.985 -0.332 -0.226 ...
    ##  $ FrequencyBodyAccelerometer-std-X_mean            : num  -0.9244 -0.9764 -0.996 -0.3191 0.0243 ...
    ##  $ FrequencyBodyAccelerometer-std-Y_mean            : num  -0.834 -0.917 -0.972 0.056 -0.113 ...
    ##  $ FrequencyBodyAccelerometer-std-Z_mean            : num  -0.813 -0.934 -0.978 -0.28 -0.298 ...
    ##  $ FrequencyBodyAccelerometerJerk-mean-X_mean       : num  -0.9571 -0.9866 -0.9946 -0.1705 -0.0277 ...
    ##  $ FrequencyBodyAccelerometerJerk-mean-Y_mean       : num  -0.9225 -0.9816 -0.9854 -0.0352 -0.1287 ...
    ##  $ FrequencyBodyAccelerometerJerk-mean-Z_mean       : num  -0.948 -0.986 -0.991 -0.469 -0.288 ...
    ##  $ FrequencyBodyAccelerometerJerk-std-X_mean        : num  -0.9642 -0.9875 -0.9951 -0.1336 -0.0863 ...
    ##  $ FrequencyBodyAccelerometerJerk-std-Y_mean        : num  -0.932 -0.983 -0.987 0.107 -0.135 ...
    ##  $ FrequencyBodyAccelerometerJerk-std-Z_mean        : num  -0.961 -0.988 -0.992 -0.535 -0.402 ...
    ##  $ FrequencyBodyGryoscopeo-mean-X_mean              : num  -0.85 -0.976 -0.986 -0.339 -0.352 ...
    ##  $ FrequencyBodyGryoscopeo-mean-Y_mean              : num  -0.9522 -0.9758 -0.989 -0.1031 -0.0557 ...
    ##  $ FrequencyBodyGryoscopeo-mean-Z_mean              : num  -0.9093 -0.9513 -0.9808 -0.2559 -0.0319 ...
    ##  $ FrequencyBodyGryoscopeo-std-X_mean               : num  -0.882 -0.978 -0.987 -0.517 -0.495 ...
    ##  $ FrequencyBodyGryoscopeo-std-Y_mean               : num  -0.9512 -0.9623 -0.9871 -0.0335 -0.1814 ...
    ##  $ FrequencyBodyGryoscopeo-std-Z_mean               : num  -0.917 -0.944 -0.982 -0.437 -0.238 ...
    ##  $ FrequencyBodyAccelerometerMagnitude-mean_mean    : num  -0.8618 -0.9478 -0.9854 -0.1286 0.0966 ...
    ##  $ FrequencyBodyAccelerometerMagnitude-std_mean     : num  -0.798 -0.928 -0.982 -0.398 -0.187 ...
    ##  $ FrequencyBodyAccelerometerJerkMagnitude-mean_mean: num  -0.9333 -0.9853 -0.9925 -0.0571 0.0262 ...
    ##  $ FrequencyBodyAccelerometerJerkMagnitude-std_mean : num  -0.922 -0.982 -0.993 -0.103 -0.104 ...
    ##  $ FrequencyBodyGryoscopeoMagnitude-mean_mean       : num  -0.862 -0.958 -0.985 -0.199 -0.186 ...
    ##  $ FrequencyBodyGryoscopeoMagnitude-std_mean        : num  -0.824 -0.932 -0.978 -0.321 -0.398 ...
    ##  $ FrequencyBodyGryoscopeoJerkMagnitude-mean_mean   : num  -0.942 -0.99 -0.995 -0.319 -0.282 ...
    ##  $ FrequencyBodyGryoscopeoJerkMagnitude-std_mean    : num  -0.933 -0.987 -0.995 -0.382 -0.392 ...
    ##  - attr(*, ".internal.selfref")=<externalptr> 
    ##  - attr(*, "groups")=Classes 'tbl_df', 'tbl' and 'data.frame':   30 obs. of  2 variables:
    ##   ..$ Subject: int  1 2 3 4 5 6 7 8 9 10 ...
    ##   ..$ .rows  :List of 30
    ##   .. ..$ : int  1 2 3 4 5 6
    ##   .. ..$ : int  7 8 9 10 11 12
    ##   .. ..$ : int  13 14 15 16 17 18
    ##   .. ..$ : int  19 20 21 22 23 24
    ##   .. ..$ : int  25 26 27 28 29 30
    ##   .. ..$ : int  31 32 33 34 35 36
    ##   .. ..$ : int  37 38 39 40 41 42
    ##   .. ..$ : int  43 44 45 46 47 48
    ##   .. ..$ : int  49 50 51 52 53 54
    ##   .. ..$ : int  55 56 57 58 59 60
    ##   .. ..$ : int  61 62 63 64 65 66
    ##   .. ..$ : int  67 68 69 70 71 72
    ##   .. ..$ : int  73 74 75 76 77 78
    ##   .. ..$ : int  79 80 81 82 83 84
    ##   .. ..$ : int  85 86 87 88 89 90
    ##   .. ..$ : int  91 92 93 94 95 96
    ##   .. ..$ : int  97 98 99 100 101 102
    ##   .. ..$ : int  103 104 105 106 107 108
    ##   .. ..$ : int  109 110 111 112 113 114
    ##   .. ..$ : int  115 116 117 118 119 120
    ##   .. ..$ : int  121 122 123 124 125 126
    ##   .. ..$ : int  127 128 129 130 131 132
    ##   .. ..$ : int  133 134 135 136 137 138
    ##   .. ..$ : int  139 140 141 142 143 144
    ##   .. ..$ : int  145 146 147 148 149 150
    ##   .. ..$ : int  151 152 153 154 155 156
    ##   .. ..$ : int  157 158 159 160 161 162
    ##   .. ..$ : int  163 164 165 166 167 168
    ##   .. ..$ : int  169 170 171 172 173 174
    ##   .. ..$ : int  175 176 177 178 179 180
    ##   ..- attr(*, ".drop")= logi TRUE

``` r
head(tidy,5)
```

    ## # A tibble: 5 x 68
    ## # Groups:   Subject [1]
    ##   Subject Label `TimeBodyAccele~ `TimeBodyAccele~ `TimeBodyAccele~
    ##     <int> <chr>            <dbl>            <dbl>            <dbl>
    ## 1       1 LAYI~            0.222         -0.0405            -0.113
    ## 2       1 SITT~            0.261         -0.00131           -0.105
    ## 3       1 STAN~            0.279         -0.0161            -0.111
    ## 4       1 WALK~            0.277         -0.0174            -0.111
    ## 5       1 WALK~            0.289         -0.00992           -0.108
    ## # ... with 63 more variables: `TimeBodyAccelerometer-std-X_mean` <dbl>,
    ## #   `TimeBodyAccelerometer-std-Y_mean` <dbl>,
    ## #   `TimeBodyAccelerometer-std-Z_mean` <dbl>,
    ## #   `TimeGravityAccelerometer-mean-X_mean` <dbl>,
    ## #   `TimeGravityAccelerometer-mean-Y_mean` <dbl>,
    ## #   `TimeGravityAccelerometer-mean-Z_mean` <dbl>,
    ## #   `TimeGravityAccelerometer-std-X_mean` <dbl>,
    ## #   `TimeGravityAccelerometer-std-Y_mean` <dbl>,
    ## #   `TimeGravityAccelerometer-std-Z_mean` <dbl>,
    ## #   `TimeBodyAccelerometerJerk-mean-X_mean` <dbl>,
    ## #   `TimeBodyAccelerometerJerk-mean-Y_mean` <dbl>,
    ## #   `TimeBodyAccelerometerJerk-mean-Z_mean` <dbl>,
    ## #   `TimeBodyAccelerometerJerk-std-X_mean` <dbl>,
    ## #   `TimeBodyAccelerometerJerk-std-Y_mean` <dbl>,
    ## #   `TimeBodyAccelerometerJerk-std-Z_mean` <dbl>,
    ## #   `TimeBodyGryoscopeo-mean-X_mean` <dbl>,
    ## #   `TimeBodyGryoscopeo-mean-Y_mean` <dbl>,
    ## #   `TimeBodyGryoscopeo-mean-Z_mean` <dbl>,
    ## #   `TimeBodyGryoscopeo-std-X_mean` <dbl>,
    ## #   `TimeBodyGryoscopeo-std-Y_mean` <dbl>,
    ## #   `TimeBodyGryoscopeo-std-Z_mean` <dbl>,
    ## #   `TimeBodyGryoscopeoJerk-mean-X_mean` <dbl>,
    ## #   `TimeBodyGryoscopeoJerk-mean-Y_mean` <dbl>,
    ## #   `TimeBodyGryoscopeoJerk-mean-Z_mean` <dbl>,
    ## #   `TimeBodyGryoscopeoJerk-std-X_mean` <dbl>,
    ## #   `TimeBodyGryoscopeoJerk-std-Y_mean` <dbl>,
    ## #   `TimeBodyGryoscopeoJerk-std-Z_mean` <dbl>,
    ## #   `TimeBodyAccelerometerMagnitude-mean_mean` <dbl>,
    ## #   `TimeBodyAccelerometerMagnitude-std_mean` <dbl>,
    ## #   `TimeGravityAccelerometerMagnitude-mean_mean` <dbl>,
    ## #   `TimeGravityAccelerometerMagnitude-std_mean` <dbl>,
    ## #   `TimeBodyAccelerometerJerkMagnitude-mean_mean` <dbl>,
    ## #   `TimeBodyAccelerometerJerkMagnitude-std_mean` <dbl>,
    ## #   `TimeBodyGryoscopeoMagnitude-mean_mean` <dbl>,
    ## #   `TimeBodyGryoscopeoMagnitude-std_mean` <dbl>,
    ## #   `TimeBodyGryoscopeoJerkMagnitude-mean_mean` <dbl>,
    ## #   `TimeBodyGryoscopeoJerkMagnitude-std_mean` <dbl>,
    ## #   `FrequencyBodyAccelerometer-mean-X_mean` <dbl>,
    ## #   `FrequencyBodyAccelerometer-mean-Y_mean` <dbl>,
    ## #   `FrequencyBodyAccelerometer-mean-Z_mean` <dbl>,
    ## #   `FrequencyBodyAccelerometer-std-X_mean` <dbl>,
    ## #   `FrequencyBodyAccelerometer-std-Y_mean` <dbl>,
    ## #   `FrequencyBodyAccelerometer-std-Z_mean` <dbl>,
    ## #   `FrequencyBodyAccelerometerJerk-mean-X_mean` <dbl>,
    ## #   `FrequencyBodyAccelerometerJerk-mean-Y_mean` <dbl>,
    ## #   `FrequencyBodyAccelerometerJerk-mean-Z_mean` <dbl>,
    ## #   `FrequencyBodyAccelerometerJerk-std-X_mean` <dbl>,
    ## #   `FrequencyBodyAccelerometerJerk-std-Y_mean` <dbl>,
    ## #   `FrequencyBodyAccelerometerJerk-std-Z_mean` <dbl>,
    ## #   `FrequencyBodyGryoscopeo-mean-X_mean` <dbl>,
    ## #   `FrequencyBodyGryoscopeo-mean-Y_mean` <dbl>,
    ## #   `FrequencyBodyGryoscopeo-mean-Z_mean` <dbl>,
    ## #   `FrequencyBodyGryoscopeo-std-X_mean` <dbl>,
    ## #   `FrequencyBodyGryoscopeo-std-Y_mean` <dbl>,
    ## #   `FrequencyBodyGryoscopeo-std-Z_mean` <dbl>,
    ## #   `FrequencyBodyAccelerometerMagnitude-mean_mean` <dbl>,
    ## #   `FrequencyBodyAccelerometerMagnitude-std_mean` <dbl>,
    ## #   `FrequencyBodyAccelerometerJerkMagnitude-mean_mean` <dbl>,
    ## #   `FrequencyBodyAccelerometerJerkMagnitude-std_mean` <dbl>,
    ## #   `FrequencyBodyGryoscopeoMagnitude-mean_mean` <dbl>,
    ## #   `FrequencyBodyGryoscopeoMagnitude-std_mean` <dbl>,
    ## #   `FrequencyBodyGryoscopeoJerkMagnitude-mean_mean` <dbl>,
    ## #   `FrequencyBodyGryoscopeoJerkMagnitude-std_mean` <dbl>

Create data set as a txt file with write.table() using row.name=FALSE
---------------------------------------------------------------------

``` r
write.table( tidy, "tidyData.txt", row.names = FALSE)
```
