## change language of error messages to English
Sys.setenv(LANG="en")  

# set the working directory for the download
setwd("~/Desktop")  

# check if the directory data exists if not create it
if(!file.exists("./data")) {    
        dir.create("./data")  
}

# change working directory and load the files
setwd("~/Desktop/data")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" # copied filelink from  the webpage
download.file(fileUrl, destfile = "accelerometers.zip", method = "curl")
list.files() # check the download

# unzip files
unzip("accelerometers.zip") 
list.files() 

# check the data
setwd("~/Desktop/data/UCI HAR Dataset")
list.files()

# set working directory
setwd("~/Desktop/data/UCI HAR Dataset")

# load package dplyr for later data manipulation
require(dplyr)

# check directory
dir()

# load the different datasets
ytest <- read.table("~/Desktop/data/UCI HAR Dataset/test/y_test.txt", quote="\"")
str(ytest)

xtest <- read.table("~/Desktop/data/UCI HAR Dataset/test/X_test.txt", quote="\"")
str(xtest)

subjecttest <- read.table("~/Desktop/data/UCI HAR Dataset/test/subject_test.txt", quote="\"")

ytrain <- read.table("~/Desktop/data/UCI HAR Dataset/train/y_train.txt", quote="\"")

xtrain <- read.table("~/Desktop/data/UCI HAR Dataset/train/X_train.txt", quote="\"")

subjecttrain <- read.table("~/Desktop/data/UCI HAR Dataset/train/subject_train.txt", quote="\"")

activitylabels <- read.table("~/Desktop/data/UCI HAR Dataset/activity_labels.txt", quote="\"")

features <- read.table("~/Desktop/data/UCI HAR Dataset/features.txt", quote="\"")

# give variable appropriate names from the feature.txt
names(xtest) <- (features[,2])
names(xtrain) <- (features[,2])

# transform the positions into a factor and give apropriate labels to the actvities
ytrain <- factor(ytrain[,1], labels=as.character(activitylabels[,2]))
ytest <- factor(ytest[,1], labels=as.character(activitylabels[,2]))

names(ytrain) <- "activity"
names(ytest) <- "activity"

# give appropriate name
names(subjecttest) <- "volunteerid"
names(subjecttrain) <- "volunteerid"

# set the two datasets test and train by combining the columns
test <- cbind(volunteerid = subjecttest, activity = ytest, xtest)
train <- cbind(volunteerid = subjecttrain, activity = ytrain, xtrain)

# merge them together by rows to one dataset
humanactivitydata <- rbind(test, train)
humanactivitydata <- tbl_df(humanactivitydata)

# convert voluneerid into factor
humanactivitydata$volunteerid <- as.factor(humanactivitydata$volunteerid)

# order them by volunteerid from 1:30 and by activity
humanactivitydata <- humanactivitydata[order(humanactivitydata$volunteerid, humanactivitydata$activity),]
tail(humanactivitydata[order(humanactivitydata$volunteerid, humanactivitydata$activity),])
head(humanactivitydata[order(humanactivitydata$volunteerid, humanactivitydata$activity),])

# to make select from the dplyr package work for strings, I needed to modify the variable names otherwise it will throw an error because it can't distinguish long variable names (see https://class.coursera.org/getdata-011/forum/thread?thread_id=105)
number <- 1:9
names(humanactivitydata) <- paste0(number, names(humanactivitydata))
names(humanactivitydata) <- paste0(LETTERS, names(humanactivitydata))

# I selected only mean() and std() because these are the mean and standardeviation according the features.info.txt
new <- select(humanactivitydata, A1volunteerid, B2activity, contains("mean()"), contains("std()"))

# removing the first two characters to get the clean names again
names(new) <- substr(names(new), 3, 100)

# cutting the clutter like - and ()
names(new) <- gsub("-", "", names(new))
names(new) <- gsub("\\)", "", names(new))
names(new) <- gsub("\\(", "", names(new))

# grouping by volunteerid and activity
new2 <- new %>% group_by(volunteerid, activity)

# take the average for all measures per volunteer and activity, I added a new column which shows the number of observation in in each row
new2 <- new2 %>% summarise(numberofobservations = n(), tBodyAccmeanX = mean(tBodyAccmeanX), tBodyAccmeanY = mean(tBodyAccmeanY), tBodyAccmeanZ = mean(tBodyAccmeanZ), tGravityAccmeanX = mean(tGravityAccmeanX), tGravityAccmeanY = mean(tGravityAccmeanY),tGravityAccmeanZ = mean(tGravityAccmeanZ), tBodyAccJerkmeanX = mean(tBodyAccJerkmeanX), tBodyAccJerkmeanY = mean(tBodyAccJerkmeanY), tBodyAccJerkmeanZ = mean(tBodyAccJerkmeanZ), tBodyGyromeanX = mean(tBodyGyromeanX), tBodyGyromeanY = mean(tBodyGyromeanY), tBodyGyromeanZ = mean(tBodyGyromeanZ), tBodyGyroJerkmeanX = mean(tBodyGyroJerkmeanX),       
                           tBodyGyroJerkmeanY = mean(tBodyGyroJerkmeanY), tBodyGyroJerkmeanZ = mean(tBodyGyroJerkmeanZ), tBodyAccMagmean = mean(tBodyAccMagmean), tGravityAccMagmean = mean(tGravityAccMagmean), tBodyAccJerkMagmean = mean(tBodyAccJerkMagmean), tBodyGyroMagmean = mean(tBodyGyroMagmean), tBodyGyroJerkMagmean = mean(tBodyGyroJerkMagmean), fBodyAccmeanX = mean(fBodyAccmeanX), fBodyAccmeanY = mean(fBodyAccmeanY), fBodyAccmeanZ = mean(fBodyAccmeanZ), fBodyAccJerkmeanX = mean(fBodyAccJerkmeanX), fBodyAccJerkmeanY = mean(fBodyAccJerkmeanY), fBodyAccJerkmeanZ = mean(fBodyAccJerkmeanZ),
                           fBodyGyromeanX = mean(fBodyGyromeanX), fBodyGyromeanY = mean(fBodyGyromeanY), fBodyGyromeanZ = mean(fBodyGyromeanZ), fBodyAccMagmean = mean(fBodyAccMagmean), fBodyBodyAccJerkMagmea = mean(fBodyBodyAccJerkMagmean), fBodyBodyGyroMagmean = mean(fBodyBodyGyroMagmean), fBodyBodyGyroJerkMagmean = mean(fBodyBodyGyroJerkMagmean), tBodyAccstdX = mean(tBodyAccstdX), tBodyAccstdY = mean(tBodyAccstdY), tBodyAccstdZ = mean(tBodyAccstdZ), tGravityAccstdX = mean(tGravityAccstdX), tGravityAccstdY = mean(tGravityAccstdY), tGravityAccstdZ = mean(tGravityAccstdZ), tBodyAccJerkstdX = mean(tBodyAccJerkstdX),
                           tBodyAccJerkstdY = mean(tBodyAccJerkstdY), tBodyAccJerkstdZ = mean(tBodyAccJerkstdZ), tBodyGyrostdX = mean(tBodyGyrostdX), tBodyGyrostdY = mean(tBodyGyrostdY), tBodyGyrostdZ = mean(tBodyGyrostdZ), tBodyGyroJerkstdX = mean(tBodyGyroJerkstdX), tBodyGyroJerkstdY = mean(tBodyGyroJerkstdY), tBodyGyroJerkstdZ = mean(tBodyGyroJerkstdZ), tBodyAccMagstd = mean(tBodyAccMagstd), tGravityAccMagstd = mean(tGravityAccMagstd), tBodyAccJerkMagstd = mean(tBodyAccJerkMagstd), tBodyGyroMagstd = mean(tBodyGyroMagstd), tBodyGyroJerkMagstd = mean(tBodyGyroJerkMagstd), fBodyAccstdX = mean(fBodyAccstdX),
                           fBodyAccstdY = mean(fBodyAccstdY), fBodyAccstdZ = mean(fBodyAccstdZ), fBodyAccJerkstdX = mean(fBodyAccJerkstdX), fBodyAccJerkstdY = mean(fBodyAccJerkstdY), fBodyAccJerkstdZ = mean(fBodyAccJerkstdZ), fBodyGyrostdX = mean(fBodyGyrostdX), fBodyGyrostdY = mean(fBodyGyrostdY), fBodyGyrostdZ = mean(fBodyGyrostdZ), fBodyAccMagstd = mean(fBodyAccMagstd), fBodyBodyAccJerkMagstd = mean(fBodyBodyAccJerkMagstd), fBodyBodyGyroMagstd = mean(fBodyBodyGyroMagstd), fBodyBodyGyroJerkMagstd = mean(fBodyBodyGyroJerkMagstd))

# remove all upper case letter
new2$activity <- tolower(new2$activity)
names(new2) <- tolower(names(new2))
# remove _ from activity labels
new2$activity <- gsub("_", "", new2$activity)

# changing working directory and wrteing a text file protect it with # to not overwrite
#setwd("~/Desktop/GettingCleaningData")
#write.table(new2, "mydata.txt", row.name=FALSE)