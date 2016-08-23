library(dplyr)
## fetch data
if(!file.exists("./project")){
        dir.create("./project")
}
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "./project/activity.csv.zip")
list.files("./project")
## unzip then read in data and info files
RawdData <- unzip("./project/activity.csv.zip")

trainingSet <- read.table("./project./UCI HAR Dataset/train/X_train.txt")
testSet <- read.table( "./project./UCI HAR Dataset/test/X_test.txt")

activity_label <- read.table("./project./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./project./UCI HAR Dataset/features.txt") 
features_info <- readLines("./project./UCI HAR Dataset/features_info.txt")
readme <- readLines("./project./UCI HAR Dataset/README.txt")

## check NA value
sum(is.na(trainingSet))
sum(is.na(testSet))

## merge training and test sets
df <- rbind(trainingSet, testSet)

## add features and make names to valid name without duplication
ta = t(features$V2) 
valid_column_names <- make.names(names=ta, unique=TRUE, allow_ = TRUE)
names(df) <- valid_column_names

## Extracts only the measurements on the mean and standard deviation
pos_mean_std <- grep("mean|std", names(df))
mean_std <- select(df, pos_mean_std)

## add one column "activityinfo" to df and name the activity use y_train and y_test info
ytrain <- read.csv("./project./UCI HAR Dataset/train/y_train.txt")
ytest <- read.csv("./project./UCI HAR Dataset/test/y_test.txt")
### copy last row to add one more observation
        sum(is.na(ytrain))
        sum(is.na(ytest))
        temtrain <- ytrain[7351,1]
        temtest <- ytest[2946,1]
        ytrain <- rbind(ytrain, temtrain)
        ytest <- rbind(ytest,temtest)
activityinfo <- rbind(ytrain, ytest)
names(activityinfo) <- c("activityinfo")
mean_std_activityinfo <- cbind(mean_std, activityinfo)

## add one column "subject" to mean_std
subject_train <- read.csv("./project./UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.csv("./project./UCI HAR Dataset/test/subject_test.txt" )
### copy last row to add one more observation
        a <- subject_train[7352,1]
        b <- subject_test[2946,1]
        subject_train <- rbind(subject_train,a)
        subject_test <- rbind(subject_test,b)
        names(subject_test) <- c("X1") 
        
subject <- rbind(subject_test, subject_train)
names(subject) <- c("subject")

## final tidy data set
tidy_df <- cbind(subject, activityinfo, mean_std)

## creat a independent dataset
df2 <- tidy_df %>% group_by(subject, activityinfo) %>% summarise_each(funs(mean))

## data output
write.csv(tidy_df, "./tidydata.csv")
write.csv(df2, "./average.csv")
## end of script
