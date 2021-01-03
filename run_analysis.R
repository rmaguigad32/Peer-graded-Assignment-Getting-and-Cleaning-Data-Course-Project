## We will be using the dplr package for this exercise 
library(dplyr)

## Load in the data sets from working directory where files were downloaded
activity_labels <- read.table("~/R Studio Files/Data/UCI HAR Dataset/activity_labels.txt", quote="\"", comment.char="")
features <- read.table("~/R Studio Files/Data/UCI HAR Dataset/features.txt", quote="\"", comment.char="")

y_test <- read.table("~/R Studio Files/Data/UCI HAR Dataset/test/y_test.txt", quote="\"", comment.char="")
X_test <- read.table("~/R Studio Files/Data/UCI HAR Dataset/test/X_test.txt", quote="\"", comment.char="")
subject_test <- read.table("~/R Studio Files/Data/UCI HAR Dataset/test/subject_test.txt", quote="\"", comment.char="")

X_train <- read.table("~/R Studio Files/Data/UCI HAR Dataset/train/X_train.txt", quote="\"", comment.char="")
y_train <- read.table("~/R Studio Files/Data/UCI HAR Dataset/train/y_train.txt", quote="\"", comment.char="")
subject_train <- read.table("~/R Studio Files/Data/UCI HAR Dataset/train/subject_train.txt", quote="\"", comment.char="")

## Create "Test" Data Table
colnames(subject_test) <- "subjectID"
colnames(y_test) <- "activity"
colnames(X_test) <-features[,2]

Merged_Test <- cbind(subject_test,y_test,X_test)

## Create "Train" Data Table
colnames(subject_train) <- "subjectID"
colnames(y_train) <- "activity"
colnames(X_train) <-features[,2]

Merged_Train <- cbind(subject_train,y_train,X_train)

## Merge both Test and Train data sets into one table 
Full_Data_Set <- rbind(Merged_Test,Merged_Train)

## Create filters too extract only mean and standard deviation measurements for
## the subjects and activities
Filters <- grepl(pattern = "subjectID",colnames(Full_Data_Set)) | 
    grepl(pattern = "activity",colnames(Full_Data_Set)) | 
    grepl(pattern = "mean..",colnames(Full_Data_Set)) | 
    grepl(pattern = "std..",colnames(Full_Data_Set))


Filter_Data <- Full_Data_Set[ , Filters]

## Create column to describe activity name
colnames(activity_labels) <- c("activity","activitytype")
Filter_Data <- merge(Filter_Data,activity_labels)
Filter_Data <-Filter_Data %>% relocate(activitytype, .before = subjectID )

## Create a tidy data set with the average of each variable for each activity 
## and each subject from the filtered data set.
Tidy_Set <- aggregate(Filter_Data[,4:82], 
                      by = list(activity = Filter_Data$activitytype, subject = Filter_Data$subjectID), 
                      FUN = mean)

## Create .txt file of the tidy data set
write.table(Tidy_Set, "Tidy_Set.txt", row.names = FALSE)