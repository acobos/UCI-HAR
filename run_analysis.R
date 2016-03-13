# reading features and selecting those containing mean or std
features <- read.table("./UCI HAR Dataset/features.txt", col.names = c("code","name"), stringsAsFactors = FALSE)
ms <- grep('(mean|std)[^Freq]',features$name)

# pick and prettify names for the selected features 
ms_names <- gsub("()","",gsub("-","_",features$name[ms]),fixed=TRUE)

# Reading training data ----

# reading and retaining only those columns corresponding to features in ms
x <- read.table("./UCI HAR Dataset/train/X_train.txt")[,ms]

# giving names to retained features (in ms)
names(x) <- ms_names

# merging activity codes and descriptors
a <- merge(read.table("./UCI HAR Dataset/train/y_train.txt", col.names = "code"),
           read.table("./UCI HAR Dataset/activity_labels.txt", col.names = c("code","activity"), stringsAsFactors = FALSE),
           all.x=TRUE)

# creating dataframe with subject ids, activity descriptors, and retained features
train <- data.frame(read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = "subject"), 
                    activity=a$activity, x)

# cleaning up
rm(a,x)

# Reading test data ----

# reading and retaining only those columns corresponding to features in ms
x <- read.table("./UCI HAR Dataset/test/X_test.txt")[,ms]

# giving names to retained features (in ms)
names(x) <- ms_names

# merging activity codes and descriptors
a <- merge(read.table("./UCI HAR Dataset/test/y_test.txt", col.names = "code"),
           read.table("./UCI HAR Dataset/activity_labels.txt", col.names = c("code","activity"), stringsAsFactors = FALSE),
           all.x=TRUE)

# creating dataframe with subject ids, activity descriptors, and retained features
test <- data.frame(read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "subject"), 
                    activity=a$activity, x)

# combining train and test dataframes (and reordering vars) ----
test$set <- "test"
train$set <- "training"
tidyData <- rbind(train,test)
tidyData <- tidyData[,c(length(tidyData),1:length(tidyData)-1)]

# cleaning up
rm(a,x, features, ms, ms_names, test, train)

# creates a second, independent tidy data set, with the average of each variable 
# for each activity and each subject ----
tidyData_means <- aggregate(x = tidyData[,4:69],
                            by = list(set=tidyData$set, subject=tidyData$subject,activity=tidyData$activity),
                            FUN = mean)
# sorting both dataframes
require(dplyr)
tidyData <- arrange(tidyData,set,subject,activity)
tidyData_means <- arrange(tidyData_means,set,subject,activity)
