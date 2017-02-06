
# 1 Merges the training and the test sets to create one data set.
# Read data from test

test_subjet_train <- read.table("UCI_HAR_Dataset/test/subject_test.txt")
names(test_subjet_train) <- "subject"
test_X_train <- read.table("UCI_HAR_Dataset/test/X_test.txt")
names_X_train <- read.table("UCI_HAR_Dataset/features.txt")
names(test_X_train) <- names_X_train[,2]
test_y_train <- read.table("UCI_HAR_Dataset/test/y_test.txt")
names(test_y_train) <- "activity"

test <- cbind(test_subjet_train,test_y_train,test_X_train)

# Read data from train

train_subjet_train <- read.table("UCI_HAR_Dataset/train/subject_train.txt")
names(train_subjet_train) <- "subject"
train_X_train <- read.table("UCI_HAR_Dataset/train/X_train.txt")
names(train_X_train) <- names_X_train[,2]
train_y_train <- read.table("UCI_HAR_Dataset/train/y_train.txt")
names(train_y_train) <- "activity"

train <- cbind(train_subjet_train,train_y_train,train_X_train)

mydata <- rbind(test,train)

# 2 Extracts only the measurements on the mean and standard deviation for each measurement.

mydata_mean_sd <- mydata[,c(1:2,grep("mean\\(\\)|st\\(\\)", names(mydata)))]

# 3 Uses descriptive activity names to name the activities in the data set

descriptive_act <- read.table("UCI_HAR_Dataset/activity_labels.txt")

mydata_mean_sd$activity <- factor(mydata_mean_sd$activity, labels = as.character(descriptive_act$V2))

# 4 Appropriately labels the data set with descriptive variable names.

names(mydata_mean_sd) <- gsub("^t", "time", names(mydata_mean_sd))
names(mydata_mean_sd) <- gsub("^f", "frequency", names(mydata_mean_sd))
names(mydata_mean_sd) <- gsub("^Acc", "Accelerometer", names(mydata_mean_sd))
names(mydata_mean_sd) <- gsub("Gyro", "Gyroscope", names(mydata_mean_sd))
names(mydata_mean_sd) <- gsub("^Mag", "Magnitude", names(mydata_mean_sd))
names(mydata_mean_sd) <- gsub("^BodyBody", "Body", names(mydata_mean_sd))

# 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidy <- aggregate(.~ subject +  activity, mydata_mean_sd,  mean)
tidy <- tidy[order(tidy$subject,tidy$activity),]

write.table(tidy, "tidy.csv", row.names = FALSE)



