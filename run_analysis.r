#Preparation
    library(dtplyr)
    library(data.table)
    setwd("./UCI HAR Dataset/")
#Test - raw
    DTwear <- fread("./test/X_test.txt", header=FALSE, colClasses="numeric")
    DTwear$set <- "Test"
    subject <- read.csv("./test/subject_test.txt", header=FALSE)
    DTwear$subject <- subject
    activity <- read.csv("./test/y_test.txt", header=FALSE)
    DTwear$activity <- activity
#Train - raw
    DTtrain <- fread("./train/X_train.txt", header=FALSE, colClasses="numeric")
    DTtrain$set <- "Train"
    subject <- read.csv("./train/subject_train.txt", header=FALSE)
    DTtrain$subject <- subject
    activity <- read.csv("./train/y_train.txt", header=FALSE)
    DTtrain$activity <- activity
#Merges the training and the test sets to create one data set.    
    DTwear <- rbind(DTwear,DTtrain)
#Appropriately labels the data set with descriptive variable names.
    Features <- read.csv("features.txt", sep=" ", header=FALSE)
    colnames(DTwear)[1:561] <- as.character(paste0(c(1:561),rep("_"),Features$V2))
    setcolorder(DTwear, c("set","subject","activity",colnames(DTwear)[1:561]))
#Uses descriptive activity names to name the activities in the data set
    act_desc <- read.csv("activity_labels.txt", header=FALSE, sep=" ")
    DTwear$activity <- as.factor(DTwear$activity)
    levels(DTwear$activity) <- act_desc$V2
#Extracts only the measurements on the mean and standard deviation for each measurement.
    mean_sd_cols <- c(1:3,grep(".*std\\(\\)|mean\\(\\).*",colnames(DTwear)))
    DTwear <- DTwear[,mean_sd_cols,with=FALSE]
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.    
    DTtidy <- melt(DTwear,id=c("set","subject","activity"),measure.vars=c(colnames(DTwear)[-(1:3)]))
    DTtidy <- dcast(DTtidy, set + subject + activity ~ variable, mean)
    colnames(DTtidy)[-(1:3)] <- as.character(paste0("avg_",colnames(DTtidy)[-(1:3)]))
    plot(DTtidy[,10:12])
    write.table(DTtidy,"WearTidy.txt",row.names=FALSE)