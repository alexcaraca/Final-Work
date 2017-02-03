startProcedure <- function () {

  # Cleaning variables 
    if (file.exists("FinalDelivery.txt"))
       file.remove("FinalDelivery.txt")
    XTrain <- NULL
    XTest <- NULL
    Filenames <- NULL
    YTrain <- NULL
    YTest <- NULL
    Filenames2 <- NULL

    # Read the input files 
    XTrain <- read.table ("train/X_train.txt")
    XTest <- read.table ("test/X_test.txt")
    FileNames <- read.table ("features.txt") [, 2]
    YTrain <- read.table ("train/Y_train.txt")
    YTest <- read.table ("test/Y_test.txt")
    FileNames2 <- read.table ("activity_labels.txt") [, 2]
    SubTrain <- read.table ("train/subject_train.txt")
    SubTest <- read.table ("test/subject_test.txt")

    # Merge of datasets 
    MergedDataX <- merge.data.frame(XTrain,XTest, all = TRUE)
    MergedDataY <- rbind(YTrain,YTest) [,1]
    MergedSub <- rbind(SubTrain,SubTest) [,1]

    # Creating names for the MergeData
    names(MergedDataX) <- FileNames

    # Extract only mean () and standard deviation () for  measurement
    MeanStd <- grep("(mean|std)\\(\\)", names(MergedDataX),value =TRUE)
    NewMergedDataX <- MergedDataX [,MeanStd]

    # Put nicer names on activities
    FileNames2 <- gsub("_"," ",FileNames2)
    MergeDataY <- FileNames2[MergedDataY]

    # Put nicer names on variables ()
    names(NewMergedDataX) <- gsub("^t", "Time", names(NewMergedDataX))
    names(NewMergedDataX) <- gsub("^f", "Frequency", names(NewMergedDataX))
    names(NewMergedDataX) <- gsub("-mean\\(\\)", "Mean", names(NewMergedDataX))
    names(NewMergedDataX) <- gsub("-std\\(\\)", "StdDev", names(NewMergedDataX))
    names(NewMergedDataX) <- gsub("-", "", names(NewMergedDataX))
    names(NewMergedDataX) <- gsub("BodyBody", "Body", names(NewMergedDataX))

    # Final merge with the subject, the activity and the measures
    Final<- cbind(Subject = MergedSub, Activities = MergeDataY, NewMergedDataX)

    # Average for each subject and activity
    library(dplyr)
    FinalDelivery <- Final %>% group_by(Subject, Activities) %>% summarise_each(funs(mean))

    # Write the file
    write.table(FinalDelivery, "C:\\Users\\luis.a.matias\\Desktop\\UCI HAR Dataset/FinalDelivery.txt", row.names = FALSE)
}