#----  Load the needed packages
packages <- c("data.table", "reshape2", "dplyr")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

pc_path <- getwd()

filesPath <- paste (pc_path, "/UCI HAR Dataset",sep="")

#----  Read File
#---- 1-subject files
Data_SubjectTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
Data_SubjectTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

#----  2 activity files
Data_ActivityTrain <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
Data_ActivityTest  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))

#---- 3 data files.
Data_Train <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
Data_Test  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))

#----  Merge the training and the test sets by row binding 
All_Subject <- rbind(Data_SubjectTrain, Data_SubjectTest)
names(All_Subject)[names(All_Subject) == 'V1'] <- 'subject'
All_Activity<- rbind(Data_ActivityTrain, Data_ActivityTest)
names(All_Activity)[names(All_Activity) == 'V1'] <- 'activityNum'

#---- combine the DATA training and test files

Data_Table <- rbind(Data_Train, Data_Test)

#---- Lookup names and set variable names
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setNames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(Data_Table) <- dataFeatures$featureName

#  activity labels
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setNames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merge columns
labels_data<- cbind(All_Activity, All_Subject)
Data_Table <- cbind(labels_data, Data_Table)


# Reading "features.txt" for just mean and std 
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name
dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
Data_Table<- subset(Data_Table,select=dataFeaturesMeanStd) 

# name of activity into Data_Table
Data_Table <- merge(activityLabels, Data_Table , by="activityNum", all.x=TRUE)
Data_Table$activityName <- as.character(Data_Table$activityName)

# create Data_Table with variable means sorted by subject and Activity
Data_Table$activityName <- as.character(Data_Table$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = Data_Table, mean) 
Data_Table<- tbl_df(arrange(dataAggr,subject,activityName))

#Save the results
write.table(Data_Table, file = "tidy_data.txt", sep = ",", row.names = FALSE)


