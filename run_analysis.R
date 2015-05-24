
#run_analysis.R
##load packages
run_analysis <- function()
{
  library(plyr)
  
  ## Reads activity files
  path_proj <- file.path("./Coursera", "UCI HAR Dataset")
  dataActivityTest  <- read.table(file.path(path_proj, "test" , "Y_test.txt" ),header = FALSE)
  dataActivityTrain <- read.table(file.path(path_proj, "train", "Y_train.txt"),header = FALSE)

  ## Reads subject files
  dataSubjectTrain <- read.table(file.path(path_proj, "train", "subject_train.txt"),header = FALSE)
  dataSubjectTest  <- read.table(file.path(path_proj, "test" , "subject_test.txt"),header = FALSE)
  
  ## Reads features files
  dataFeaturesTest  <- read.table(file.path(path_proj, "test" , "X_test.txt" ),header = FALSE)
  dataFeaturesTrain <- read.table(file.path(path_proj, "train", "X_train.txt"),header = FALSE)
  
  ## Combines by rows
  dataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
  dataActivity<- rbind(dataActivityTrain, dataActivityTest)
  dataFeatures<- rbind(dataFeaturesTrain, dataFeaturesTest)
  
  ## Names
  names(dataSubject)<-c("subject")
  names(dataActivity)<- c("activity")
  dataFeaturesNames <- read.table(file.path(path_proj, "features.txt"),head=FALSE)
  names(dataFeatures)<- dataFeaturesNames$V2
  
  ## Merges to create Dataset
  dataCombine <- cbind(dataSubject, dataActivity)
  Dataset <- cbind(dataFeatures, dataCombine)
  
  # Extracts only the measurements on the mean and standard deviation for each measurement
  ExtrFeaturesNames<-dataFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesNames$V2)]
  selectNames<-c(as.character(ExtrFeaturesNames), "subject", "activity" )
  Data_extr<-subset(Dataset,select=selectNames)
  
  # Uses descriptive activity names to name the activities in the data set
  activityLabels <- read.table(file.path(path_proj, "activity_labels.txt"),header = FALSE)
  
  # Appropriately labels the data set with descriptive variable names.
  names(Data_extr)<-gsub("^t", "time", names(Data_extr))
  names(Data_extr)<-gsub("^f", "frequency", names(Data_extr))
  names(Data_extr)<-gsub("Acc", "accelerometer", names(Data_extr))
  names(Data_extr)<-gsub("Gyro", "gyroscope", names(Data_extr))
  names(Data_extr)<-gsub("Mag", "magnitude", names(Data_extr))
  names(Data_extr)<-gsub("BodyBody", "body", names(Data_extr))
  
  # From the data set in step 4, creates a second, independent tidy data set with the average of each 
  # variable for each activity and each subject
  Data_tidy<-aggregate(. ~subject + activity, Data_extr, mean)
  Data_tidy<-Data_tidy[order(Data_tidy$subject,Data_tidy$activity),]
  
  destination = paste(path_proj, "/Data_tidy.txt", sep="")

  write.table(Data_tidy, file = destination, row.name=FALSE)
}
