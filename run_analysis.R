runanalysis<-function(){
  
  locFeatures<<-"UCI HAR Dataset/features.txt"
  
  locXTest<<-"UCI HAR Dataset/test/X_test2.txt"
  locXtrain<<-"UCI HAR Dataset/train/X_train2.txt"
  
  locActivityTest<<-"UCI HAR Dataset/test/y_test2.txt"
  locSubjectTest<<-"UCI HAR Dataset/test/subject_test2.txt"
  
  locActivityTrain<<-"UCI HAR Dataset/train/y_train2.txt"
  locSubjectTrain<<-"UCI HAR Dataset/train/subject_train2.txt"
  
  
  #let's get started!
  library(dplyr)
  
  #reading all data
  xTest<<-readLines(locXTest)
  xTrain<<-readLines(locXtrain)
  features<<-readLines(locFeatures)
  
  subjectTest<<-readLines(locSubjectTest)
  activityTest<<-readLines(locActivityTest)
  subjectTrain<<-readLines(locSubjectTrain)
  activityTrain<<-readLines(locActivityTrain)
  
  
  #d<-data.frame(matrix(NA, nrow = 0, ncol = 4))
  #f<-data.frame(matrix(NA, nrow = 0, ncol = 4))
  
  #separating dataa and datad; creating a table for test group
  
  for(j in 1:length(features)){
    features[j]<<-gsub("^\\d+\\s+", "", features[j])
  }
  
  testTotal<<-NULL
  trainTotal<<-NULL
  
  for(i in 1:length(xTest)){
    xTest[i]<<-gsub("(^ *| *$)","",xTest[i])
    tableTest<<-as.numeric(unlist(strsplit(as.character(xTest[i]),"\\s+")))
    testSet<<-cbind(obs=i,features,tableTest,subject=subjectTest[i],activity=activityTrain[i],type="test")
    testTotal<<-rbind(testTotal,testSet)
  }
  
  for(i in 1:length(xTrain)){
    xTrain[i]<<-gsub("(^ *| *$)","",xTrain[i])
    tableTrain<<-as.numeric(unlist(strsplit(as.character(xTrain[i]),"\\s+")))
    trainSet<<-cbind(obs=i,features,tableTrain,subject=subjectTrain[i],activity=activityTrain[i],type="train")
    trainTotal<<-rbind(trainTotal,trainSet)
  }
  
  testTrainTotal<<-rbind(trainTotal,testTotal)
  #training and test set have been merged
  
  ttTotal<<-tbl_df(as.data.frame(testTrainTotal))
  requiredMeasurements<<-c("tBodyAcc-mean()-X", "tBodyAcc-mean()-Y", "tBodyAcc-mean()-Z", "tBodyAcc-std()-X", "tBodyAcc-std()-Y", "tBodyAcc-std()-Z", "tGravityAcc-mean()-X", "tGravityAcc-mean()-Y", "tGravityAcc-mean()-Z", "tGravityAcc-std()-X", "tGravityAcc-std()-Y", "tGravityAcc-std()-Z", "tBodyAccJerk-mean()-X", "tBodyAccJerk-mean()-Y", "tBodyAccJerk-mean()-Z", "tBodyAccJerk-std()-X", "tBodyAccJerk-std()-Y", "tBodyAccJerk-std()-Z", "tBodyGyro-mean()-X", "tBodyGyro-mean()-Y", "tBodyGyro-mean()-Z", "tBodyGyro-std()-X", "tBodyGyro-std()-Y", "tBodyGyro-std()-Z", "tBodyGyroJerk-mean()-X", "tBodyGyroJerk-mean()-Y", "tBodyGyroJerk-mean()-Z", "tBodyGyroJerk-std()-X", "tBodyGyroJerk-std()-Y", "tBodyGyroJerk-std()-Z", "tBodyAccMag-mean()", "tBodyAccMag-std()", "tGravityAccMag-mean()", "tGravityAccMag-std()", "tBodyAccJerkMag-mean()", "tBodyAccJerkMag-std()", "tBodyGyroMag-mean()", "tBodyGyroMag-std()", "tBodyGyroJerkMag-mean()", "tBodyGyroJerkMag-std()", "fBodyAcc-mean()-X", "fBodyAcc-mean()-Y", "fBodyAcc-mean()-Z", "fBodyAcc-std()-X", "fBodyAcc-std()-Y", "fBodyAcc-std()-Z", "fBodyAccJerk-mean()-X", "fBodyAccJerk-mean()-Y", "fBodyAccJerk-mean()-Z", "fBodyAccJerk-std()-X", "fBodyAccJerk-std()-Y", "fBodyAccJerk-std()-Z", "fBodyGyro-mean()-X", "fBodyGyro-mean()-Y", "fBodyGyro-mean()-Z", "fBodyGyro-std()-X", "fBodyGyro-std()-Y", "fBodyGyro-std()-Z", "fBodyAccMag-mean()", "fBodyAccMag-std()", "fBodyBodyAccJerkMag-mean()", "fBodyBodyAccJerkMag-std()", "fBodyBodyGyroMag-mean()", "fBodyBodyGyroMag-std()", "fBodyBodyGyroJerkMag-mean()", "fBodyBodyGyroJerkMag-std()")
  ttTotalFilter<<-filter(ttTotal,features %in% requiredMeasurements)
  #extracting mean and standard deviation 
  
  
  colnames(ttTotalFilter)<<-c("observation","measurement","result","subject","activity","type")
  
  ttTotalFilter<<-mutate(ttTotalFilter,activity=as.character(activity),result=as.numeric(as.character(result)))
  ttTotalFilter<<-mutate(ttTotalFilter, activity = replace(activity, activity=="1", "walking"))
  ttTotalFilter<<-mutate(ttTotalFilter, activity = replace(activity, activity=="2", "walkingupstairs"))
  ttTotalFilter<<-mutate(ttTotalFilter, activity = replace(activity, activity=="3", "walkingdownstairs"))
  ttTotalFilter<<-mutate(ttTotalFilter, activity = replace(activity, activity=="4", "sitting"))
  ttTotalFilter<<-mutate(ttTotalFilter, activity = replace(activity, activity=="5", "standing"))
  ttTotalFilter<<-mutate(ttTotalFilter, activity = replace(activity, activity=="6", "laying"))
  #renaming the activity values
 
  answer1<-group_by(ttTotalFilter,measurement)
  answer2<-group_by(ttTotalFilter,subject)
  
  answer<-rbind(answer1,answer2)
  return(answer)
  
    #I divided the results in two tables: one shows the mean and standard deviation per activity, and the other per user.
  #summarize1<<-summarize(tosummarize1,mean=mean(result),sd=sd(result))
  #summarize2<<-summarize(tosummarize2,mean=mean(result),sd=sd(result))
  
  
}