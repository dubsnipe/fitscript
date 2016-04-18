runanalysis<-function(dataA="UCI HAR Dataset/test/X_test.txt",
                      dataB="UCI HAR Dataset/test/y_test.txt",
                      dataC="UCI HAR Dataset/test/subject_test.txt",
                      dataD="UCI HAR Dataset/train/subject_train.txt",
                      dataE="UCI HAR Dataset/train/X_train.txt",
                      dataF="UCI HAR Dataset/train/y_train.txt"){
  
  #let's get started!
  library(dplyr)
  
  #reading all data
  linesa<-readLines(dataA)
  linesb<-as.numeric(readLines(dataB))
  linesc<-as.numeric(readLines(dataC))
  linesd<-as.numeric(readLines(dataD))
  linese<-readLines(dataE)
  linesf<-as.numeric(readLines(dataF))
  
  
  d<-data.frame(matrix(NA, nrow = 0, ncol = 4))
  f<-data.frame(matrix(NA, nrow = 0, ncol = 4))
  
  #separating data and creating a table for test group
  for(i in 1:length(linesa)){
    linesa[i]<-gsub("^ *| *$","",linesa[i])
    tablea<-as.numeric(unlist(strsplit(as.character(linesa[i]),"\\s+")))
    for(j in 1:length(tablea)){
      d<-rbind(d,c(linesb[i],linesc[i],tablea[j]))
    }
  }
  colnames(d)<-c("activity","subject","reading")
  
  #separating data and creating a table for train group
  for(i in 1:length(linese)){
    linese[i]<-gsub("^ *| *$","",linese[i])
    tablec<-as.numeric(unlist(strsplit(as.character(linese[i]),"\\s+")))
    for(j in 1:length(tablec)){
      f<-rbind(f,c(linesf[i],linesd[i],tablec[j]))
    }
  }
  
  #I renamed columns and added a new column to represent the data group before merging both datasets
  colnames(f)<-c("activity","subject","reading")
  d$sample <- "test"
  f$sample <- "train"
  final<-rbind(d,f)
  
  #turning into dplyr table
  final<-tbl_df(final)
  
  #renaming the activity values
  final<-mutate(final, activity = replace(activity, activity==1, "walking"))
  final<-mutate(final, activity = replace(activity, activity==2, "walkingupstairs"))
  final<-mutate(final, activity = replace(activity, activity==3, "walkingdownstairs"))
  final<-mutate(final, activity = replace(activity, activity==4, "sitting"))
  final<-mutate(final, activity = replace(activity, activity==5, "standing"))
  final<-mutate(final, activity = replace(activity, activity==6, "laying"))
  
  #preparing to summarize
  tosummarize<-group_by(final,activity)
  tosummarize2<-group_by(final,subject)
  
  #I divided the results in two tables: one shows the mean and standard deviation per activity, and the other per user.
  summarize1<-summarize(tosummarize,mean=mean(reading),sd=sd(reading))
  summarize2<-summarize(tosummarize2,mean=mean(reading),sd=sd(reading))
  
  #This will bind both previous tables together with a new column to label which type of data it represents.
  summarize1x<-mutate(summarize1,datatype="activity")
  summarize2x<-mutate(summarize2,datatype="user")
  colnames(summarize1x)<-NA
  colnames(summarize2x)<-NA
  step5<-rbind(summarize1x,summarize2x)
  colnames(step5)<-c("variable","mean","sd","type")
  step5<-select(step5,type,variable,mean)
  
  #putting all results in a list to make life easier
  answer<-list(final,summarize1,summarize2,step5)
  return(answer)
}