runanalysis<-function(dataA,dataB,dataC,dataD,dataE,dataF){
  library(dplyr)
  #dataA: location of X_test.txt
  #dataB: location of y_test.txt
  #dataE: location of X_train.txt
  #dataF: location of y_train.txt
  
  linesa<-readLines(dataA)
  linesb<<-as.numeric(readLines(dataB))
  linesc<<-as.numeric(readLines(dataC))
  linesd<<-as.numeric(readLines(dataD))
  linese<-readLines(dataE)
  linesf<<-as.numeric(readLines(dataF))
  
  
  d<<-data.frame(matrix(NA, nrow = 0, ncol = 4))
  f<<-data.frame(matrix(NA, nrow = 0, ncol = 4))
  
  
  for(i in 1:length(linesa)){
    
    linesa[i]<-gsub("^ *| *$","",linesa[i])
    tablea<<-as.numeric(unlist(strsplit(as.character(linesa[i]),"\\s+")))
    
    for(j in 1:length(tablea)){
      d<<-rbind(d,c(linesb[i],linesc[i],tablea[j]))
    }
  }
  colnames(d)<-c("activity","subject","reading")
  
  
  for(i in 1:length(linese)){
    
    linese[i]<-gsub("^ *| *$","",linese[i])
    tablec<<-as.numeric(unlist(strsplit(as.character(linese[i]),"\\s+")))
    
    for(j in 1:length(tablec)){
      f<<-rbind(f,c(linesf[i],linesd[i],tablec[j]))
    }
  }
  
  colnames(f)<-c("activity","subject","reading")
  
  d$sample <- "test"
  f$sample <- "train"
  
  final<<-rbind(d,f)
  
  #turning into dplyr table
  final<<-tbl_df(final)
  
  write.csv(final,file="text3.csv")
  
  return(final)
}