complete<-function(directory,id=1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## where 'id' is the monitor ID number and 'nobs' is the number 
  ## of complete cases
  
  v<-vector()
  for (i in id){
    if (i<10){
      idtemp=paste0("00",toString(i))
    }else if (i<100){
      idtemp=paste0("0",toString(i))
    }else{
      idtemp=toString(i)
    }
    a<-read.csv(paste0('./',directory,'/',idtemp,'.csv'))
    index1<-!is.na(a$sulfate) & !is.na(a$nitrate)
    result<-length(a$sulfate[index1])
    v<-rbind(v,c(i,result))
  }
   v1<-data.frame(v)
   colnames(v1)<-c("id","nobs")
   v1
}
