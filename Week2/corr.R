corr<-function(directory,threshold=0){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating
  ##number of completely observed obaservations (on all
  ## variables ) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  table<-complete(directory)
  index<-table$nobs>threshold
  id1<-table$id[index]
  nobs1<-table$nobs[index]  
  cor_result<-vector()
  for (i in id1){
    if (i<10){
      idtemp=paste0("00",toString(i))
    }else if (i<100){
      idtemp=paste0("0",toString(i))
    }else{
      idtemp=toString(i)
    }
  a<-read.csv(paste0('./',directory,'/',idtemp,'.csv'))
  index1<-!is.na(a$sulfate) & !is.na(a$nitrate)
  cor_result<-append(cor_result,cor(a$sulfate[index1],a$nitrate[index1]))
  }
  cor_result
}
