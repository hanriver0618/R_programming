pollutantmean <- function(directory,pollutant,id=1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or nitrate"
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
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
    result<-a[pollutant][!is.na(a[pollutant])]
    v<-append(v,result)
  }
  mean(v)
}
