rankhospital<-function(state, outcome, num="best"){
  ## Read outcome data
  
  ## Check that state and outcome are valid
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if (outcome=="heart attack") {
    outcome<-"Heart.Attack"
  } else if (outcome=="heart failure") {
    outcome<-"Heart.Failure"
  } else if (outcome=="pneumonia") {
    outcome<-"Pneumonia"
  } else {
    stop("invalid outcome")
  }
  
  cname<-paste0("Hospital.30.Day.Death..Mortality..Rates.from.",outcome)
  index<-df["State"]==state
  if (!any(index)) {
    stop("invalid state")
  }
  
  index2<-df[cname][index]!="Not Available"
  rank_index<-rank(as.numeric(df[cname][index][index2]),ties.method='min')
  for (i in 1:max(rank_index)){
    remember<-rank_index==i
    if (length(rank_index[remember])>=2){
      temp<-df["Hospital.Name"][index][index2][rank_index==i]
      temp_index<-rank(temp)
      new<-rank_index[remember]-1+temp_index
      for (j in 1:length(temp)){
        rank_index[remember][j]<-new[j]
      }
    }
  }
  
  if (num=="worst") num<-max(rank_index)
  if (num=="best") num<-1
  if (max(rank_index)<num){
    result<-NA
  } else {
    result<-df["Hospital.Name"][index][index2][rank_index==num]
  }
  result
}