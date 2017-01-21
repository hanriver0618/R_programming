rankall<-function(outcome,num="best"){
  ## read outcome data
  ## check that state and outcome are valid
  ## for each state, find the hospital of the given rank
  ## return a data frame with the hopital names and the (abbreviated) state name
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
  all_state<-sort(unique(df$State))
  hos<-vector()
  for (k in 1:length(all_state)){
    index<-df["State"]==all_state[k]
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
    
    if (num=="worst") {
      num1<-max(rank_index)
    } else if (num=="best") {
      num1<-1
    } else {
      num1<-num
    }
    
    if (max(rank_index)<num1){
      h<-NA
    } else {
       h<-df["Hospital.Name"][index][index2][rank_index==num1]
    }
    hos<-append(hos,h)
  }
  x<-data.frame(hospital=hos,state=all_state)
  rownames(x)<-all_state
  x
}