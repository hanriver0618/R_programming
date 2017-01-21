best<-function(state,outcome){
  ## Read outcome Data
  ## Check that state and outcome are valic
  ## Return hospital name in that state with lowest 30-day death
  
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
  d<-as.numeric(df[cname][index][index2])
  df["Hospital.Name"][index][index2][d==min(d)]

}