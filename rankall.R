rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  file <- "outcome-of-care-measures.csv"
  hospitalName <- 'Hospital.Name'
  state<-'State'
  data <- read.csv(file,colClasses = "character")
  states <- unique(data[,7])
  validOutcome <- c('heart attack','heart failure','pneumonia')
  if(!outcome %in% validOutcome)
    stop("invalid outcome")
  colname <- ''
  if(outcome == 'heart attack')
    colname = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
  else if(outcome == 'heart failure')
    colname = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
  else if(outcome == 'pneumonia')
    colname = 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
  data[,colname]<-as.numeric(data[,colname])
  data1<-subset(data,!is.na(data[colname]))
  data2<-data1[,c(state,hospitalName,colname)]
  data3<-split(data2,data2$State)
  data4<-lapply(data3, function(x) x<-x[order(x[,colname],x[,hospitalName]),])
  mycols <- c('state','hospital','rate');
  results <- data.frame(x=character(0),y=character(0),z=character(0))
  colnames(results) <- mycols
  numVar <- 1
  for(item in states){
    frame <- as.data.frame(data4[item])
    colnames(frame)<-c(mycols)
    if(is.character(num)){
      if(num == 'best')
        numVar <- 1
      else if(num =='worst')
        numVar <- nrow(frame)
    }else if(is.numeric(num)){
      numVar <- num
    }
    selected <- frame[numVar,c(2,1,3)]
    selected[2] = item
    results <- rbind.data.frame(results,selected)
  }
  results[order(results[,2]),c(1,2)]
}

##OUTPUTS

##> source("rankall.R")
##> head(rankall("heart attack", 20), 10)
##hospital state
##NA                                 <NA>    AK
##59       D W MCMILLAN MEMORIAL HOSPITAL    AL
##211   ARKANSAS METHODIST MEDICAL CENTER    AR
##154 JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
##564               SHERMAN OAKS HOSPITAL    CA
##651            SKY RIDGE MEDICAL CENTER    CO
##696             MIDSTATE MEDICAL CENTER    CT
##NA2                                <NA>    DC
##NA1                                <NA>    DE
##808      SOUTH FLORIDA BAPTIST HOSPITAL    FL
##Warning message:
  ##In rankall("heart attack", 20) : NAs introduced by coercion
##> tail(rankall("pneumonia", "worst"), 3)
##hospital state
##4588 MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
##4505                     PLATEAU MEDICAL CENTER    WV
##4654           NORTH BIG HORN HOSPITAL DISTRICT    WY
##Warning message:
  ##In rankall("pneumonia", "worst") : NAs introduced by coercion
##> tail(rankall("heart failure"), 10)
##hospital state
##3797                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL    TN
##3935                                        FORT DUNCAN MEDICAL CENTER    TX
##4237 VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER    UT
##4341                                          SENTARA POTOMAC HOSPITAL    VA
##4278                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR    VI
##4275                                              SPRINGFIELD HOSPITAL    VT
##4399                                         HARBORVIEW MEDICAL CENTER    WA
##4561                                    AURORA ST LUKES MEDICAL CENTER    WI
##4473                                         FAIRMONT GENERAL HOSPITAL    WV
##4644                                        CHEYENNE VA MEDICAL CENTER    WY
##Warning message:
  ##In rankall("heart failure") : NAs introduced by coercion
##> 


