# best.R

# setwd("c:/users/jd022981/dropbox/dev/r/2014.04.09 R Programming (Coursera)/ProgAssign3")

best <- function( state, outcome ) { 
    
    ## Read outcome data.  
    ## Added na.strings to change "Not Available" to NA, which prevents coercian
    ## warnings in the as.numeric call below.  
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    
    ## Check that state and outcome are valid.  
    if( !( state %in% state.abb ) )
        stop("invalid state")
    if( !( outcome %in% c("heart attack","heart failure","pneumonia") ) )
        stop("invalid outcome")

    ## filter on state.  
    outcomeData <- subset( outcomeData, State==state )

    ## get the death rate based on the outcome.  
    if( outcome == "heart attack" ) 
        outcomeColumn <- 11
    if( outcome == "heart failure" ) 
        outcomeColumn <- 17
    if( outcome == "pneumonia" ) 
        outcomeColumn <- 23

    outcomeData[,outcomeColumn] <- as.numeric(outcomeData[,outcomeColumn])
    outcomeDataSort <- outcomeData[order(outcomeData[outcomeColumn],outcomeData[2]) , ]
    
    ## Return hospital name in that state with lowest 30-day death rate.  
    outcomeDataSort[1,2]

}

## TEST CASES

## best("TX", "heart attack")
## "CYPRESS FAIRBANKS MEDICAL CENTER"

## best("TX", "heart failure")
## "FORT DUNCAN MEDICAL CENTER"

## best("MD", "heart attack")
## "JOHNS HOPKINS HOSPITAL, THE"

## best("MD", "pneumonia")
## "GREATER BALTIMORE MEDICAL CENTER"

## best("BB", "heart attack")
## Error in best("BB", "heart attack") : invalid state

## best("NY", "hert attack")
## Error in best("NY", "hert attack") : invalid outcome
