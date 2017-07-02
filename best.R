best <- function(state, outcome) {
    data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
    
    validStates <- unique(data$State)
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if (is.null(state) | !(state %in% validStates)) {
        stop("Invalid state")
    }
    
    if (is.null(outcome) | !(outcome %in% validOutcomes)) {
        stop("Invalid outcome")
    }
    
    dataColumns <- c(11, # "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                     17, # "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                     23  # "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    )
    
    dataCol <- dataColumns[match(x = outcome, table = validOutcomes)]
    
    stateHospitals <- data[data$State == state,]
    
    subdata <- data.frame(name=stateHospitals[[2]], outcome=as.numeric(stateHospitals[[dataCol]]))
    bestOutcome <- min(subdata$outcome[!is.na(subdata$outcome)])
    bestHospitals <- sort(as.character(subdata$name[subdata$outcome == bestOutcome]))
    min(bestHospitals)
}