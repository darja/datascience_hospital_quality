readDataAndCheckState <- function(state) {
    data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
    
    validStates <- unique(data$State)
    
    if (is.null(state) | !(state %in% validStates)) {
        stop("Invalid state")
    }
    
    data
}

getOutcomeDataColumn <- function(outcome) {
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if (is.null(outcome) | !(outcome %in% validOutcomes)) {
        stop("Invalid outcome")
    }
    
    dataColumns <- c(11, # "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                     17, # "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                     23  # "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    )
    
    dataColumns[match(x = outcome, table = validOutcomes)]
}