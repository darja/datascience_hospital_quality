rankhospital <- function(state, outcome, num = "best") {
    data <- readDataAndCheckState(state)
    
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    
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
    subdata <- subdata[!is.na(subdata$outcome),]
    subdata <- subdata[order(subdata$outcome, subdata$name),]
    subdata <- within(subdata, rank <- rank(order(outcome, name), ties.method = "first"))
    
    if (num == "best") {
        num <- 0
    } else if (num == "worst") {
        num <- nrow(subdata)
    }
    
    subdata[subdata$rank == num, 1]
}