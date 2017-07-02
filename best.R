best <- function(state, outcome) {
    data <- readDataAndCheckState(state)
    dataCol <- getOutcomeDataColumn(outcome)
    
    stateHospitals <- data[data$State == state,]
    
    subdata <- data.frame(name=stateHospitals[[2]], outcome=as.numeric(stateHospitals[[dataCol]]))
    bestOutcome <- min(subdata$outcome[!is.na(subdata$outcome)])
    bestHospitals <- sort(as.character(subdata$name[subdata$outcome == bestOutcome]))
    min(bestHospitals)
}