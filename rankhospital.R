rankhospital <- function(state, outcome, num = "best") {
    data <- readDataAndCheckState(state)
    dataCol <- getOutcomeDataColumn(outcome)
    
    stateHospitals <- data[data$State == state,]
    subset <- data.frame(name=stateHospitals[[2]], outcome=as.numeric(stateHospitals[[dataCol]]))
    rankSubset(subset, num)
}

rankSubset <- function(subset, num) {
    subset <- subset[!is.na(subset$outcome),]
    subset <- subset[order(subset$outcome, subset$name),]
    subset <- within(subset, rank <- rank(order(outcome, name), ties.method = "first"))
    
    if (num == "best") {
        num <- 0
    } else if (num == "worst") {
        num <- nrow(subset)
    }
    
    if (nrow(subset) < num) {
        return(NA)
    }
    
    as.character(subset[subset$rank == num, ]$name)
}
