readDataAndCheckState <- function(state) {
    data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
    
    validStates <- unique(data$State)
    
    if (is.null(state) | !(state %in% validStates)) {
        stop("Invalid state")
    }
    
    data
}