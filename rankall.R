rankall <- function(outcome, num = "best") {
    data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
    dataCol <- getOutcomeDataColumn(outcome)
    
    subset <- data.frame(state = data$State, name = data$Hospital.Name, outcome = as.numeric(data[[dataCol]]))
    subset <- subset[!is.na(subset$outcome),]
    
    states <- unique(subset$State)
    
    grouped <- split(subset, subset$state)
    rankedList <- mapply(rankSubset, grouped, MoreArgs = list(num = num), SIMPLIFY = F)
    rankedFrame <- data.frame(hospital = matrix(unlist(rankedList), nrow = length(rankedList)), state = names(rankedList))
    
    rankedFrame
}
