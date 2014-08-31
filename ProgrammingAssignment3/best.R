best <- function(state, outcome) {
        ## Validate the parameters passed. STOP if not matching with outcomes vector 
        outcomes = c("heart attack", "heart failure", "pneumonia")
        if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
        
        ## Read outcome data from the csv file
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Identify the relevant columns and assign column names to those
        data <- data[c(2, 7, 11, 17, 23)]
        names(data)[1] <- "name"
        names(data)[2] <- "state"
        names(data)[3] <- "heart attack"
        names(data)[4] <- "heart failure"
        names(data)[5] <- "pneumonia"
        
        ## Validate the two digit state code. STOP execution if not found
        states <- data[, 2]
        states <- unique(states)
        if( state %in% states == FALSE ) stop("invalid state")
        
        ## Verify rows with the matching state code	
        data <- data[data$state==state & data[outcome] != 'Not Available', ]
        vals <- data[, outcome]
        rowNum <- which.min(vals)
        
        ## Return hospital name
        data[rowNum, ]$name
}