rankhospital <- function(state, outcome, num) {
        ## Read the outcome data into a vector
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Identify the relevant columns and assign column names to those
        data <- data[c(2, 7, 11, 17, 23)]
        names(data)[1] <- "name"
        names(data)[2] <- "state"
        names(data)[3] <- "heart attack"
        names(data)[4] <- "heart failure"
        names(data)[5] <- "pneumonia"
        
        ## Validate the outcome paramenters passed. STOP if not matching
        outcomes = c("heart attack", "heart failure", "pneumonia")
        if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
        
        ## Validate the two digit state code. STOP execution if not found
        states <- data[, 2]
        states <- unique(states)
        if( state %in% states == FALSE ) stop("invalid state")
        
        ## Validate the num value
        if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
        
        ## Verify rows with the matching state code    
        data <- data[data$state==state & data[outcome] != 'Not Available', ]
        
        ## Order the data
        data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
        data <- data[order(data$name, decreasing = FALSE), ]
        data <- data[order(data[outcome], decreasing = FALSE), ]
        
        ## Use the num parameter
        vals <- data[, outcome]
        if( num == "best" ) {
                rowNum <- which.min(vals)
        } else if( num == "worst" ) {
                rowNum <- which.max(vals)
        } else {
                rowNum <- num
        }
        
        ## Return hospital name for the requested state 
        data[rowNum, ]$name
}