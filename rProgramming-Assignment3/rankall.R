rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    ## Choose col based on "outcome"
    ## Check that outcome is valid
    col <- NULL
    switch(outcome,
           "heart attack" = {
               col <- 11 #"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
           }
           , 
           "heart failure" = {
               col <- 17 #"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
           }
           , 
           "pneumonia" = {
               col <- 23 #"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
           }
           , stop("invalid outcome") ## default
    )
    
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    split_df <- split(df, df$State) # a list
    
    hospital <- sapply(split_df, function(x){
        ## change "Not Available" to NA in order to use functions on NA
        for(i in 1:nrow(x)){
            if(x[i,col] == "Not Available"){
                x[i,col] = NA
            }
        }
        
        ## order hospitals by rank, cut NAs
        rate <- as.numeric(x[,col])
        ranked <- x[order(rate,x[,"Hospital.Name"], na.last=NA),]
        
        ## deal with num cases
        rank <- vector('numeric')
        num_rows <- nrow(ranked)
        if(num == "best"){ 
            rank <- 1
        } else if(num == "worst"){
            rank <- num_rows
        } else {
            rank <- num
        }
        
        ## output result
        if(rank > num_rows){
            #print("NA")
            NA
        } else {
            #print(ranked[rank,"Hospital.Name"])
            ranked[rank,"Hospital.Name"]
        }
        
    }) #end lapply
    state <- names(hospital)
    results <- data.frame(cbind(hospital,state))
    results
}
