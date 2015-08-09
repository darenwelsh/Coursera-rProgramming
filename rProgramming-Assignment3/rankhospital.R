rankhospital <- function(state, outcome, num = "best") {
    ## check validity of "state" argument
    switch(state,
           AL=,
           AK=,
           AZ=,
           AR=,
           CA=,
           CO=,
           CT=,
           DE=,
           FL=,
           GA=,
           HI=,
           ID=,
           IL=,
           IN=,
           IA=,
           KS=,
           KY=,
           LA=,
           ME=,
           MD=,
           MA=,
           MI=,
           MN=,
           MS=,
           MO=,
           MT=,
           NE=,
           NV=,
           NH=,
           NJ=,
           NM=,
           NY=,
           NC=,
           ND=,
           OH=,
           OK=,
           OR=,
           PA=,
           RI=,
           SC=,
           SD=,
           TN=,
           TX=,
           UT=,
           VT=,
           VA=,
           WA=,
           WV=,
           WI=,
           WY=TRUE,
           stop("invalid state")
    )
    
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
    split_df <- split(df, df$State)
    state_df <- split_df[[state]]
    
    ## change "Not Available" to NA in order to use functions on NA
    for(i in 1:nrow(state_df)){
        if(state_df[i,col] == "Not Available"){
            state_df[i,col] = NA
        }
    }
    
    ## order hospitals by rank, cut NAs
    ranked <- state_df[order(state_df[,col],state_df[,"Hospital.Name"], na.last=NA),]
    
    ## deal with num cases
    rank <- 1
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
        print("NA")
    } else {
        print(ranked[rank,"Hospital.Name"])
    }
}
