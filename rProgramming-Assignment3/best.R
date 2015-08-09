best <- function(state, outcome) {
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
                col <- 11
            }
        , 
        "heart failure" = {
            col <- 17
            }
        , 
        "pneumonia" = {
            col <- 23
            }
        , stop("invalid outcome") ## default
        )
    
    ## Read outcome data
    data_frame <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    split_data_frame <- split(data_frame, data_frame$State)
    state_data_frame <- split_data_frame[[state]]
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate (Hospital.Name)
    best_hospital_name <- "FOO"
    best_rate <- 999999
    num_rows <- nrow(state_data_frame)
    for(i in 1:num_rows){
        ## if rate < best_rate, update values
        if(state_data_frame[i,col] != "Not Available"){
            rate <- as.numeric(state_data_frame[i,col])
            ## if there is a tie, use first Hospital.Name alphabetically
            if(rate == best_rate){
                ## only update values if new contender has name earlier in alphabet
                if(state_data_frame[i,"Hospital.Name"] < best_hospital_name ){
                    best_rate <- rate
                    best_hospital_name <- state_data_frame[i,2]
                }
            }
            else if(rate < best_rate){
                best_rate <- rate
                best_hospital_name <- state_data_frame[i,2]
            }
        }
    }
    ## output result
    best_hospital_name
}
