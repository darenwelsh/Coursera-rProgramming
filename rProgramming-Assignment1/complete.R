complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    ## identify all available files to read in
    fullFileList <- dir(directory, pattern = '\\.csv', full.names = TRUE)
    
    ## cull list to those from id vector
    workingFileList <- fullFileList[id]
    
    ## read data into matrix
    ##tables <- sapply(workingFileList, read.csv)
    
    ## for each file, count complete cases
    nobs <- sapply(workingFileList, function(f) sum(complete.cases(read.csv(f))))
    
    ## output
    results <- data.frame(id, nobs)
    rownames(results) <- NULL
    results
}
