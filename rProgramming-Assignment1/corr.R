corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    #for all files {
    #   load data
    #   for all row in data {
    #        get row
    #        check condition
    #        store to dataframe/two vector/...
    #    }
    #    check length( dataframe/two vector/...) with threshold {
    #        calc corr
    #    }
    #    else
    #    next file
    #}

    ## init empty vector
    corrs <- vector(mode="numeric", length=0)

    ## identify all available files to read in
    fullFileList <- dir(directory, pattern = '\\.csv', full.names = TRUE)

    ## number of available files
    nfiles <- length(fullFileList)
    ## for each file, do something
    #results <- vector(mode="numeric", length=0)
    for(fileName in fullFileList){
        #read data into data frame
        sample <- read.csv(fileName, header = TRUE, sep = ",")
        nobs <- sum(complete.cases(sample))
        if(nobs > threshold){
            thiscorr <- cor(sample[,"sulfate"],sample[,"nitrate"], use="complete.obs")
            corrs[fileName] <- thiscorr
        }
    }
    as.vector(corrs)
}