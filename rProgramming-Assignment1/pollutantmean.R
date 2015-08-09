pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    
    ## identify all available files to read in
    fullFileList <- dir(directory, pattern = '\\.csv', full.names = TRUE)
    
    ## cull list to those from id vector
    workingFileList <- fullFileList[id]
    
    ## read data into matrix
    tables <- lapply(workingFileList, read.csv)
    ## bind rows to data frame
    dataframe <- do.call(rbind, tables)
    
    # just the pollutant col
    dataset <- dataframe[,pollutant]
    
    ## output
    mean(dataset, na.rm = TRUE)
}
