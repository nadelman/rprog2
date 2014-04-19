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
  
  ## all vecotrized operations
  finalId <- formatC(id, width = 3, format = "d", flag = "0")
  filePattern <- paste(finalId, '.csv', sep = "")
  finalFile <- paste(directory, '/', finalId, '.csv', sep = "")
  runningVals <- c()
  
  for(file in finalFile) {
    data <- read.csv(file, header=TRUE)
    
    if (pollutant == "sulfate") {
      runningVals <- append(runningVals, data$sulfate[!is.na(data$sulfate)])    
    } else if( pollutant == "nitrate") {
      runningVals <- append(runningVals, data$nitrate[!is.na(data$nitrate)])    
    }
  }
  
  mean(runningVals)
}