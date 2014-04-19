complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  output <- data.frame(id= numeric(0), nobs= integer(0))
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  for(monitorId in id) {
    finalId <- formatC(monitorId, width = 3, format = "d", flag = "0")  
    filePattern <- paste(finalId, '.csv', sep = "")
    finalFile <- paste(directory, '/', finalId, '.csv', sep = "")
    data <- read.csv(finalFile, header=TRUE)
    
    ## leverage fact that complete cases is a vector of boolean vals
    row <- c(monitorId,sum(complete.cases(data)))
    output <- rbind(output, row)
  }
  
  names(output) <- c("id","nobs")
  output
}