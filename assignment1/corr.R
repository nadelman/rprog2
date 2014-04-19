corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  runningVals <- c()
  fileList <- list.files(path = directory)
             
  ## Return a numeric vector of correlations
  for(file in fileList) {
    finalFile <- paste(directory, '/', file, sep = "")
    data <- read.csv(finalFile, header=TRUE)
    completeCasesOld <- subset(data, complete.cases(data))
    completeCases <- subset(data,  (!is.na(data$sulfate) & !is.na(data$nitrate)))
    
    if(length(completeCases[,1]) != length(completeCasesOld[,1])) {
      print("MISMATCH!!!")
    }
    
    
    if(length(completeCases[,1]) > threshold) {
      runningVals <- append(runningVals, cor(completeCases$sulfate, completeCases$nitrate))
    }    
  }
  
  if(length(runningVals) == 0) {
    runningVals <- c(0)
  }
  
  runningVals
}



