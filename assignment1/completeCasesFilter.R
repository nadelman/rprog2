## 
##
##
##
completeCasesFilter <- function (inputDirectory, targetDirectory) {
  fileList <- list.files(path = inputDirectory, include.dirs=TRUE)
  
  for(file in fileList) {
    data <- read.csv(paste(inputDirectory, '/', file, sep = ""), header=TRUE)
    completeCases <- subset(data, complete.cases(data))
    
    newFile <- paste(targetDirectory, '/', file, sep = "")
    print(newFile)
    
    write.table(completeCases, file=newFile, append=FALSE)
  }
  
} 