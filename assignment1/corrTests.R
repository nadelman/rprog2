test.corr150 <- function(tolerance) {
  
  cr <- corr("specdata", 150)
  head <- head(cr)
  summary <- summary(cr)
  summaryVector <- c(summary[[1]], summary[[2]], summary[[3]], summary[[4]], summary[[5]], summary[[6]])
  
  # these values are pulled from https://d396qusza40orc.cloudfront.net/rprog%2Fdoc%2Fcorr-demo.html
  exHead <- c(-0.01896, -0.14051, -0.04390, -0.06816, -0.12351, -0.07589)
  exSummary <- c(-0.2110, -0.0500,  0.0946,  0.1250, 0.2680, 0.7630) 
  
  #print(paste("Expected head: ",exHead))
  #print(paste("Actual head: ", head))
  #print(paste("Expected summary vectory: ", exSummary))
  #print(paste("Actual summary vectory: ", summaryVector))
  
  #checkEquals(round(head,5), exHead, msg=paste("expected ", exHead, " but was ", head))
  #checkEquals(round(summaryVector,4), exSummary, msg=paste("expected ", summaryVector, " but was ", exSummary))
  
  checkEqualsNumeric(head, exHead, msg=paste("expected ", exHead, " but was ", head), tolerance=tolerance)
  checkEqualsNumeric(summaryVector, exSummary, msg=paste("expected ", summaryVector, " but was ", exSummary), tolerance=tolerance)
}

test.corr400 <- function(tolerance) {
  
  cr <- corr("specdata", 400)
  head <- head(cr)
  summary <- summary(cr)
  summaryVector <- c(summary[[1]], summary[[2]], summary[[3]], summary[[4]], summary[[5]], summary[[6]])
  
  # these values are pulled from https://d396qusza40orc.cloudfront.net/rprog%2Fdoc%2Fcorr-demo.html
  exHead <- c(-0.01896, -0.04390, -0.06816, -0.07589,  0.76313, -0.15783)
  exSummary <- c(-0.1760, -0.0311,  0.1000,  0.1400,  0.2680,  0.7630) 
  
  print(paste("Expected head: ",exHead))
  print(paste("Actual head: ", head))
  print(paste("Expected summary vectory: ", exSummary))
  print(paste("Actual summary vectory: ", summaryVector))
  
  #checkEquals(round(head,5), exHead, msg=paste("expected ", exHead, " but was ", head))
  #checkEquals(round(summaryVector,4), exSummary, msg=paste("expected ", summaryVector, " but was ", exSummary))
  
  checkEqualsNumeric(head, exHead, msg=paste("expected ", exHead, " but was ", head), tolerance=tolerance)
  checkEqualsNumeric(summaryVector, exSummary, msg=paste("expected ", summaryVector, " but was ", exSummary), tolerance=tolerance)
}


test.corr5000 <- function() {
  
  cr <- corr("specdata", 5000)
  head <- head(cr)
  summary <- summary(cr)
  
  print(summary)
  
  # these values are pulled from https://d396qusza40orc.cloudfront.net/rprog%2Fdoc%2Fcorr-demo.html
  exHead <- c()
  exSummaryLength <- 0
  
  print(paste("Expected head: ",exHead))
  print(paste("Actual head: ", head))
  print(paste("Expected summary vectory: ", exSummaryLength))
  print(paste("Actual summary vector: ", length(summary)))
  
  #checkEquals(round(head,5), exHead, msg=paste("expected ", exHead, " but was ", head))
  #checkEquals(round(summaryVector,4), exSummary, msg=paste("expected ", summaryVector, " but was ", exSummary))
  
  checkEqualsNumeric(head, exHead, msg=paste("expected ", exHead, " but was ", head), tolerance=tolerance)
  checkEquals(length(summaryVector), exSummaryLength, msg=paste("expected ", summaryVector, " but was ", exSummary))
}


test.corr0 <- function(tolerance) {
  
  cr <- corr("specdata")
  head <- head(cr)
  summary <- summary(cr)
  summaryVector <- c(summary[[1]], summary[[2]], summary[[3]], summary[[4]], summary[[5]], summary[[6]])
  
  # these values are pulled from https://d396qusza40orc.cloudfront.net/rprog%2Fdoc%2Fcorr-demo.html
  exLength <- 323
  exSummary <- c(-1.0000, -0.0528,  0.1070,  0.1370,  0.2780,  1.0000) 
  
  print(paste("Expected length: ",exLength))
  print(paste("Actual length: ", length(cr)))
  print(paste("Expected summary vectory: ", exSummary))
  print(paste("Actual summary vectory: ", summaryVector))
  
  #checkEquals(round(head,5), exHead, msg=paste("expected ", exHead, " but was ", head))
  #checkEquals(round(summaryVector,4), exSummary, msg=paste("expected ", summaryVector, " but was ", exSummary))
  
  checkEquals(length(cr), exLength, msg=paste("expected ", exLength, " results but was ", length(cr), "  \r"))
  checkEqualsNumeric(summaryVector, exSummary, msg=paste("expected ", summaryVector, " but was ", exSummary), tolerance=tolerance)
}
