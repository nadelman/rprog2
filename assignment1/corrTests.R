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