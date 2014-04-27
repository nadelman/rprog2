##
# Validate that when the value has not been cached yet, 
##
test.cachematrix <- function() {
  # a random invertable matrix
  inputMatrix <-  matrix(c(1,2,3,6,5,4,9,0,1), nrow = 3, ncol = 3)
  expectedInv <- solve(inputMatrix)
  
  testMatrix <- makeCacheMatrix(inputMatrix)
  
  checkEquals(inputMatrix, testMatrix$get())
  checkEquals(NULL, testMatrix$getinv())
  
  output <- cacheSolve(testMatrix)
  
  checkEquals(expectedInv, testMatrix$getinv())
}

