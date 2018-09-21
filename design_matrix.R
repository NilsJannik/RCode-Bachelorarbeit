design_matrix <- function(testResults, 
                          testNiveau = 0.05,
                          connectMatrix){
  pValues <- testResults$p.value
  colindex <- 1
  for(i in 1 : dim(pValues)[1]){
    for(j in 1 : colindex){
      if(pValues[i,j] < 0.05){
        connectMatrix[i + 1, j] <- 1
      }
    }
    colindex <- colindex + 1
  }
  return(connectMatrix)
}