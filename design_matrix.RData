design_matrix <- function(testResults, 
                          connectMatrix,
                          rankVector,
                          testNiveau){
  pValues <- testResults$p.value
  
  sorted <- sort(rankVector, index = TRUE)$ix ## Algo mit bester Performance in 1. Zeile usw
  connectMatrix <- connectMatrix[sorted,]
  rNames <- rownames(pValues)
  cNames <- colnames(pValues)
  for(i in rownames(connectMatrix)){
    if(i %in% rNames){
      j <- 1
      while(!is.na(pValues[i,cNames[j]])){
        if(pValues[i,cNames[j]] < testNiveau){
          
          if(connectMatrix[cNames[j],i] == 0){
            connectMatrix[i,cNames[j]] <- 1
          }
        }
        j <- j + 1
        if(j > ncol(pValues)){
          break
        }
      }
    }
    
    if(i %in% cNames){
      k <- nrow(pValues)
      while(!is.na(pValues[rNames[k],i])){
        if(pValues[rNames[k],i] < testNiveau){
          if(connectMatrix[rNames[k],i] == 0){
            connectMatrix[i,rNames[k]] <- 1
          }
        }
        k <- k - 1
        if(k == 0){
          break
        }
      }
    }
  }
  return(connectMatrix)
}

