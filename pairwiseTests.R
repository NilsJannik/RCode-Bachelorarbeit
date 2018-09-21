pairwiseTests <- function(sectionObjects, perfName, confName, algo.Name){
  N <- length(sectionObjects)
  results <- c(rep(list(NULL), N))
  
  for(i in 1 : N){
    results[[i]] <- posthoc.kruskal.nemenyi.test(
      sectionObjects[[i]]$ydist, 
      sectionObjects[[i]]$confName, 
      "Chisquare")
  }
  
  rankMatrix <- matrix(ncol = length(algo.Name), nrow = length(sectionObjects))
  colnames(rankMatrix) <- algo.Name
  for(i in 1:length(sectionObjects)){
    
    CN <- colnames(sectionObjects[[i]])
    
    performance <- sectionObjects[[i]][,CN == perfName]
    algos <- sectionObjects[[i]][,CN == confName]
    
    rankData <- data.frame(algos, performance, rank(performance))
    
    for(j in 1 : length(algo.Name)){
      rankMatrix[i,j] <- mean(rankData[algos == algo.Name[j],ncol(rankData)])
    }
  }  

  return(list(results, rankMatrix))
}