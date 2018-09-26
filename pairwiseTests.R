
pairwiseTests <- function(sectionObjects = sections, perfName, confName, algo.Name){
  # Clustergroesse nachschauen:
  N <- length(sectionObjects)
  results <- c(rep(list(NULL), N))
 
  # paarweise Tests mit dem nemenyi Test
  for(i in 1 : N){
    results[[i]] <- posthoc.kruskal.nemenyi.test(
      x = sectionObjects[[i]]$ydist, 
      g = sectionObjects[[i]]$confName, 
      dist = "Chisquare")
  }
  
  # Bestimme in den N Cluster die Durchschnittsraenge ueber alle Werte des
  # Performanzindikator
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