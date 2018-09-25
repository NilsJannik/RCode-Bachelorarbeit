clusterFunctionHclust <- function(data, 
                             perfName, 
                             .expID, 
                             distMethod = "euclidean", 
                             clusterMethod = "complete"){
  N <- length(unique(.expID))
  data <- data.frame(subset(data, select = perfName), 
                     rep(1 : N, each = table(.expID)[1]))
  colnames(data) <- c("ydist",".expID")
  
  h.cluster <- hclust(dist(data, method = distMethod), 
                      method = clusterMethod)  
  ########
  
  mergefunction <- function(hc, clustersize){ # braucht aktuell noch ewig
    # Filtering the Mergeobject
    mo <- hc$merge 
    # How many Objects were clustered
    N <- max(abs(mo))
    Layers <- list()
    Layers[[N]] <- 0
    
    searchPosList <- function(object, x, step = FALSE){
      for(i in 1:length(object)){
        if(step){
          if(!is.null(object[[i]])){
            if(object[[i]][length(object[[i]])] == x){
              return(i)
            }
          }
        } else {
          if(!is.null(object[[i]])){
            for(j in 1:(length(object[[i]]) - 1)){
              if(object[[i]][j] == x){
                return(i)
              }
            }
          }
        }
      }
      return(NA)
    }
    for(i in 1 : (N - clustersize)){
      if(i%%500 == 0){
        print(i)
      }
      object1 <- mo[i,1]
      object2 <- mo[i,2]
      
      if(object1 < 0 && object2 < 0){
        object1 <- abs(object1)
        Layers[[object1]] <- c(object1, abs(object2))
        Layers[[object1]][length(Layers[[object1]]) + 1] <- i
      }
      else if(object1 < 0 && object2 > 0){
        object1 <- abs(object1)
        posObject2 <- searchPosList(Layers, mo[i,2], step = TRUE)
        nl2 <- length(Layers[[posObject2]])
        Layers[[posObject2]][nl2] <- object1
        Layers[[posObject2]][nl2 + 1] <- i
      }
      else if(object1 > 0 && object2 > 0){
        # Use the Mergeindices for pointing on the appropriate row
        posObject1 <- searchPosList(Layers, object1, step = TRUE)
        posObject2 <- searchPosList(Layers, object2, step = TRUE)
        nl1 <- length(Layers[[posObject1]]) - 1
        nl2 <- length(Layers[[posObject2]]) - 1
        # Merge Object 1 and Object 2
        for(j in 1:nl2){
          Layers[[posObject1]][j + nl1] <- Layers[[posObject2]][j]
        }
        Layers[[posObject1]][length(Layers[[posObject1]]) + 1] <- i
        Layers[[posObject2]] <- 0
      }
      
    }
    # folgendes noch schoener loesen:
    ##
    lLayers <- length(Layers)
    Layers[[lLayers]] <- NULL
    result <- list()
    ctr <- 1
    for(i in 1:(lLayers - 1)){
      if(!is.null(Layers[[i]]) && Layers[[i]] != 0){
        result[[ctr]] <- Layers[[i]]
        ctr <- ctr + 1
      }
    }
    ##
    return(result)
  }
  
  clusterResult <- mergefunction(h.cluster,5)
  ########
  return(clusterResult)
  
}



