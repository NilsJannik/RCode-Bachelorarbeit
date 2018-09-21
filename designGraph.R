
designGraph <- function(testResults, 
                        data,
                        plotMethod = in_circle(),
                        h = 3,# Maaße muss automatisiert bestimmt werden...
                        v = 2,
                        design_matrix,
                        design_edges){
  
  rankMatrix <- testResults[[2]]
  testResults <- testResults[[1]]
  
  
  algo.Name <- as.vector(unique(data$confName))
  count_algos <- length(algo.Name)
  
  connection_matrix <- matrix(rep(0,count_algos^2),
                        nrow = count_algos, 
                        ncol = count_algos)
  diag(connection_matrix) <- rep(0,count_algos)
  rownames(connection_matrix) <- algo.Name
  colnames(connection_matrix) <- algo.Name
  
  ############################################################################## 
  ## design_matrix Funktion muss noch komplett abgeaendert und angepasst werden

  par(mfrow = c(h,v))
  for(i in 1:length(testResults)){
    
    connections <- design_matrix(testResults[[i]], 
                                 connectMatrix = connection_matrix)
    sorted <- sort(rankMatrix[i,], index = TRUE)$ix
    connections <- connections[sorted,]
    
    arrows <- design_edges(connections)
    g <- make_empty_graph() + vertices(algo.Name)
    g <- g + edges(arrows)
    
    coords <- layout_(g,plotMethod) 
    
    N <- gsize(g)
    # edge_attr(g) <- list(color = rainbow(N)) # Farben noch anpassen :P
    edge_attr(g) <- list(color = rep("black",N))
    k <- length(algo.Name)
    radius <- max(nchar(algo.Name)) * 4
    vertex_attr(g) <- list(name = algo.Name,
                           color = rep("white",k),
                           size = rep(radius,k))
    
    plot(g, rescale = FALSE,
         layout = coords,
         edge.arrow.size = 0.2,
         vertex.label.cex = 0.9,
         vertex.label.color = "black")
  }
  par(mfrow = c(1,1))
}
