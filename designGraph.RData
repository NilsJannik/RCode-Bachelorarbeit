designGraph <- function(testResults, 
                        data,
                        confName = confName,
                        h = 3,# Maaße muss automatisiert bestimmt werden...
                        v = 2,
                        design_matrix,
                        design_edges,
                        useTransitivity,
                        searchForTransitivity,
                        transform_connections,
                        shape = "circle",
                        edge.width = 1,
                        testNiveaus = c(1e-10,1e-5,1e-2,5e-1),
                        clustmittel){
  
  rankMatrix <- testResults[[2]]
  testResults <- testResults[[1]]
  
  # Kreiere connection_matrix fuer die Beziehungen zwischen den Algorithmen: 
  algo.Name <- as.vector(unique(data$confName))
  count_algos <- length(algo.Name)
  
  connection_matrix <- matrix(rep(0, count_algos^2),
                              nrow = count_algos, 
                              ncol = count_algos)
  diag(connection_matrix) <- rep(0,count_algos)
  rownames(connection_matrix) <- algo.Name
  colnames(connection_matrix) <- algo.Name
  
  # Erstelle passend zu jeder Testmatrix ein gerichteter Graph:
  par(mfrow = c(h,v))
  for(i in 1:length(testResults)){
    # Erstelle passend zu den Testresultaten zum aktuellen Niveau die
    # connectionsmatrix:
    sorted <- sort(rankMatrix[i,], index = TRUE) ## Algo mit bester Performance in 1. Zeile usw
    ord_index <- sorted$ix
    ord_ranks <- sorted$x 
    
    connections <- list()
    testNiveaus <- sort(testNiveaus) # Falls die Niveaus nicht geordnet eingegeben werden
    for(j in 1:length(testNiveaus)){
      connections[[j]] <- design_matrix(testResults = testResults[[i]], 
                                        connectMatrix = connection_matrix,
                                        order = sorted$ix,
                                        testNiveau = testNiveaus[j])
    }
    correct_connect <- transform_connections(connections)
    # Transitivitaet ausnutzen und entsprechende Verbindungen entfernen:
    lty_type <- c()
    arrows <- c()
    nNiveaus <- length(testNiveaus)
    for(j in 1:nNiveaus){
      correct_connect[[j]] <-  useTransitivity(correct_connect[[j]])
      lty_type <- c(lty_type, rep(j, sum(correct_connect[[j]])))
      ## Wenn viele Niveaus gewaehlt werden
      ## funktioniert das hier nicht
      
      # Entsprechend der connections-Matrix werden die edges = Pfeile erstellt
      arrows <- c(arrows, design_edges(correct_connect[[j]]))
    }
    
    
    
    # Erstelle das Graphenobjekt mit im Kreis der Rangordnung nach geordnete
    # Algorithmen.
    graphobject <- make_empty_graph() + vertices(algo.Name) + edges(arrows)
    coords <- layout_in_circle(graphobject,
                               order = ord_index)
    # Einstellungsparameter der edges:
    k <- gsize(graphobject)
    edge_attr(graphobject) <- list(color = rep("black", k) )
    ############################################################################
    # Einstellungsparameter der vertexes = "Kreise"
    radius <- max(nchar(algo.Name)) * 4
    vertex_names <- paste(colnames(rankMatrix),"\n", round(rankMatrix[i,], digits = 0))
    
    vertex_attr(graphobject) <- list(
      name = vertex_names,
      color = rep("white", count_algos),
      size = rep(radius, count_algos),
      shape = rep(shape, count_algos))
    vertex_attr(graphobject)$color[ord_index[1]] <- rgb(255 ,83 ,40 , 
                                                        maxColorValue = 256)

    subtitle <- c("Clustermittel: \n")
    rnames <- rownames(clustmittel)
    nr <- nrow(clustmittel)
    if(nr != 1){
      for(k in 1:(nr - 1)){
        subtitle <- paste(subtitle, rnames[k], "=", clustmittel[k,i],",")    
      }
    }
    subtitle <- paste(subtitle, rnames[nr], "=", clustmittel[nr,i])
    
    plot(graphobject, 
         rescale = FALSE,
         layout = coords,
         edge.arrow.size = 0.2,
         vertex.label.cex = 0.9,
         vertex.label.color = "black",
         edge.width = edge.width,
         edge.lty = lty_type,
         sub = subtitle)
  }
  plot.new()
  legend(title = "Testniveaus",
         "center",
         legend = testNiveaus, 
         lty = 1:nNiveaus,
         col = "black",
         bty = "n")
  par(mfrow = c(1,1))
}
