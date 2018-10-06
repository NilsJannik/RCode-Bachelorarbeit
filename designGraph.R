
designGraph <- function(testResults, 
                        data,
                        confName = confName,
                        h = 3,# Maaße muss automatisiert bestimmt werden...
                        v = 2,
                        design_matrix,
                        design_edges,
                        useTransitivity,
                        searchForTransitivity,
                        shape = "circle",
                        edge.width = 1){
  
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
    connections <- design_matrix(testResults = testResults[[i]], 
                                 connectMatrix = connection_matrix,
                                 order = sorted$ix,
                                 testNiveau = 0.05)
    # Transitivitaet ausnutzen und entsprechende Verbindungen entfernen:
    connections <-  useTransitivity(connections)
    
    ord_index <- sorted$ix
    ord_ranks <- sorted$x 
    # Entsprechend der connections-Matrix werden die edges = Pfeile erstellt
    arrows <- design_edges(connections)
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
    vertex_names <- paste(algo.Name,"\n", round(rankMatrix[i,], digits = 0))
    
    vertex_attr(graphobject) <- list(
                            name = vertex_names,
                           color = rep("white", count_algos),
                            size = rep(radius, count_algos),
                           shape = rep(shape, count_algos))
    vertex_attr(graphobject)$color[ord_index[1]] <- rgb(255 ,83 ,40 , maxColorValue = 256)
    linetypes <- 1 ### Muss spaeter je nach Niveau festgelegt werden (kann ein
    ### Vektor mit allen Typen fuer jeden edge einzelnd sein)
    plot(graphobject, 
         rescale = FALSE,
         layout = coords,
         edge.arrow.size = 0.2,
         vertex.label.cex = 0.9,
         vertex.label.color = "black",
         edge.width = edge.width,
         edge.lty = linetypes)
  }
  par(mfrow = c(1,1))
}
