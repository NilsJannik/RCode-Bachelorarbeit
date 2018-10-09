transform_connections <- function(connections){
  if(length(connections) == 1){
    return(connections)
  }
  actual_edges <- connections[[1]]
  for(i in 2:length(connections)){
    connections[[i]][actual_edges == connections[[i]]] <- 0
    actual_edges[actual_edges != connections[[i]]] <- 1
  }
  return(connections)
}
