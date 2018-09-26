
designSections <- function(data,
                           clusterResult){
  clustersize <- length(clusterResult)
  sections <- list()
  for(i in 1:clustersize){
    chosenID <- sort(clusterResult[[i]])
    sections[[i]] <- data[chosenID,]
  }
  return(sections)
}
