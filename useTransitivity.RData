useTransitivity <- function(cmatrix){
  cnames <- colnames(cmatrix)
  rnames <- rownames(cmatrix)
  nc <- length(cnames)
  nr <- length(rnames)
  for(i in 1:(nr)){
    for(j in 1:nc){
      if(cmatrix[i,j] == 1){
        transi <- searchForTransitivity(
          result = c(),
          cmatrix,
          cnames,
          rnames[i],
          cnames[j])
        if(transi){
          cmatrix[i,j] <- 0
        }
      }
    }
  }
  return(cmatrix)
}
