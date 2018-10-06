
searchForTransitivity <- function(result = c(),
                                  cmatrix, 
                                  cnames, 
                                  start, 
                                  ziel,
                                  first = TRUE){
  
  if(start == ziel) { return( TRUE )}
  goTo <- cnames[cmatrix[start,] == 1]
  if( first ){
    goTo <- goTo[goTo != ziel]
    first <- FALSE
  }
  
  if( 
    (sum(cmatrix[start,]) > 0 && first == FALSE) ||
    (sum(cmatrix[start,]) > 1 && first == TRUE)
  ){
    
    for(i in goTo){
      r <- searchForTransitivity(result, cmatrix, cnames, i, ziel, first = FALSE)
      result <- c(result, r)
    }
  }
  if(sum(result) > 0) { 
    return(TRUE)
  } else { 
    return(FALSE)
  }
}