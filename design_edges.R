design_edges <- function(c_matrix){
  con_string <- NULL
  nr <- nrow(c_matrix)
  rnames <- rownames(c_matrix)
  for(i in 1 : nr){
    for(j in 1 : nr){
      if(c_matrix[i,j] == 1){
        con_string <- c(con_string, rnames[i], rnames[j] )
      } 
    }
  }
  return(con_string)
}