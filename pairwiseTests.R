pairwiseTests <- function(sectionOjbects){
  N <- length(sectionOjbects)
  testResults <- c(rep(list(NULL), N))
  
  for(i in 1 : N){
    testResults[[i]] <- posthoc.kruskal.nemenyi.test(
      sectionOjbects[[i]]$ydist, 
      sectionOjbects[[i]]$confName, 
      "Chisquare")
  }
  return(testResults)
}