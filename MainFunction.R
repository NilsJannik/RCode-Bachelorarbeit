# Dokumentation:
https://github.com/NilsJannik/RCode-Bachelorarbeit.git
# ...
####
memory.limit()
memory.limit(size=12000)

#setwd("P:/Bachelorarbeit")
load("Datensaetze/mainHiera.RData") 

library(igraph)
library(PMCMR)
analyse <- function(data = mainHiera,
                    perfName = "ydist",
                    expParName = c("a","b","cc","d"),
                    algo.Name = c("Arc", "Stan", "Ico", "IcoCorrected", "Imp", 
                                  "ImpArc", "Wedge") )
{
  if("jobID"%in% colnames(data)){
    data <- data[,colnames(data) != "jobID"]
  }
  
  # Design JobID's
  jobID <- factor(apply(mainHiera[,expParName], 1, paste,collapse = "_"))
  Nlevel <- length(levels(jobID))
  jobID <- sort(as.vector(jobID),index = TRUE)
  data <- data[jobID$ix,]
  jobID <- jobID$x
  newdata <- data.frame(data, jobID = jobID) # Neue Variable im Datensatz .expID statt jobID

  source("RAnalyse/clusterFunction1.RData")
  h.clust <- clusterFunction1(data = newdata,
                          perfName = perfName,
                          jobID = jobID)

  source("RAnalyse/mergefunction.RData")
  clusterResult <- mergefunction(h.clust,5) # mergefunction noch optimieren!
                                                  # automatische Clustergroessenbestimmung einfuegen

  return(clusterResult)
}
clusterResult <- analyse()

data <- mainHiera
source("RAnalyse/designSections.RData")
sections <- designSections(data,clusterResult)

source("RAnalyse/pairwiseTests.RData")
testResults <- pairwiseTests(sections)


source("RAnalyse/designGraph.RData")
source("RAnalyse/design_matrix.RData")
source("RAnalyse/design_edges.RData")
designGraph(testResults,
            data,
            plotMethod = in_circle(), 
            h = 2, 
            v = 3,# diese maassen finde ich bis jetzt am besten. Muss noch automatisiert werden
            design_matrix,
            design_edges) 

################################################################################

