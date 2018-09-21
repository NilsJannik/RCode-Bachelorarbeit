# Dokumentation:
# ...
####
memory.limit()
memory.limit(size=12000)

setwd("C:/Users/Nils Jannik/Desktop/Bachelorarbeit")

load("Datensaetze/mainHiera.RData") 

library(igraph)
library(PMCMR)
analyse <- function(data = mainHiera,
                    perfName = "ydist",
                    expParName = c("a","b","cc","d"),
                    confName = "confName",
                    algo.Name = c("Arc", "Stan", "Ico", "IcoCorrected", "Imp", 
                                  "ImpArc", "Wedge") )
{
  if("jobID"%in% colnames(data)){
    data <- data[,colnames(data) != "jobID"]
  }
  
  # Design .expID's
  .expID <- factor(apply(mainHiera[,expParName], 1, paste,collapse = "_"))
  Nlevel <- length(levels(.expID))
  .expID <- sort(as.vector(.expID),index = TRUE)
  data <- data[.expID$ix,]
  .expID <- .expID$x
  newdata <- data.frame(data, .expID = .expID) # Neue Variable im Datensatz .expID statt jobID

  source("RAnalyse/clusterFunctionHClust.RData")
  clusterResult <- clusterFunctionHclust(data = newdata,
                          perfName = perfName,
                          .expID = .expID)
# 
#   source("RAnalyse/mergefunction.RData")
#   clusterResult <- mergefunction(h.clust,5) # mergefunction noch optimieren!
#                                                   # automatische Clustergroessenbestimmung einfuegen

  return(clusterResult)
}
clusterResult <- analyse()

data <- mainHiera
source("RAnalyse/designSections.RData")
sections <- designSections(data,clusterResult)

source("RAnalyse/pairwiseTests.RData")

testResults <- pairwiseTests(sections,
                             perfName,
                             confName,
                             algo.Name)

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

# An Analysis of Benchmark datasets based on selectable cluster methods 
# combined with a graphical Illustration by directed graphs.
# 
# Eine Analyse von Benchmark Datensätzen basierend auf wählbahren 
# Clustermethoden in Kombination mit gerichteten Graphen.















