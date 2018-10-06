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
                                  "ImpArc", "Wedge"),
                    clusterFunction = "clusterFunctionHclust")
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

  # Lade ausgewaehlte clusterFunction  (bisjetzt nur Hclust)
  source(paste0("RAnalyse/",clusterFunction,".RData"))

  clusterResult <- clusterFunctionHclust(data = newdata,
                          perfName = perfName,
                          .expID = .expID, 
                          distMethod = "euclidean", 
                          clusterMethod = "complete")
  return(clusterResult)
}
clusterResult <- analyse()

# Weise den jeweiligen in clusterResult vorhandenen ergebenen Clustern
# die zugehoerigen Zeilen aus den genutzten Daten zu
source("RAnalyse/designSections.RData")
sections <- designSections(data = mainHiera,
                           clusterResult = clusterResult)
##

# Wie sind die Einstellungen in den einzelnen Clustern:
# s <- sections[[1]]

# apply(sections[[5]][,expParName],2,table)
##
source("RAnalyse/pairwiseTests.RData")

perfName = "ydist"
expParName = c("a","b","cc","d")
confName = "confName"
algo.Name = c("Arc", "Stan", "Ico", "IcoCorrected", "Imp", 
              "ImpArc", "Wedge") 

testResults <- pairwiseTests(sections,
                             perfName,
                             confName,
                             algo.Name)

source("RAnalyse/designGraph.RData")
source("RAnalyse/design_matrix.RData")
source("RAnalyse/design_edges.RData")
source("RAnalyse/searchForTransitivity.RData")
source("RAnalyse/useTransitivity.RData")
# pdf("GerichteteGraphen.pdf")
designGraph(testResults,
            data = mainHiera,
            plotMethod = in_circle(), 
            h = 2, 
            v = 3,# diese maassen finde ich bis jetzt am besten. Muss noch automatisiert werden
            design_matrix,
            design_edges,
            useTransitivity,
            searchForTransitivity) 
# dev.off()
################################################################################

# To do:
# -Transitivitaet ausnutzen
# - unterschiedliche Pfeilarten (fuer versch Sig niveaus)
# -Funktionseigenschaften der Clustermittelpunkte
# -Funktionen dokumentieren
# -Packet: Cluster anschauen
# -automatische Clustergeroessenbestimmung (vlt mit BIC...)



##########


