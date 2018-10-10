# Dokumentation:
# ...
####
memory.limit()
memory.limit(size = 12000)

setwd("C:/Users/Nils Jannik/Desktop/Bachelorarbeit")
load("Datensaetze/mainHiera.RData") 

library(igraph)
library(PMCMR)
# Funktion - Bereitet die Daten fuer die Clusteranalyse vor und
#           fuehrt diese dann durch
# Eingabe:
# - data: Der gegebene Benchmarkdatensatz (data.frame)
# - perfName: Welcher Parameter soll als Performanzindikator genutzt werden
#             (character)
# - expParName: Welche Parameter koennen eingestellt werden
#              (Vektor con Charactern)
# - confName: Der Name der Spalte in dem die Algorithmen stehen (Character)
# - algo.Name: Zu welchen Algorithmen sind in data Daten vorhanden
#              (Vektor von Charactern) ##confName und algo.Name sollte man wohl
                                       ## noch spaeter zusammenfassen...mal sehen

# - clusterFunction: Welche Clusterefunktion soll benutzt werden (Function)
# Rueckgabe: Eine Liste mit Listenobjekten in denen die jeweilige Indizierung zu
#          den in den jeweiligen Clustern vorhandenen Daten ##(noch zu schwammig...)
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
  newdata <- data.frame(data, .expID = .expID) 
  
  # Selektiere die Spalte mit dem Performanzindikator herraus und fuege die
  # zugehoerige expID hinzu
  N <- length(unique(.expID))
  data <- data.frame(subset(newdata, select = perfName), 
                     rep(1 : N, each = table(.expID)[1]))
  colnames(data) <- c("ydist",".expID")
  
  # Lade ausgewaehlte clusterFunction  (bisjetzt nur clusterFunctionHclust)
  source(paste0("RAnalyse/",clusterFunction,".RData"))

  clusterResult <- clusterFunction(data = data,
                          perfName = perfName,
                          .expID = .expID, 
                          distMethod = "euclidean", 
                          clusterMethod = "complete")
  return(clusterResult)
}
clusterResult <- analyse()
#
# Funktion zur Zuweisung des ClusterResult zu den Daten
source("RAnalyse/designSections.RData")
# Funktionen fuer die Tests:
source("RAnalyse/pairwiseTests.RData")
# Funktionen fuer die grafische Darstellung:
source("RAnalyse/designGraph.RData")
source("RAnalyse/design_matrix.RData")
source("RAnalyse/design_edges.RData")
source("RAnalyse/searchForTransitivity.RData")
source("RAnalyse/useTransitivity.RData")
source("RAnalyse/transform_connections.RData")


# Weise den jeweiligen in clusterResult vorhandenen ergebenen Clustern
# die zugehoerigen Zeilen aus den genutzten Daten zu
sections <- designSections(data = mainHiera,
                           clusterResult = clusterResult)
##
expParName <- c("a","b","cc","d")
# Wie sind die Einstellungen in den einzelnen Clustern (Clustermittelpunkte):
clustmittel <- round(sapply(1:length(sections),
                            function(i) colMeans(sections[[i]][,expParName])),
                     digits = 3)
# Fuehre paarweise Teste zwischen den Performanzen der einzelnen Algorithmen
# durch
testResults <- pairwiseTests(sections,
                             perfName = "ydist",
                             confName = "confName",
                             algo.Name =  c("Arc", "Stan", "Ico", "IcoCorrected", 
                                            "Imp","ImpArc", "Wedge") )


# pdf("GerichteteGraphen.pdf")
designGraph(testResults,
            data = mainHiera, 
            confName = confName,
            h = 2, 
            v = 3,# diese Maassen finde ich bis jetzt am besten. Muss noch 
                  # automatisiert werden
            design_matrix,
            design_edges,
            useTransitivity,
            searchForTransitivity,
            transform_connections,
            shape = "circle",# oder square / sphere usw fuer Form der vertexes
            edge.width = 1,
            testNiveaus = c(1e-10, 1e-5, 1e-2, 0.5),
            clustmittel) 
# dev.off()
################################################################################

# To do:
# -Funktionen dokumentieren
# -Paket: Cluster anschauen
# -automatische Clustergeroessenbestimmung (vlt mit BIC...)



##########


