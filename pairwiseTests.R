# Funktion - 
# Eingabe:
# - sectionObjects: Eine Liste bei der in jedem Element der zu dieser Gruppe 
#                   zugehoerige Teil der Daten in Form eines data.frame stehen
# - perfName: Welcher Parameter soll als Performanzindikator genutzt werden
#             (character)
# - confName: Der Name der Spalte in dem die Algorithmen stehen (Character)
# - algo.Name: Zu welchen Algorithmen sind in data Daten vorhanden
#              (Vektor von Charactern)
# Rueckgabe:
# Eine Liste der Laenge 2:
# - results: Eine Liste mit den geweiligen Matrizen zu den jeweiligen Cluster-
#            gruppen, in denen die jeweiligen p-Werte der einzelnen paarweisen
#            Tests stehen
# - rankMatrix: Eine Matrix in dem in Zeile i und Spalte j der Durchschnittsrang
#               der Performance des j-ten Algorithmus in der i-ten Clustergruppe
#               ist
pairwiseTests <- function(sectionObjects = sections, 
                          perfName, 
                          confName, 
                          algo.Name){
  # Clustergroesse nachschauen:
  N <- length(sectionObjects)
  results <- c(rep(list(NULL), N))
 
  # paarweise Tests mit dem nemenyi Test
  for(i in 1 : N){
    results[[i]] <- posthoc.kruskal.nemenyi.test(
      x = sectionObjects[[i]]$ydist, 
      g = sectionObjects[[i]]$confName, 
      dist = "Chisquare")
  }
  
  # Bestimme in den N Cluster die Durchschnittsraenge ueber alle Werte des
  # Performanzindikator
  rankMatrix <- matrix(ncol = length(algo.Name), nrow = length(sectionObjects))
  colnames(rankMatrix) <- algo.Name
  for(i in 1:length(sectionObjects)){
    CN <- colnames(sectionObjects[[i]])
    
    performance <- sectionObjects[[i]][ , CN == perfName]
    algos <- sectionObjects[[i]][, CN == confName]
    
    rankData <- data.frame(algos, performance, rank(performance))
    for(j in 1 : length(algo.Name)){
      rankMatrix[i,j] <- mean(rankData[algos == algo.Name[j],ncol(rankData)])
    }
  }  
  return(list(results, rankMatrix))
}