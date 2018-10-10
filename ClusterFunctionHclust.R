# Funktion - Implementierung eines Hierachisches Clusterverfahren
# Eingabe: 
# - data: Der gegebene Benchmarkdatensatz mit zusaetzlich eingefuergen
#         Experimenten ID's (siehe .expID)
# - perfName: Welcher Parameter soll als Performanzindikator genutzt werden
#             (character)
# - .expID: Jede unterschiedliche Einstellung bekommt eine einzelne 
#           Experiment ID
# - distMethod: Welche distanzmethode wird die Abstaende der Daten in R genutzt
# - clusterMethod: Welche hierarchische Clustermethode soll genutzt werden
# Rueckgabe: 
# Eine Liste der Laenge der optimalen Clustergroesse in der in jedem Element 
# alle dieser Clustergruppe zugehoerigen Elemente (in Form ihrer Indizes) stehen.
clusterFunctionHclust <- function(data, 
                             perfName, 
                             .expID, 
                             distMethod = "euclidean", 
                             clusterMethod = "complete"){
  
  # Selektiere die Spalte mit dem Performanzindikator herraus und fuege die
  # zugehoerige expID hinzu
  N <- length(unique(.expID))
  data <- data.frame(subset(data, select = perfName), 
                     rep(1 : N, each = table(.expID)[1]))
  colnames(data) <- c("ydist",".expID")
  
  # hierachisches Clustern der Performancewerte.
  h.cluster <- hclust(dist(data, method = distMethod), 
                      method = clusterMethod)  
  
  # Funktion - Vollzieht die in hclust vollzogenen mergeoperationen nach und 
  #            findet so die jeweiligen Gruppen bei einer gegebenen optimalen
  #            Clustergroesse
  # Eingabe: 
  # - hc: Ein Objekt der Klasse hclust (Rueckgabe der hclust - Funktion)
  # - clustersize: optimale Clustergroesse (Integer)
  # Rueckgabe:
  # Eine Liste der Laenge clustercize in der in jedem Element alle dieser 
  # Clustergruppe zugehoerigen Elemente (in Form ihrer Indizes) stehen.
  mergefunction <- function(hc, clustersize){ # braucht aktuell noch ewig
    # Filter das mergeobject aus dem h.cluster Objekt herraus
    mo <- hc$merge 
    # Finde herraus wie viele Objekte geclustert wurden.
    N <- max(abs(mo))
    # Erstelle Layersobjekt zum nachvollziehen des Clusterings
    Layers <- list()
    
    # Anhand einer sogenannten MergeID wird das entsprechende gemergde Objekt 
    # innerhalb einer entsprechenden Liste von Objekten gefunden
    searchPosList <- function(object = Layers, MergeID){
      for(i in 1:length(object)){
          if(!is.null(object[[i]])){
            if(object[[i]][length(object[[i]])] == MergeID){
              return(i)
            }
          }
      }
      return(NA)
    }    

    # Es muessen N - optimale Clustergroesse Mergeopberationen nachvollzogen
    # werden
    for(i in 1 : (N - clustersize)){
      ## kann noch spaeter entfernt werden. Ist im Grunde nur um den Fortschritt
      # bei dem Funktionsdurchlauf zu beobachten
      if(i %% 500 == 0){
        print(i)
        # print(c(object1,object2))
      }
      ##
      # Definiere die in diesem Durchlauf beobachteten Objekte
      object1 <- mo[i,1]
      object2 <- mo[i,2]
      
      # Wenn beide Objekte kleiner 0 sind, so wurden beide Objekte noch nicht
      # zusammen gemerged. Daher entsprechen deren Absolutwerte deren "wahren"
      # Werte
      if(object1 < 0 && object2 < 0){
        # Fuege in Layers an der Stelle von abs(object1) die beiden Objekte ein
        # und weise ihnen zuzueglich noch die MergeID i zu.
        object1 <- abs(object1)
        Layers[[object1]] <- c(object1, abs(object2), i)
      }
      # Wenn nur Object1 kleiner 0 ist, so wurde dieses noch nie gemerged.
      # Object2 hingegen besteht schon aus mehreren Objekten und wurd bereits
      # gemerged
      else if(object1 < 0 && object2 > 0){
      # abs(object1) entspricht auch wieder dem wahren "Wert" von Objekt1
        object1 <- abs(object1)
      # Finde herraus an welcher Stelle die zu Objekt2 zugehoerigen Werte stehen.
        posObject2 <- searchPosList(Layers, MergeID = object2)
        # Fuege Object1 mit MergeID hinter Object2 ein
        nl2 <- length(Layers[[posObject2]])
        Layers[[posObject2]][nl2:(nl2 + 1)] <- c(object1, i)
        
      }
      # Sind Object1 und Object2 groesser als 0, so wurden beide bereits 
      # gemerged.
      else if(object1 > 0 && object2 > 0){
        # Finde herraus in welcher Zeile sich die Werte zu Object1 und Object2
        # im einzelnen befinden.
        posObject1 <- searchPosList(Layers, MergeID = object1)
        posObject2 <- searchPosList(Layers, MergeID = object2)
        nl1 <- length(Layers[[posObject1]]) - 1
        nl2 <- length(Layers[[posObject2]]) - 1
        # Fuege die Werte von Object2 hinter den Werten von Object1 ein und
        # setze die zugehoerige MergeID
        Layers[[posObject1]][1:nl2 + nl1] <- Layers[[posObject2]][1:nl2]
        Layers[[posObject1]][length(Layers[[posObject1]]) + 1] <- i
        # Setze die Werte an der Stelle von Objekt2 auf 0
        Layers[[posObject2]] <- 0
        }
      
    }
    # Lese die der vorher bestimmten Clustergroesse entsprechenden Anzahl an
    # Objecten aus dem Layersobjekt herraus und gebe dieses zurueck
    lLayers <- length(Layers)
    # Layers[[lLayers]] <- NULL
    result <- list()
    ctr <- 1
    for(i in 1:(lLayers - 1)){
      if(!is.null(Layers[[i]]) && Layers[[i]] != 0){
        result[[ctr]] <- Layers[[i]]
        ctr <- ctr + 1
      }
    }
    ##
    return(result)
  }
  
  clusterResult <- mergefunction(h.cluster, clustersize = 5 ) 
  ## Clustersize muss noch automatisch bestimmt werden 
  ########
  return(clusterResult)
  
}

